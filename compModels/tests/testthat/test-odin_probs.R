test_that("Probabilities are equal given parameter and state values", {
  base_states <- c("S", "E", "I", "R")
  infector_state <- "I"
  infectee_state <- "S"

  param_vector <- c("beta" = 2, "alpha" = 2, "gamma" = 5)
  # Define state values
  initial_states_vec <- c("S" = 999, "I" = 1, "R" = 0, "E" = 0)

  seirmodel <- define_states(base_states) |>
    add_infection(
      infector_state,
      infectee_state,
      "E",
      names(param_vector[1])
    ) |>
    add_transition("E", "I", names(param_vector[2])) |>
    add_transition("I", "R", names(param_vector[3]))

  seircompiled <- compilemodel(seirmodel)

  out_probs <- odin_probs(seircompiled)
  model_pop <- seircompiled$modelinstructions$tblntotal
  odin_tot <- odin_tot_pop(model_pop)

  # Assign parameter values to variables in the environment
  for (param in names(param_vector)) {
    assign(param, param_vector[param])
  }
  for (each_state in names(initial_states_vec)) {
    assign(each_state, initial_states_vec[each_state])
  }

  eval(parse(text = odin_tot))

  # Evaluate each expression
  percapitarate_values <- sapply(
    seircompiled$modelinstructions$tblprocesses$percapitarate, function(expr) {
      eval(parse(text = expr))
    }
  )
  seircompiled$modelinstructions$tblprocesses["eval_percapitarate"] <- unname(
    percapitarate_values
  )


  split_out_probs <- stringr::str_split_fixed(out_probs, "exp\\(-", 2)
  seircompiled$modelinstructions$tblprocesses["odin_probs"] <- substr(
    split_out_probs[, 2], 1, nchar(split_out_probs[, 2]) - 1
  )

  # Evaluate each expression
  odin_prob_values <- sapply(
    seircompiled$modelinstructions$tblprocesses$odin_probs, function(expr) {
      eval(parse(text = expr))
    }
  )
  seircompiled$modelinstructions$tblprocesses["eval_odin_probs"] <- unname(
    odin_prob_values
  )

  expect_equal(
    seircompiled$modelinstructions$tblprocesses$eval_percapitarate,
    seircompiled$modelinstructions$tblprocesses$eval_odin_probs
  )
})
