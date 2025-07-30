test_that("Number of equations is correct", {
  base_states <- c("S", "E", "I", "R")
  infector_state <- "I"
  infectee_state <- "S"

  seirmodel <- define_states(base_states) |>
    add_infection(infector_state, infectee_state, "E", "beta") |>
    add_transition("E", "I", "alpha") |>
    add_transition("I", "R", "gamma")

  seircompiled <- compilemodel(seirmodel)
  core_eqns <- odin_core_eqns(seircompiled, base_states)
  expect_equal(length(base_states), length(core_eqns))
})


test_that("Number of transitions into and out states is correct", {
  base_states <- c("S", "E", "I", "R")
  infector_state <- "I"
  infectee_state <- "S"

  seirmodel <- define_states(base_states) |>
    add_infection(infector_state, infectee_state, "E", "beta") |>
    add_transition("E", "I", "alpha") |>
    add_transition("I", "R", "gamma")

  seircompiled <- compilemodel(seirmodel)

  core_eqns <- odin_core_eqns(seircompiled, base_states)
  trans_mat <- seircompiled$modeloutstructions$petermatrix

  state_exits <- c()
  state_enters <- c()
  for (each_row in seq(1, nrow(trans_mat))) {
    row <- trans_mat[each_row, ]
    row_vals <- table(row)
    num_exits <- length(row_vals[names(row_vals) == "-1"])
    num_enters <- length(row_vals[names(row_vals) == "1"])
    state_exits <- c(state_exits, num_exits)
    state_enters <- c(state_enters, num_enters)
  }
  names(state_exits) <- seircompiled$modeloutstructions$updatedstates
  names(state_enters) <- seircompiled$modeloutstructions$updatedstates
  state_exits <- state_exits[base_states]
  state_enters <- state_enters[base_states]

  num_equn_exits <- lengths(regmatches(core_eqns, gregexpr(" - ", core_eqns)))
  num_equn_enters <- lengths(
    regmatches(core_eqns, gregexpr(" \\+ ", core_eqns))
  )
  names(num_equn_exits) <- base_states
  names(num_equn_enters) <- base_states

  expect_equal(state_exits, num_equn_exits)
  expect_equal(state_enters, num_equn_enters)
})

test_that("Group movements are correct", {
  base_states <- c("S", "E", "I", "R")
  infector_state <- "I"
  infectee_state <- "S"

  seirmodel <- define_states(base_states) |>
    add_infection(infector_state, infectee_state, "E", "beta") |>
    add_transition("E", "I", "alpha") |>
    add_transition("I", "R", "gamma")

  seircompiled <- compilemodel(seirmodel)
  e <- odin_core_eqns(seircompiled, base_states)

  save_move_groups <- c()
  for (each_str in e) {
    split_str <- stringr::str_split(each_str, " ")[[1]]

    move_groups <- split_str[grepl("n_", split_str)]
    save_move_groups <- c(save_move_groups, move_groups)
  }
  save_move_groups <- unique(save_move_groups)

  for (each_group in save_move_groups) {
    split_group <- stringr::str_split(each_group, "")[[1]]
    exit_state <- split_group[3]
    enter_state <- split_group[4]
    movement_mod_instructions <- seircompiled$modelinstructions$tblprocesses[
      seircompiled$modelinstructions$tblprocesses$states_down == exit_state &
        seircompiled$modelinstructions$tblprocesses$states_up == enter_state,
    ]
    compiled_exit_state <- movement_mod_instructions$states_down
    compiled_enter_state <- movement_mod_instructions$states_up

    expect_equal(exit_state, compiled_exit_state)
    expect_equal(enter_state, compiled_enter_state)
  }
})
