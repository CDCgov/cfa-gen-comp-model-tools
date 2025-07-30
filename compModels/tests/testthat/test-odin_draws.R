test_that("Number of draws is correct", {
  base_states <- c("S", "E", "I", "R")
  infector_state <- "I"
  infectee_state <- "S"

  seirmodel <- define_states(base_states) |>
    add_infection(infector_state, infectee_state, "E", "beta") |>
    add_transition("E", "I", "alpha") |>
    add_transition("I", "R", "gamma")

  seircompiled <- compilemodel(seirmodel)

  out_draws <- odin_draws(seircompiled, base_states)
  expect_equal(
    NROW(seircompiled$modelinstructions$tblprocesses),
    length(out_draws)
  )
})


test_that("Draws are with matching probability", {
  base_states <- c("S", "E", "I", "R")
  infector_state <- "I"
  infectee_state <- "S"

  seirmodel <- define_states(base_states) |>
    add_infection(infector_state, infectee_state, "E", "beta") |>
    add_transition("E", "I", "alpha") |>
    add_transition("I", "R", "gamma")

  seircompiled <- compilemodel(seirmodel)

  out_draws <- odin_draws(seircompiled, base_states)

  n_match <- stringr::str_remove(
    stringr::str_extract(out_draws, "n_([A-Za-z]+)"), "n_"
  )
  p_match <- stringr::str_remove(
    stringr::str_extract(out_draws, "p_([A-Za-z]+)"), "p_"
  )
  expect_equal(n_match, p_match)
})
