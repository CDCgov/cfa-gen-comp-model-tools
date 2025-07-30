test_that("Total population is correctly defined for odin", {
  base_states <- c("S", "E", "I", "R")
  infector_state <- "I"
  infectee_state <- "S"

  seirmodel <- define_states(base_states) |>
    add_infection(infector_state, infectee_state, "E", "beta") |>
    add_transition("E", "I", "alpha") |>
    add_transition("I", "R", "gamma")

  seircompiled <- compilemodel(seirmodel)

  pop_info <- seircompiled$modelinstructions$tblntotal
  tot_pop_result <- odin_tot_pop(pop_info)
  resulting_pop_info <- paste0("Ntotal <- (", gsub(
    " ", "",
    paste0(base_states, collapse = " + ")
  ), ")")

  expect_equal(tot_pop_result, resulting_pop_info)
})
