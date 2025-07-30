test_that("All states are initialized", {
  test_states <- c("S", "E", "I", "R")
  test_initial_states_vec <- c("S" = 999, "I" = 1, "R" = 0, "E" = 0)
  odin_init_states <- odin_initial_states(
    test_states,
    test_initial_states_vec
  )
  expect_equal(length(test_states), length(odin_init_states))
})

test_that("States are initialized correctly", {
  test_states <- c("S", "E", "I", "R")
  test_initial_states_vec <- c("S" = 999, "I" = 1, "R" = 0, "E" = 0)
  odin_init_states <- odin_initial_states(
    test_states,
    test_initial_states_vec
  )

  odin_init_states <- gsub(" ", "", odin_init_states)
  split_inits <- stringr::str_split_fixed(odin_init_states, "\\(", 2)
  split_inits <- cbind(
    split_inits,
    stringr::str_split_fixed(split_inits[, 2], "\\)", 2)
  )
  split_inits <- cbind(
    split_inits,
    stringr::str_split_fixed(split_inits[, 4], "-", 2)
  )
  resulting_init_states_vec <- setNames(
    as.numeric(split_inits[, 6]), split_inits[, 3]
  )
  resulting_init_states_vec <- resulting_init_states_vec[
    names(test_initial_states_vec)
  ]
  expect_equal(test_initial_states_vec, resulting_init_states_vec)
})
