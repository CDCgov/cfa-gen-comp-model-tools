test_that("define_initialstate assigns according to basestates", {
  basestatex0 <- c("S" = 999, "I" = 1)
  updatedstates <- c("S1", "S2", "I1", "I2", "R1", "R2")
  basestates <- c("S", "S", "I", "I", "R", "R")
  tblupdatedstates <- dplyr::tibble(
    updatedstate = updatedstates,
    basestates = basestates,
    interactionscale = 0,
    transitionscale = 0,
    environment_names = 0
  )
  outlist <- list(
    modeloutstructions = list(updatedstates = updatedstates),
    modelinstructions = list(tblupdatedstates = tblupdatedstates)
  )

  output <- define_initialstate(outlist, namedvector = basestatex0)

  expect_equal(output$X0, c(999, 999, 1, 1, 0, 0))
})
