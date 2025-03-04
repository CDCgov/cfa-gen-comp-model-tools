test_that("set_x0_byname sets initial conditions", {
  x0vec <- c(S = 999, I = 1, R = 0)
  tblx0 <- dplyr::tibble(updatedstate = names(x0vec), X0 = NA)
  expect_equal(set_x0_byname(tblx0, x0vec)$X0, unname(x0vec))
})
