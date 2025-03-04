test_that("output_initialstate converts table to named list", {
  tblx0 <- dplyr::tibble(updatedstate = c("X", "Y", "Z"), X0 = c(0, 1, 2))
  expect_equal(output_initialstate(tblx0), c(X = 0, Y = 1, Z = 2))
})
