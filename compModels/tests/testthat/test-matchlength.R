test_that("matchlength replicates or throws error", {
  desiredlength <- 3
  vecrep <- c("repthis")
  vecerror <- c("make", "error")
  expect_equal(length(matchlength(vecrep, desiredlength)), desiredlength)
  expect_error(matchlength(vecerror, desiredlength))
})
