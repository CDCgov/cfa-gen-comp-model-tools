test_that("matchandpaste_basestate replicates or throws error", {
  desiredlength <- 3
  vecrep <- c("repthis")
  vecrep2 <- c("repthis", "andthis")
  listrep <- list("repthis")
  listrep2 <- list(c("repthis", "andthis"))
  listerror <- list("make", "error")
  expect_equal(
    length(matchandpaste_basestate(vecrep, desiredlength)),
    desiredlength
  )
  expect_equal(
    length(matchandpaste_basestate(vecrep2, desiredlength)),
    desiredlength
  )
  expect_equal(
    length(matchandpaste_basestate(listrep, desiredlength)),
    desiredlength
  )
  expect_equal(
    length(matchandpaste_basestate(listrep2, desiredlength)),
    desiredlength
  )
  expect_error(matchandpaste_basestate(listerror, desiredlength))
})
