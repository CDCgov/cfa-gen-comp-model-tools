test_that("cuminc2inc converts incidence", {
  t <- c(1, 2, 3, 4)
  cuminc <- c(3, 4, 6, 7)

  outvec <- cuminc2inc(t, cuminc)
  outlist <- cuminc2inc(t, cuminc, keepfirst = FALSE)

  expect_true(is.list(outlist))

  expect_equal(
    length(outlist$t),
    length(t) - 1
  )

  expect_equal(
    diff(c(0, cuminc)),
    outvec
  )
})
