test_that("namedlist2tibblerow sums a string and a numeric", {
  testlist <- list(A = "5", B = 5, C = c(1, 2, 3), D = c("1", "2", "3"))
  tblout <- namedlist2tibblerow(testlist)
  expect_equal(nrow(tblout), 1)
  expect_equal(
    sapply(tblout, class),
    c(A = "character", B = "numeric", C = "list", D = "list")
  )
})
