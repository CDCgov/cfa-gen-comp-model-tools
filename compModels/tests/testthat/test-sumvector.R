test_that("sumvector sums a string and a numeric", {
  expect_equal(sumvector(c(1, 2, 3)), 6)
  expect_equal(sumvector(as.character(c(1, 2, 3))), "(1+2+3)")
})
