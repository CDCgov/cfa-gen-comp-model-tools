test_that("modeloutput2odeinput combines petermatrix and rates", {
  testlist <- list(
    modeloutstructions =
      list(
        petermatrix = matrix(c(1, -1, 0, 2, -3, 0), nrow = 2, ncol = 3),
        processrates = c("r1", "r2", "r3")
      )
  )
  outstr <- modeloutput2odeinput(testlist)
  expect_equal(outstr, c("r1-3*r3", "-r1+2*r2"))
})
