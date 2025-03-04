test_that("states2petercolumn makes petersen column from input vectors", {
  statenames <- c("Add", "Addalso", "Subtract", "Subtractalso", "Neutral")
  addvec <- c("Add", "Add", "Addalso", "Neutral")
  subtractvec <- c("Subtract", "Subtractalso", "Neutral")
  expect_equal(
    matrix(states2petercolumn(subtractvec, addvec, statenames)),
    matrix(c(2, 1, -1, -1, 0), ncol = 1)
  )
})
