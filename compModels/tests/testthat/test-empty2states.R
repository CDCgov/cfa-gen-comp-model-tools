test_that("empty2states replaces \"\" with vector", {
  vec1 <- c("the", "order", "doesnt")
  vec2 <- c("", "matter")

  expect_equal(length(empty2states(vec2, vec1)), 4)
  expect_equal(empty2states(vec1, vec2), vec1)
})
