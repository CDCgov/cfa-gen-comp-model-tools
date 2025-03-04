test_that("normalize2probability converts vectors to sum to 1", {
  testnumvec <- c(1, 2, 3)
  testcharvec <- c("1", "2", "3")
  expect_equal(sum(normalize2probability(testnumvec)), 1)
  expect_equal(sum(sapply(
    normalize2probability(testcharvec),
    function(x) {
      eval(parse(text = x))
    }
  )), 1)
})
