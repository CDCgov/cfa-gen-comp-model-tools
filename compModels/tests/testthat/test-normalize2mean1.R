test_that("normalize2mean1 converts vectors to have a mean 1", {
  testnumvec <- c(1, 2, 3)
  testcharvec <- c("1", "2", "3")
  expect_equal(mean(normalize2mean1(testnumvec)), 1)
  expect_equal(mean(sapply(
    normalize2mean1(testcharvec),
    function(x) {
      eval(parse(text = x))
    }
  )), 1)
})
