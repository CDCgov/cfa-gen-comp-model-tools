test_that("time2rate converts both numerics and strings", {
  expect_equal(time2rate(10), .1)
  expect_equal(eval(parse(text = time2rate("10"))), .1)
})
