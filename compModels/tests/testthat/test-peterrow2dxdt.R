test_that("peterrow2dxdt gives SIR values 1* and nothing else", {
  peterrow <- matrix(c(1, 0, -2), nrow = 1)
  ratestr <- c("r1", "r2", "r3")
  expect_equal(peterrow2dxdt(peterrow, ratestr), "r1-2*r3")
})
