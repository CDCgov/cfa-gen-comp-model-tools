test_that("test that output is a ggplot object", {
  modelout <- run_sir(
    init = c(s = 1e05 - 1, i = 1, r = 0),
    time = seq(0.1, 100, by = 0.1),
    parms = c(beta = 0.00001, gamma = 0.1)
  )
  outplot <- plot_determin_model(modelout)
  expect_equal(ggplot2::is.ggplot(outplot), TRUE)
})
