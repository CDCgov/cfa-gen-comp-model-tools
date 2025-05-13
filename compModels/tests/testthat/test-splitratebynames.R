test_that("splitratebynames splits rate right", {
  currrate <- "this*is*a*function*I"
  oldnames <- "I"
  newnames <- "(I_1+I_2+I_3)"
  newrate <- splitratebynames(currrate, oldnames, newnames)
  expect_equal(newrate, "this*is*a*function*(I_1+I_2+I_3)")
})
