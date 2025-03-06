test_that("cleanscale fills in nested manner", {
  somerates <- c("", "1", "2*3", "1*2*3", "2*3*1")
  tbl2clean <- dplyr::tibble(
    interactionscale = somerates,
    transitionscale = somerates
  )
  tblcleaned <- cleanscale(tbl2clean)

  expect_equal(
    tblcleaned$interactionscale,
    c("", "", "2*3*", "1*2*3*", "2*3*1*")
  )
})
