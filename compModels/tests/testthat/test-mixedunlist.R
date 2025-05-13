test_that("mixedunlist gives correct names", {
  mixedlist <- list(group1 = "1", type1 = c(group2 = "2", group3 = "3"))

  expect_equal(
    names(mixedunlist(mixedlist)),
    c("group1", "group2", "group3")
  )
})
