test_that("makenewvariablenamesfromrow replicates or throws error", {
  tbltest <- dplyr::tibble(
    basestates = c("S", "S"),
    name1 = c("A1", "A2"),
    name2 = c("B1", "B2"),
    nameignore = NA,
    name3 = c("C1", "C2"),
    nameignore2 = c("D", NA)
  )
  expect_equal(
    makenewvariablenamesfromrow(tbltest),
    c("S_name1A1_name2B1_name3C1", "S_name1A2_name2B2_name3C2")
  )
  expect_equal(length(makenewvariablenamesfromrow(rbind(tbltest, tbltest))), 4)
})
