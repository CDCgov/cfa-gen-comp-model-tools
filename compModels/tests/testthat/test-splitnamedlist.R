test_that("splitnamedlist gives right size", {
  my_list <- list(name1 = c(1, 2, 3, 4), name2 = c(5, 6, 7, 8))
  my_listsplit <- splitnamedlist(my_list)
  expect_equal(length(my_listsplit), 4)
  expect_equal(sapply(my_listsplit, length), rep_len(2, length(my_list$name1)))
})
