test_that("matchlength_namedlist replicates or throws error", {
  testlist <- list(name1 = c(1, 2), name2 = c(4, 5), name3 = 2)
  desiredlength <- max(sapply(testlist, length))
  matchlist <- rep_len(desiredlength, length(testlist))
  names(matchlist) <- names(testlist)
  testlistout <- matchlength_namedlist(testlist, desiredlength)

  expect_equal(
    length(testlistout),
    desiredlength
  )
  expect_equal(
    sapply(testlistout, length),
    rep_len(length(testlist), desiredlength)
  )
  expect_error(matchlength_namedlist(testlist, 3))

  testlist2 <- list(name1 = c(1, 2))
  matchlength_namedlist(testlist2, length(testlist2$name1))
})
