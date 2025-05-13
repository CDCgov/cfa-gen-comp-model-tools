test_that("grouptypecombinations groups accordingly", {
  combinethesetypes <- c("type1", "type2")
  grouptypes <- c("type1", "type1", "type1", "type2", "type2", "type3")
  groupnames <- c("group1", "group2", "group3", "group1", "group2", "group1")
  interactionscales <- c("1*", "2*", "3*", "4*", "5*", "6*")
  tblgroup <- dplyr::tibble(
    grouptype = grouptypes,
    groupname = groupnames,
    scaleinteractions = interactionscales,
    scaletransitions = interactionscales,
    scalemigrations = interactionscales,
    basestates = ""
  )
  check1 <- grouptypecombinations(
    combinethesetypes,
    tblgroup,
    "grouptype",
    "groupname"
  )
  expect_equal(colnames(check1), combinethesetypes)

  expectlength <- 1
  for (currtype in combinethesetypes) {
    expectlength <- expectlength * nrow(tblgroup |> dplyr::filter(.data$grouptype == currtype)) # nolint: line_length_linter.
  }
  expect_equal(nrow(check1), expectlength)
})
