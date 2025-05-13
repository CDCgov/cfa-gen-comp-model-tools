test_that("groupcombinations groups accordingly", {
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

  typein <- c("type1", "type2")
  check1 <- groupcombinations(typein, tblgroup)
  expect_equal(typein %in% colnames(check1), rep_len(TRUE, length(typein)))
  expectlength <- 1
  for (currtype in typein) {
    expectlength <- expectlength * nrow(tblgroup |> dplyr::filter(.data$grouptype == currtype)) # nolint: line_length_linter.
  }
  expect_equal(nrow(check1), expectlength)

  typein2 <- c("type3")
  check2 <- groupcombinations(typein2, tblgroup)
  expect_equal(typein %in% colnames(check1), rep_len(TRUE, length(typein)))
  expectlength <- 1
  for (currtype in typein2) {
    expectlength <- expectlength * nrow(tblgroup |> dplyr::filter(.data$grouptype == currtype)) # nolint: line_length_linter.
  }
  expect_equal(nrow(check2), expectlength)
})
