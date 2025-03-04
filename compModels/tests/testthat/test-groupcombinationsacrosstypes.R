test_that("groupcombinationsacrosstypes groups accordingly", {
  combinethesetypes <- c("type1", "type2")
  grouptypes <- c("type1", "type1", "type1", "type2", "type2", "type3")
  groupnames <- c("group1", "group2", "group3", "group1", "group2", "group1")
  interactionscales <- c("1*", "2*", "3*", "4*", "5*", "6*")
  tblgroup <- dplyr::tibble(
    grouptype = grouptypes,
    groupname = groupnames,
    interactionscale = interactionscales,
    transitionscale = "",
    basestates = ""
  )

  check1 <- groupcombinationsacrosstypes(c("type1", "type2"), tblgroup) |>
    dplyr::pull(interactionscale)
  expect_equal(check1, c("1*4*", "2*4*", "3*4*", "1*5*", "2*5*", "3*5*"))

  check2 <- groupcombinationsacrosstypes(c("type3"), tblgroup) |>
    dplyr::pull(interactionscale)
  expect_equal(check2, c("6*"))
})
