test_that("split2typecols group accordingly", {
  agegroups <- c("Young", "Old")
  workgroups <- c("Patient", "HCW")
  comblists <- expand.grid(agegroups, workgroups, stringsAsFactors = FALSE)
  names(comblists) <- c("Age", "ID")
  combliststbl <- dplyr::as_tibble(comblists) |>
    dplyr::mutate(basestates = list(c("x", "y", "z")))
  combinethese <- list(c("Age", "ID"))

  currtbl2split <- dplyr::tibble(
    fromstate = "y",
    tostate = "z",
    grouptype = list(c("Age", "ID"))
  )
  expect_equal(
    nrow(split2typecols(
      currtbl2split,
      combinethese, list(combliststbl)
    )),
    nrow(comblists)
  )
})
