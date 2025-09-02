test_that("grabfromtbl filters correctly", {
  xa <- c("A", "B", "C")
  x1 <- c(1, 2, 3)
  combvec <- expand.grid(xa, x1)
  names(combvec) <- c("Letters", "Numbers")
  tbl2grab <- dplyr::as_tibble(combvec)
  namedlist <- list(Letters = c("A", "B"), Numbers = 3)
  grablist <- c("A", "B")
  grabbedtbl <- grabfromtbl(namedlist, tbl2grab)
  grabbedtbl_and <- grabfromtbl(namedlist, tbl2grab, andlogic = TRUE)

  grabbedtbl_hardcode <-
    tbl2grab |>
    dplyr::filter((Letters %in% namedlist$Letters) | (Numbers %in% namedlist$Numbers)) # nolint: line_length_linter.
  grabbedtbl_hardcode_and <-
    tbl2grab |>
    dplyr::filter((Letters %in% namedlist$Letters) & (Numbers %in% namedlist$Numbers)) # nolint: line_length_linter.
  expect_equal(
    grabbedtbl,
    grabbedtbl_hardcode
  )

  expect_equal(
    grabbedtbl_and,
    grabbedtbl_hardcode_and
  )
})
