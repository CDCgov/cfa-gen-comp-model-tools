#' Generates new state names from basestates
#'
#' Pastes basestate names with column names and column
#' values, delimited by _
#'
#' @param tibblerow tibble row to generate state names from
#' Ignore columns with all NA.
#' @return string of updated name
#' @importFrom rlang .data
makenewvariablenamesfromrow <- function(tibblerow) {
  currbasestate <- tibblerow |> dplyr::pull("basestates")
  newname <- make.names(currbasestate)
  # remove columns with only NA
  tibblerow <- tibblerow |>
    dplyr::select(-"basestates") |>
    dplyr::select_if(~ !any(is.na(.)))
  if (ncol(tibblerow) >= 1) {
    for (currcol in colnames(tibblerow)) {
      newname <- paste0(
        newname, "_",
        make.names(currcol),
        make.names(tibblerow |> dplyr::pull(currcol))
      )
    }
  }
  return(newname)
}
