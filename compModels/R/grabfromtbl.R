#' Grabs rows that matches values in (named)list
#'
#' Names of input named list are column names.
#'
#' @param namedlist (named)list of groups to grab. If names are given they
#' correspond to types
#' @param tbl2grab table with that has columns with values matching those in the
#' (named)list. If a named list is input, then some column names match the
#' namedlist names.
#' @return table with rows that contain the named values
#' @family internal
grabfromtbl <- function(namedlist, tbl2grab) {
  if (identical(namedlist, "")) {
    tblout <- tbl2grab
  } else {
    if (is.null(names(namedlist))) {
      # Add stop if value doesn't exist
      tblout <- tbl2grab |>
        dplyr::filter(dplyr::if_any(-.data$basestates, ~ . %in% namedlist))
    } else {
      currcolnames <- names(namedlist)
      curridx <- 0
      andlogic <- rep(TRUE, nrow(tbl2grab))
      orlogic <- rep(FALSE, nrow(tbl2grab))
      for (currvec in namedlist) {
        curridx <- curridx + 1
        currcol <- tbl2grab[[currcolnames[[curridx]]]]
        if (FALSE %in% (currvec %in% currcol)) {
          stop("Check instructions. Value not in named column.")
        }
        currlogic <- currcol %in% currvec
        andlogic <- andlogic & currlogic
        orlogic <- orlogic | currlogic
      }
      tblout <- tbl2grab |> dplyr::filter(orlogic)
    }
  }
  return(tblout)
}
