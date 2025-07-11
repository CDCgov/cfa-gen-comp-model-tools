#' Convert named list for input as tibble column
#'
#' Replicates length one vector and ensures n-length
#' vectors are same size as given length.
#'
#' @param namedlist named list to split
#' @param desiredlength output list length
#' @return list of lists for tibble input
#' @family internal
matchlength_namedlist <- function(namedlist, desiredlength) {
  if (length(namedlist) == 0) {
    namedlist <- list(namedlist)
  } else {
    namedlist <- lapply(
      namedlist,
      function(x) {
        matchlength(x, desiredlength)
      }
    )
    namedlist <- splitnamedlist(namedlist)
  }
  return(namedlist)
}
