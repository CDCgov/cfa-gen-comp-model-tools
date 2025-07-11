#' Slices named list of vectors by combining vector
#' elements by index in separate named lists
#'
#' Vectors must be same length, use matchlength.R to coerce.
#'
#'
#' @param namedlist named list of vectors
#' @return list of named lists of 1-element vectors
#' @family internal
splitnamedlist <- function(namedlist) {
  currnames <- names(namedlist)
  maxlength <- max(sapply(namedlist, length))
  outlist <- lapply(
    seq(maxlength),
    function(i) {
      lapply(
        namedlist,
        function(x) {
          x[[i]]
        }
      )
    }
  )
  return(outlist)
}
