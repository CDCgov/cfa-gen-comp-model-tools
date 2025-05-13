#' Unlist named list to get immediate names of each element
#'
#' Useful when getting values associated to group names from a list that has
#' both groupnames and typenames
#'
#' @param mixedlist list of named vectors
#' @return named vector with immediate names
mixedunlist <- function(mixedlist) {
  unlistvec <- c()
  for (mlidx in seq_along(mixedlist)) {
    currmixedlist <- mixedlist[mlidx]
    if (length(unlist(currmixedlist)) == 1) {
      unlistvec <- c(unlistvec, unlist(currmixedlist))
    } else {
      unlistvec <- c(unlistvec, currmixedlist[[1]])
    }
  }
  return(unlistvec)
}
