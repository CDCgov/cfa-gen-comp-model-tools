#' Get instantaneous change to population
#'
#' For a given row of petersen matrix,
#' add/subtract process
#' rates as scaled by value in row. Useful to
#' generate input for deSolve
#'
#' @param currrow Row of petersen matrix with integer values
#' @param currprocessrates character vector of process
#' rates to be added together
#' @return String specifying total instaneous change
#' across the row (ie for a state)
peterrow2dxdt <- function(currrow, currprocessrates) {
  # combine row of petersen matrix and
  # process vec into string interpretable by R
  # and used as input for deSolve
  nzlogic <- currrow != 0
  nzrow <- currrow[nzlogic]
  nzprocess <- currprocessrates[nzlogic]
  nzrowpositive <- nzrow > 0
  rowchar <- as.character(nzrow)
  rowchar <- paste0(rowchar, "*")
  rowchar[nzrowpositive] <- paste0("+", rowchar[nzrowpositive])
  rowcharfinal <- sapply(rowchar, remove1star, USE.NAMES = FALSE)
  dxdtstr <- sub(
    "^\\+", "",
    paste(paste0(rowcharfinal, nzprocess), collapse = "")
  )
  return(dxdtstr)
}
