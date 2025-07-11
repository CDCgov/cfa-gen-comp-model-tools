#' Convert rate to time
#'
#' Outputs inverse of input (e.g., 1/x) in same type as input
#'
#'
#' @param inputtimes numeric or character vector
#' @return vector of same type as input with inverse values
#' @family internal
time2rate <- function(inputtimes) {
  # Converts vector into rate. Output is same class as input
  if (is.numeric(inputtimes)) {
    outputrates <- 1 / inputtimes
  } else if (is.character(inputtimes)) {
    outputrates <- paste0("(1/(", inputtimes, "))")
  } else {
    stop("Input time vector to normalize is neither numeric or a character.")
  }
  return(outputrates)
}
