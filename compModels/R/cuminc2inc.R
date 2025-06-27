#' Convert cumulative incidence to incidence
#'
#' Requires times vector and cumulative incidence vector, finds where cumulative
#' incidence changes, and then outputs when the change occurs and the magnitude
#'
#' @param t numeric vector of times
#' @param cuminc named list/vector vector specifying updatedstate population
#' sizes, same length as t
#' @param keepfirst logical whether first element of cuminc should be included
#' as incident
#' @return numeric vector of incidence if keepfirst and
#' a named list with time and incidence vectors
#' @export
cuminc2inc <- function(t, cuminc, keepfirst = TRUE) {
  if (length(t) != length(cuminc)) {
    stop("Input times and cumulative incidence vectors must be same length.")
  }

  if (keepfirst) {
    cuminc <- c(0, cuminc)
  }

  outobj <- diff(cuminc)

  if (!keepfirst) {
    outobj <- list(t = t[2:length(t)], cuminc = outobj)
  }
  outobj
}
