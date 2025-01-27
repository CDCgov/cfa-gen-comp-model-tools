#' Validate Gillespie argument values
#'
#' Helper function to validate Gillespie argument values, used as part of a
#' validation function
#' @param propensity_fns the transition rate equations
#' @param change_matrix matrix that gives transition information between states
#' @return stops with information or indicates checks passed
check_gillespie_args <- function(propensity_fns, change_matrix) {
  if (!is.character(propensity_fns)) stop("propensity_fns must be character")
  if (!is.matrix(change_matrix)) stop("change_matrix must be a matrix")

  print("Gillespie argument value checks passed")
}
