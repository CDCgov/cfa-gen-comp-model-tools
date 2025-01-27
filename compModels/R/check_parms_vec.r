#' Validate parameter values
#'
#' Helper function to validate time step value, used as part of a validation
#' function
#' @param parms_vec vector of parameter values
#' @return stops with information or indicates checks passed
check_parms_vec <- function(parms_vec) {
  if (!is.numeric(parms_vec)) {
    stop("Parameter values must be numeric.")
  }

  if (any(parms_vec <= 0)) {
    stop("All parameter values must be positive.")
  }

  print("parameter values checks passed")
}
