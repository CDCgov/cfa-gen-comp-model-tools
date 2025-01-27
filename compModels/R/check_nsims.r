#' Validate number of simulations step value
#'
#' Helper function to validate number of simulations step value, used as part
#' of a validation function
#' @param n_sims number of simulations desired
#' @return stops with information or indicates checks passed
check_nsims <- function(n_sims) {
  if (!is.numeric(n_sims) || length(n_sims) != 1 || n_sims <= 0) {
    stop("The number of time steps must be a single positive numeric value.")
  }

  print("n_sims values checks passed")
}
