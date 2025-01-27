#' Validate time step value
#'
#' Helper function to validate time step value, used as part of a validation
#' function
#' @param n_timesteps number of time steps
#' @return stops with information or indicates checks passed
check_time_step <- function(n_timesteps) {
  if (!is.numeric(
    n_timesteps
  ) ||
    length(n_timesteps) != 1 ||
    n_timesteps <= 0) {
    stop("The number of time steps must be a single positive numeric value.")
  }

  print("times values checks passed")
}
