#' Validate time step value
#'
#' Helper function to validate time step value, used as part of a validation
#' function
#' @param n_timesteps number of time steps
#' @return stops with information or indicates checks passed

##############################################################################
# NOTE: This function is PLANNED TO BE MODIFIED / INCORPORATED DIFFERENTLY in
#       this repo as part of the merge of compModels with SIRmodelbuilder
#       functionality. It may be determined that part of the functionality is
#       useful and, if so, it will remain or be incorporated into other
#       functionality.
##############################################################################
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
