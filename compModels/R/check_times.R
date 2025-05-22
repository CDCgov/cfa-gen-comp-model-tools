#' Validate time values
#'
#' Helper function to validate time values, used as part of a validation
#' function
#' @param times vector of time steps to run the model over
#' @return stops with information or indicates checks passed

##############################################################################
# NOTE: This function is PLANNED TO BE MODIFIED / INCORPORATED DIFFERENTLY in
#       this repo as part of the merge of compModels with SIRmodelbuilder
#       functionality. It may be determined that part of the functionality is
#       useful and, if so, it will remain or be incorporated into other
#       functionality.
##############################################################################
check_times <- function(times) {
  if (length(times) < 1) {
    stop("The times vector must have at least one element.")
  }

  if (!is.numeric(times) || !all(diff(times) > 0)) {
    stop("The times vector must be numeric and strictly increasing.")
  }

  print("times values checks passed")
}
