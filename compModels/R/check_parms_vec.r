#' Validate parameter values
#'
#' Helper function to validate time step value, used as part of a validation
#' function
#' @param parms_vec vector of parameter values
#' @return stops with information or indicates checks passed

##############################################################################
# NOTE: This function is PLANNED TO BE MODIFIED / INCORPORATED DIFFERENTLY in
#       this repo as part of the merge of compModels with SIRmodelbuilder
#       functionality. It may be determined that part of the functionality is
#       useful and, if so, it will remain or be incorporated into other
#       functionality.
##############################################################################
check_parms_vec <- function(parms_vec) {
  if (!is.numeric(parms_vec)) {
    stop("Parameter values must be numeric.")
  }

  if (any(parms_vec <= 0)) {
    stop("All parameter values must be positive.")
  }

  print("parameter values checks passed")
}
