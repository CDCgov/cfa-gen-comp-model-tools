#' Validate adaptivetau argument values
#'
#' Helper function to validate adaptivetau argument values, used as part of a
#' validation function
#' @param transitions matrix object with transition information between states
#' @return stops with information or indicates checks passed

##############################################################################
# NOTE: This function is PLANNED TO BE MODIFIED / INCORPORATED DIFFERENTLY in
#       this repo as part of the merge of compModels with SIRmodelbuilder
#       functionality. It may be determined that part of the functionality is
#       useful and, if so, it will remain or be incorporated into other
#       functionality.
##############################################################################
check_adaptivetau_args <- function(transitions) {
  if (!is.matrix(transitions)) stop("transitions must be a matrix")

  print("adaptivetau argument value checks passed")
}
