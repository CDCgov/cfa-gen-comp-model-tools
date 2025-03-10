#' Validate initial values
#'
#' Helper function to validate initial values, used as part of a validation
#'
#' @param init_vals the starting values for populations in each compartment
#' @return stops with information or indicates checks passed

##############################################################################
# NOTE: This function is PLANNED TO BE MODIFIED / INCORPORATED DIFFERENTLY in
#       this repo as part of the merge of compModels with SIRmodelbuilder
#       functionality. It may be determined that part of the functionality is
#       useful and, if so, it will remain or be incorporated into other
#       functionality.
##############################################################################
check_init_vals <- function(init_vals) {
  if (!all(is.numeric(init_vals))) {
    stop("All initial values must be numeric.")
  }

  if (any(init_vals < 0)) {
    stop("All initial values must be non-negative.")
  }

  if (any(init_vals > .Machine$integer.max)) {
    stop("Initial conditions exceed the maximum integer size.")
  }

  print("initial values checks passed")
}
