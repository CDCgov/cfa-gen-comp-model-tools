#' Validate number of initial values matches number of compartment names
#'
#' Helper function to validate that the number of initial values matches number
#' of compartment names, used as part of a validation
#' function
#' @param init_vals the starting values for populations in each compartment
#' @param comp_names names of the compartments
#' @return stops with information or indicates checks passed

##############################################################################
# NOTE: This function is PLANNED TO BE MODIFIED / INCORPORATED DIFFERENTLY in
#       this repo as part of the merge of compModels with SIRmodelbuilder
#       functionality. It may be determined that part of the functionality is
#       useful and, if so, it will remain or be incorporated into other
#       functionality.
##############################################################################
check_initvals_compnames <- function(init_vals, comp_names) {
  if (length(init_vals) != length(comp_names)) {
    stop("The number of initial values must match the number of compartment
          names.")
  }

  print("number of initial values and compartment names are equal")
}
