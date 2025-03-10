#' Validate subgroups list
#'
#' Helper function to validate subgroups, used as part of a validation
#' function
#' @param subgroups_list list of one or more vectors of subgroup names to be
#' used for stratification
#' @return stops with information or indicates checks passed

##############################################################################
# NOTE: This function is PLANNED TO BE MODIFIED / INCORPORATED DIFFERENTLY in
#       this repo as part of the merge of compModels with SIRmodelbuilder
#       functionality. It may be determined that part of the functionality is
#       useful and, if so, it will remain or be incorporated into other
#       functionality.
##############################################################################
check_subgroups_list <- function(subgroups_list) {
  if (!is.null(subgroups_list)) {
    if (!is.list(subgroups_list)) {
      stop("subgroups_list must be a list.")
    }

    valid_subgroup <-
      any(sapply(subgroups_list, function(x) is.vector(x) && length(x) >= 2))

    if (!valid_subgroup) {
      stop("subgroups_list must contain at least one element with at least
             two values")
    }
  }

  print("subgroups list checks passed")
}
