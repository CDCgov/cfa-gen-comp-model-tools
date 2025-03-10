#' Updates parameters due to modifications during model run
#'
#' This helper function updates model parameters as a result of an intervention
#' or other modification
#'
#' @param current_parms_vec vector of current model parameter values
#' @param modifier_matrix value to be multiplied against transition matrix
#' @return vector of modified parameter values
#' @export
#' @examples
#' \dontrun{
#' updated_parms_vec <- update_parameters(current_parms_vec, modifier_matrix)
#' }
##############################################################################
# NOTE: This function is PLANNED TO BE REMOVED from this repo as part of the
#       merge of compModels with SIRmodelbuilder functionality. It may be
#       determined that part of the functionality is useful and, if so, it will
#       be incorporated into other functionality.
##############################################################################
update_parameters <- function(current_parms_vec, modifier_matrix) {
  if (!is.null(modifier_matrix)) {
    names_to_modify <- intersect(
      names(current_parms_vec),
      rownames(modifier_matrix)
    )
    for (name in names_to_modify) {
      current_parms_vec[name] <-
        current_parms_vec[name] * modifier_matrix[name, ]
    }
  }
  return(current_parms_vec)
}
