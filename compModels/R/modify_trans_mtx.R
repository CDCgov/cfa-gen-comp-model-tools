#' Modifies values in the transition rate matrix
#'
#' Helper function to modify the transition matrix by applying a modifier to
#' specific user defined compartments
#'
#' @param trans_matrix matrix of transition rates between each of the
#' compartments
#' @param intervention_modifiers value to be multiplied against specific
#' values in transition matrix
#' @return modified transmission matrix
#' @examples
#' \dontrun{
#' current_trans_matrix <- modify_trans_mtx(
#'   trans_matrix,
#'   intervention_modifiers
#' )
#' }
##############################################################################
# NOTE: This function is PLANNED TO BE REMOVED from this repo as part of the
#       merge of compModels with SIRmodelbuilder functionality. It may be
#       determined that part of the functionality is useful and, if so, it will
#       be incorporated into other functionality.
##############################################################################
modify_trans_mtx <- function(trans_matrix, intervention_modifiers) {
  if (!is.null(intervention_modifiers)) {
    # Intervention_modifiers is a matrix with the same dimensions as
    # trans_matrix and contains modification factors for each
    # corresponding transition.
    trans_matrix <- trans_matrix * intervention_modifiers
  }
  trans_matrix
}
