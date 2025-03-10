#' Calculates the rates of change for in/out flows from compartments
#'
#' Helper function to calculate change rates for in and out flows in the
#' compartmental model
#'
#' @param state holds the current state of the compartments
#' @param comp_names names of the compartments
#' @param trans_matrix matrix of transition rates between each of the
#' compartments
#' @return calculated change rates
#' @examples
#' \dontrun{
#' change_rates <- calculate_change_rates(
#'   state, comp_names,
#'   current_trans_matrix
#' )
#' }
##############################################################################
# NOTE: This function is PLANNED TO BE REMOVED from this repo as part of the
#       merge of compModels with SIRmodelbuilder functionality. It may be
#       determined that part of the functionality is useful and, if so, it will
#       be incorporated into other functionality.
##############################################################################
calculate_change_rates <- function(state, comp_names, trans_matrix) {
  change_rates <- numeric(length(state))
  for (i in seq_along(comp_names)) {
    inflow <- sum(state * trans_matrix[, i], na.rm = TRUE)
    outflow <- sum(state[i] * trans_matrix[i, ], na.rm = TRUE)
    change_rates[i] <- inflow - outflow
  }
  change_rates
}
