#' Calculates modified rates of change for in/out flows from compartments
#'
#' Helper function to calculate modified change rates for in and out flows in
#' a generalized compartmental model with intervention
#' [compModels::run_gen_deterministic()].
#'
#' @param time vector of time steps to run the model over
#' @param state holds the current state of the compartments
#' @param parms additional parameters needed such as comp_names (compartment
#' names), trans_matrix (transition rate matrix), intervention_start_time,
#' intervention_end_time, modifier_matrix (values to be multiplied against
#' transition matrix)
#' @return list object with columns for time and each compartment

gen_rates <- function(time, state, parms) {
  comp_names <- parms$comp_names
  trans_matrix <- if (is_intervention_period(
    time, parms$intervention_start_time,
    parms$intervention_end_time
  )) {
    modify_trans_mtx(parms$trans_matrix, parms$modifier_matrix)
  } else {
    parms$trans_matrix
  }

  # Calculate changes based on current transition matrix (original or modified)
  change_rates <- calculate_change_rates(state, comp_names, trans_matrix)
  list(change_rates)
}
