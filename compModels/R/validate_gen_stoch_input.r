#' Checks validity of input to stochastic compartmental model
#'
#' Helper function that runs checks on generalized compartmental model input
#' parameters, times, and initial conditions
#'
#' @param parms_vec vector of parameter values
#' @param init_vals the starting values for populations in each compartment
#' @param n_timesteps number of time steps
#' @param method character name of stochastic method used, default NULL. Example
#' options include "GillespieSSA" or "adaptivetau"
#' @param propensity_fns provide transition equations as a vector of equations,
#' default NULL
#' @param change_matrix matrix that gives transition information between states,
#' default NULL
#' @param transitions list object with transition information between states,
#' default NULL
#' @param n_sims number of simulations desired
#' @param intervention_start_time time at which intervention will start
#' @param intervention_end_time time at which intervention will end
#' @param modifier single value, matrix of values, or user supplied function to
#' to be multiplied against transition matrix
#' @return stops with information or indicates Input Checks Passed
#' @export
validate_gen_stoch_input <- function(parms_vec, init_vals,
                                     n_timesteps,
                                     method = NULL,
                                     propensity_fns = NULL,
                                     change_matrix = NULL,
                                     transitions = NULL,
                                     n_sims,
                                     intervention_start_time = NULL,
                                     intervention_end_time = NULL,
                                     modifier = NULL) {
  check_parms_vec(parms_vec)
  check_init_vals(init_vals)
  check_time_step(n_timesteps)
  if (method == "GillespieSSA") {
    check_gillespie_args(propensity_fns, change_matrix)
  } else if (method == "adaptivetau") {
    check_adaptivetau_args(transitions)
  }
  check_nsims(n_sims)
  check_intervention_times(intervention_start_time, intervention_end_time)
  check_modifier2(modifier)

  print("All input checks passed")
}
