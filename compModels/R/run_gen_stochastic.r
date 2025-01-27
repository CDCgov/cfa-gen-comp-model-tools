#' Run generalized stochastic compartmental model
#'
#' Runs basic stochastic compartmental model using [GillespieSSA::ssa()] given
#' a user specified number and names of compartments, transition matrix
#' between compartments (change matrix), disease-related parameters, number of
#' time steps, and initial starting conditions for each compartment (state).
#' No seed is set in the function so results are expected to vary each time
#' the function is run. Parameter values must be a vector, as required by
#' GillespieSSA, and ssa() function returns given compartment names.
#'
#' @param parms_vec vector of parameter values
#' @param propensity_fns the transition rate equations
#' @param init_vals the starting values for populations in each compartment
#' @param n_timesteps number of time steps
#' @param change_matrix matrix that gives transition information between states
#' @param n_sims number of simulations desired
#' @return list of dataframes object of the data part of GillespieSSA object
#' containing the time and states of the simulation. Number of elements should
#' match the number of simulations
#' @export
#' @examples
#' \dontrun{
#' init_vals <- c(s = 990, e = 100, i1 = 10, i2 = 0, r = 0)
#' change_matrix <- matrix(
#'   c(
#'     -1, 0, 0, 0, # S -> E
#'     +1, -1, 0, 0, # E -> I1
#'     0, +1, -1, 0, # I1 -> I2
#'     0, 0, +1, -1, # I2 -> R
#'     0, 0, 0, +1
#'   ),
#'   nrow = length(init_vals),
#'   byrow = TRUE,
#'   dimnames = list(names(init_vals), NULL)
#' )
#' modelout <- run_gen_stochastic(
#'   parms_vec = c(
#'     beta = 0.001, sigma = 0.1,
#'     alpha = 0.05, gamma = 0.1
#'   ),
#'   propensity_fns = c(
#'     "beta * s * i1",
#'     "sigma * e",
#'     "alpha * i1",
#'     "gamma * i2"
#'   ),
#'   init_vals = c(
#'     s = 990, e = 100,
#'     i1 = 10, i2 = 0, r = 0
#'   ),
#'   n_timesteps = 100,
#'   change_matrix = change_matrix,
#'   n_sims = 10
#' )
#' }
run_gen_stochastic <- function(parms_vec, propensity_fns, init_vals,
                               n_timesteps, change_matrix, n_sims) {
  validate_gen_stoch_input(parms_vec, init_vals, n_timesteps,
    method = "GillespieSSA",
    propensity_fns = propensity_fns,
    change_matrix = change_matrix,
    transitions = NULL,
    n_sims,
    intervention_start_time = NULL,
    intervention_end_time = NULL,
    modifier = NULL
  )

  nu <- change_matrix # State Change Matrix
  t <- n_timesteps

  sims <- lapply(1:n_sims, function(i) {
    GillespieSSA::ssa(
      x0 = init_vals,
      a = propensity_fns,
      nu = nu,
      parms = parms_vec,
      tf = t,
      method = GillespieSSA::ssa.d(),
      simName = "General Stochastic Model"
    )$data
  })

  lapply(sims, function(x) data.frame(x)) # output list object of data frames
}
