#' Runs generalized stochastic compartmental model with tau leaping
#'
#' Runs basic stochastic SIR model using [adaptivetau::ssa.exact()] given
#' initial conditions (states), transitions, rate function and rate
#' function equations, parameters, number of timesteps, population size, and
#' number of simulations to run.
#' No seed is set in the function so results are expected to vary each time
#' the function is run.
#'
#' @param init_vals the starting values for populations in each compartment
#' @param transitions list object with transition information between states
#' @param rate_func function containing the transition rate factory, e.g.
#'                  [compModels::generalized_rates()] where the user
#'                  can define rate_eqns to feed into the function (see example)
#' @param parms_vec vector of parameter values
#' @param n_timesteps number of time steps
#' @param n population size
#' @param n_sims number of simulations to run
#' @return data frame object of the time and states of the simulation
#' @export
#' @examples
#' \dontrun{
#' init_vals <- c(s = 99999, i = 1, r = 0)
#' transitions <- list(c(s = -1, i = 1), c(i = -1, r = 1))
#' parms_vec <- c(beta = 0.00001, gamma = 0.1, n = 1e05)
#' n_timesteps <- 100
#' n_sims <- 10
#' rate_eqns <- list(
#'   dplyr::expr(beta * s * i), # /n removed for now, will need to explore
#'   dplyr::expr(gamma * i)
#' )
#' out <- run_gen_stochastic_tau(
#'   init_vals, # ssa.exact requires this to be a vector, not a named list
#'   transitions,
#'   generalized_rates(rate_eqns),
#'   parms_vec,
#'   n_timesteps,
#'   parms_vec[["n"]],
#'   n_sims
#' )
#' plot_stoch_model(out, time_var = "time")
#' }
run_gen_stochastic_tau <- function(init_vals, transitions,
                                   rate_func,
                                   parms_vec, n_timesteps,
                                   n, n_sims) {
  # Note: arguments in the order of ssa.exact() arguments

  # Validate input if necessary (you may need to create a new validation fn)
  t <- n_timesteps

  sims <- lapply(1:n_sims, function(i) {
    adaptivetau::ssa.exact(
      init.values = init_vals,
      transitions = transitions,
      rateFunc = rate_func,
      params = parms_vec,
      tf = t
    )
  })

  lapply(sims, function(x) data.frame(x))
}
