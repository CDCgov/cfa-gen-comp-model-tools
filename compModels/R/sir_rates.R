#' Calculates SIR flow rates
#'
#' Runs basic SIR model given parameters, times, and starting
#' conditions and returns the flows (rates of change) between comparments
#' for use in stochastic modeling [compModels::run_sir_stochastic()]
#'
#' @param init_values vector of initial values of s0 initial count of
#' susceptibles, #' i0 initial count of infecteds, and r0 initial count of
#' recovereds
#' @param params vector with beta transmission rate, and gamma recovery rate
#' @param tf vector of time steps to run the model over
#' @return named numeric vector for each state (compartment)
#' @examples
#' \dontrun{
#' sir_rates(
#'   init_values = c(s = 1e05 - 1, i = 1, r = 0),
#'   params = c(beta = 0.00001, gamma = 0.1),
#'   tf = seq(0.1, 100, by = 0.1)
#' )
#' }
##############################################################################
# NOTE: This function is PLANNED TO BE REMOVED from this repo as part of the
#       merge of compModels with SIRmodelbuilder functionality. It may be
#       determined that part of the functionality is useful and, if so, it will
#       be incorporated into other functionality.
##############################################################################
sir_rates <- function(init_values, params, tf) {
  s <- init_values[1]
  i <- init_values[2]
  r <- init_values[3]

  return(c(
    params[["beta"]] * s * i, # rate of loss of susceptibles to infection
    params[["gamma"]] * i # rate of recovery of infecteds
  ))
}
