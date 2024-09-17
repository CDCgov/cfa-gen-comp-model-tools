#' Calculates SIR compartment states in context of an intervention
#'
#' Contains basic SIR model equations given parameters, times, and starting
#' conditions with a calculation of the effective beta given an intervention.
#'
#' @param time time step (type: double) the model is currently running on
#' @param state vector of initial values of s0 initial count of susceptibles,
#' i0 initial count of infecteds, and r0 initial count of recovereds
#' @param parms vector with named parameters to include beta transmission rate,
#' and gamma recovery rate
#' @return list with elements for each state (compartment) and effective beta
#' @examples
#' \dontrun{
#' sir_deriv_intervention(
#'   times = seq(0.1, 100, by = 0.1),
#'   state = c(s = 1e05 - 1, i = 1, r = 0),
#'   parms = c(
#'     beta = 0.00001, gamma = 0.1,
#'     intervention_start_time = 10,
#'     intervention_end_time = 20,
#'     intervention_impact = 0.3
#'   )
#' )
#' }
sir_deriv_intervention <- function(time, state, parms) {
  s <- state[[1]]
  i <- state[[2]]
  r <- state[[3]]

  effective_beta <- if (is_intervention_period(
    time,
    parms[["intervention_start_time"]],
    parms[["intervention_end_time"]]
  )) {
    parms[["beta"]] * parms[["intervention_impact"]]
  } else {
    parms[["beta"]]
  }

  ds <- -effective_beta * s * i
  di <- effective_beta * s * i - parms[["gamma"]] * i
  dr <- parms[["gamma"]] * i
  list(c(s = ds, i = di, r = dr), beta = effective_beta)
}
