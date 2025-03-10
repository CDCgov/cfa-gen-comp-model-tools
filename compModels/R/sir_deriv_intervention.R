#' Calculates SIR compartment states in context of an intervention
#'
#' Contains basic SIR model equations given parameters, times, and starting
#' conditions with a calculation of the effective beta given an intervention.
#' The flow from susceptible to infectious is explicit frequency dependent
#' transmission.
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
#'     beta = 0.5, gamma = 0.1,
#'     intervention_start_time = 10,
#'     intervention_end_time = 20,
#'     intervention_impact = 0.3
#'   )
#' )
#' }
##############################################################################
# NOTE: This function is PLANNED TO BE REMOVED from this repo as part of the
#       merge of compModels with SIRmodelbuilder functionality. It may be
#       determined that part of the functionality is useful and, if so, it will
#       be incorporated into other functionality.
##############################################################################
sir_deriv_intervention <- function(time, state, parms) {
  s <- state[[1]]
  i <- state[[2]]
  r <- state[[3]]
  n <- s + i + r

  effective_beta <- if (is_intervention_period(
    time,
    parms[["intervention_start_time"]],
    parms[["intervention_end_time"]]
  )) {
    parms[["beta"]] * parms[["intervention_impact"]]
  } else {
    parms[["beta"]]
  }

  ds <- (-effective_beta * s * i) / n
  di <- (effective_beta * s * i) / n - parms[["gamma"]] * i
  dr <- parms[["gamma"]] * i
  list(c(s = ds, i = di, r = dr), beta = effective_beta)
}
