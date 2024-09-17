#' Calculates SIR compartment states
#'
#' Contains basic SIR model equations given parameters, times, and starting
#' conditions.
#'
#' @param time time step (type: double) the model is currently running on
#' @param state vector of initial values of s0 initial count of susceptibles,
#' i0 initial count of infecteds, and r0 initial count of recovereds
#' @param parms vector with named parameters to include: beta transmission rate,
#' and gamma recovery rate
#' @return list with elements for each state (compartment)
#' @examples
#' \dontrun{
#' sir_deriv(
#'   time = 1,
#'   state = c(s = 1e05 - 1, i = 1, r = 0),
#'   parms = c(beta = 0.00001, gamma = 0.1)
#' )
#' }
sir_deriv <- function(time, state, parms) {
  s <- state[1]
  i <- state[2]
  r <- state[3]
  ds <- -parms["beta"] * s * i
  di <- parms["beta"] * s * i - parms["gamma"] * i
  dr <- parms["gamma"] * i
  list(c(s = ds, i = di, r = dr))
}
