#' Runs SIR compartmental model
#'
#' Runs basic SIR model using [deSolve::ode()] given parameters,
#' times, and starting conditions. Performs basic input validation via
#' [compModels::validate_sir_input]
#'
#' @param init vector of initial values of s0 initial count of susceptibles,
#' i0 initial count of infecteds, and r0 initial count of recovereds
#' @param time vector (type: double) containing time steps to run the model over
#' @param parms vector with named parameters to include: beta transmission rate,
#' and gamma recovery rate
#' @return data frame with columns for time and each state (compartment)
#' @export
#' @examples
#' \dontrun{
#' run_sir(
#'   init = c(s = 1e05 - 1, i = 1, r = 0),
#'   time = seq(0.1, 100, by = 0.1),
#'   parms = c(beta = 0.5, gamma = 0.1)
#' )
#' }
run_sir <- function(init, time, parms) {
  validate_sir_input(init, time, parms)

  sir_out <- deSolve::ode(
    y = init, times = time,
    func = sir_deriv,
    parms = parms
  )
  as.data.frame(sir_out)
}
