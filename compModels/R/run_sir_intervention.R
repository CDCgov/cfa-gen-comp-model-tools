#' Runs SIR compartmental model in context of an intervention
#'
#' Runs a basic SIR model using [deSolve::ode()] given parameters,
#' times, and starting conditions and applies an intervention impacting
#' transmission during a specific time range. This references
#' [compModels::sir_deriv_intervention()] which uses explicit frequency
#' dependent transmission.
#'
#' @param init vector of named numeric initial values of s initial count of
#' susceptibles, i initial count of infecteds, and r initial count of recovereds
#' @param time vector (type: double) containing time steps to run the model over
#' @param parms vector with named parameters to include: beta transmission rate,
#' and gamma recovery rate, intervention_start_time time at which
#' intervention will start, intervention_end_time time at which intervention
#' will end, and the intervention_impact value to be multiplied against beta
#' transmission parameter to adjust transmission during the intervention
#' @return data frame with columns for time and each state (compartment)
#' @export
#' @examples
#' \dontrun{
#' run_sir_intervention(
#'   init = c(s = 1e05 - 1, i = 1, r = 0),
#'   time = seq(0.1, 100, by = 0.1),
#'   parms = list(
#'     beta = 0.5, gamma = 0.1,
#'     intervention_start_time = 20,
#'     intervention_end_time = 30,
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
run_sir_intervention <- function(init, time, parms) {
  validate_sir_input(init, time, parms, intervention = TRUE)

  sir_out <- deSolve::ode(
    y = init, times = time,
    func = sir_deriv_intervention,
    parms = parms
  )
  as.data.frame(sir_out)
}
