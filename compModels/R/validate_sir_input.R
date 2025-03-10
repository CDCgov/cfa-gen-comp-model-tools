#' Checks validity of input to SIR model
#'
#' Helper function that runs checks on basic SIR model input parameters, times,
#' and initial conditions
#'
#' @param init vector of initial values of s0 initial count of susceptibles,
#' i0 initial count of infecteds, and r0 initial count of recovereds
#' @param time vector (type: double) containing time steps to run the model over
#' @param parms vector with beta transmission rate, and gamma recovery rate
#' @param intervention logical (default FALSE) to indicate if testing input
#' parameters related to an intervention need to be checked
#' @return stops with information or indicates "Input Checks Passed"
#' @examples
#' \dontrun{
#' validate_sir_input(
#'   init = c(1e05 - 1, 1, 0),
#'   times = seq(0.1, 100, by = 0.1),
#'   parms = c(0.00001, 0.1)
#' )
#' }
##############################################################################
# NOTE: This function is PLANNED TO BE REMOVED from this repo as part of the
#       merge of compModels with SIRmodelbuilder functionality. It may be
#       determined that part of the functionality is useful and, if so, it will
#       be incorporated into other functionality.
##############################################################################
validate_sir_input <- function(init, time, parms, intervention = FALSE) {
  if (!is.numeric(parms[["beta"]])) stop("beta must be numeric")
  if (!is.numeric(parms[["gamma"]])) stop("gamma must be numeric")
  if (!is.numeric(init["s"])) stop("s must be numeric")
  if (!is.numeric(init["i"])) stop("i must be numeric")
  if (!is.numeric(init["r"])) stop("r must be numeric")
  if (parms[["beta"]] < 0) stop("beta should be non-negative")
  if (parms[["gamma"]] < 0) stop("gamma should be non-negative")
  if (init["s"] < 0) stop("s should be non-negative")
  if (init["i"] < 0) stop("i should be non-negative")
  if (init["r"] < 0) stop("r should be non-negative")
  if (length(time) == 0) {
    stop("time vector must have at least one element")
  }
  if (!all(diff(time) > 0)) {
    stop("time vector must be strictly increasing")
  }
  if (any(c(init["s"], init["i"], init["r"]) > .Machine$integer.max)) {
    stop("Initial conditions exceed the maximum integer size.")
  }
  print("Input checks passed")
}
