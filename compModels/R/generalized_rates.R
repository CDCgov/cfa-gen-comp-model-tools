#' Calculates rates along model in/out flows from compartments

#' Helper function to calculate modified change rates for in and out flows in
#' a stochastic generalized compartmental model using [adaptivetau::ssa.exact()]
#' @param rate_eqns a list of the transition rate equations between flows,
#'                  listed in the same order as the transitions supplied to the
#'                  model runner function
#' @return the rate(s) at a specific time point
#' @export


##############################################################################
# NOTE: This function is PLANNED TO BE REMOVED from this repo as part of the
#       merge of compModels with SIRmodelbuilder functionality. It may be
#       determined that part of the functionality is useful and, if so, it will
#       be incorporated into other functionality.
##############################################################################
generalized_rates <- function(rate_eqns) {
  function(state, params, t) {
    sapply(rate_eqns, function(eq) {
      eval(parse(text = eq), envir = as.list(c(state, params, t)))
    })
  }
}
