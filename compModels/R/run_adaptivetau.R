#' Wrapper to run adaptivetau functions [adaptivetau::ssa.exact()] or
#' [adaptivetau::ssa.adaptivetau()] from compiled model and formatted inputs
#'
#' @param x0 the starting values for populations in each compartment
#' @param compiledmodel compiled model as a list, output from compilemodel()
#' @param rate_func function to calculate rates, defaults to NULL which uses
#' [compModels::generalized_rates()]
#' @param parameters vector of parameter values
#' @param t number of time steps
#' @param method method to use in adaptivetau, defaults to "exact". Other option
#' is "adaptivetau"
#' @return list of dataframes object of the data part of GillespieSSA object
#' containing the time and states of the simulation. Number of elements should
#' match the number of simulations
#' @export
run_adaptivetau <- function(x0, compiledmodel, rate_func = NULL,
                            parameters, t,
                            method = "exact") {
  model_rates <- compiledmodel$modeloutstructions$processrates # rateeqns
  model_peter <- compiledmodel$modeloutstructions$petermatrix # transitions
  parameters <- parameters

  if (is.null(rate_func)) {
    rate_func <- generalized_rates
  }

  if (method == "exact") {
    sims <-
      adaptivetau::ssa.exact(
        init.values = x0,
        transitions = as.matrix(model_peter),
        rateFunc = rate_func(as.list(model_rates)),
        params = parameters,
        tf = t
      )
  } else if (method == "adaptivetau") {
    sims <-
      adaptivetau::ssa.adaptivetau(
        init.values = x0,
        transitions = as.matrix(model_peter),
        rateFunc = rate_func(as.list(model_rates)),
        params = parameters,
        tf = t
      )
  } else {
    stop("Method not recognized. Please use 'exact' or 'adaptivetau'")
  }
  return(sims)
}
