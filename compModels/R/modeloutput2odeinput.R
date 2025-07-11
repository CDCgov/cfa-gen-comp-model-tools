#' Generate deSolve input from compiled model
#'
#' @param outlist compiled model
#' @return character vector with strings corresponding
#' to instantaneous population change as values.
#' @family conversions
modeloutput2odeinput <- function(outlist) {
  currpeter <- outlist$modeloutstructions$petermatrix
  currprocessrates <- outlist$modeloutstructions$processrates
  dxdt <- apply(currpeter, 1, peterrow2dxdt, currprocessrates)
  return(dxdt)
}
