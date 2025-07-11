#' Generate deSolve input function from compiled model
#'
#' @param outlist compiled model including
#' name "modeloutsructions"
#' @return deSolve input function.
#' @family conversions
#' @export
model2desolvefunction <- function(outlist) {
  odestr <- modeloutput2odeinput(outlist)
  currode <- odegenerator(odestr)
  return(currode)
}
