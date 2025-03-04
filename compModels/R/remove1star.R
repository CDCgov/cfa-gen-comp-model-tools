#' Simplifies math equations
#'
#' Remove "1*" from string only if no other numbers or decimals precede 1
#'
#' @param string Character vector to remove "1*" substring
#' @return string with "1*" removed
remove1star <- function(string) {
  outstring <- string
  tokenizedstr <- tokenizerate(string)
  staridx <- which(tokenizedstr == "*")
  unityidx <- which(tokenizedstr == "1")
  if ((length(staridx) > 0) && (length(unityidx) > 0)) {
    unitystaridx <- intersect(unityidx, staridx - 1)
    killidx <- c(unitystaridx, unitystaridx + 1)
    cleantokenedstring <- tokenizedstr[-killidx]
    outstring <- paste(cleantokenedstring, collapse = "")
  }
  return(outstring)
}
