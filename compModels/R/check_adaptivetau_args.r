#' Validate adaptivetau argument values
#'
#' Helper function to validate adaptivetau argument values, used as part of a
#' validation function
#' @param transitions list object with transition information between states
#' @return stops with information or indicates checks passed
check_adaptivetau_args <- function(transitions) {
  if (!is.list(transitions)) stop("transitions must be a list")
  sapply(
    X = transitions,
    FUN = function(x) {
      if (!is.numeric(x)) {
        stop("All elements of transitions list must be numeric")
      }
    }
  )
  sapply(
    X = transitions,
    FUN = function(x) {
      if (!all(!is.null(names(x)))) {
        stop("All elements of transitions list must be named numeric vectors")
      }
    }
  )

  print("adaptivetau argument value checks passed")
}
