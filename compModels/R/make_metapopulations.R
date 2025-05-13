#' Set metapopulations in model
#'
#' Checks user input and outputs tibble to add metapopulations to instructions
#'
#' @param metapopulation vector of metapopulation names
#' default is ""
#' @param scaleinteractions vector same length as
#' input metapopulations/environment_names that
#' scales all interactions in given metapop/environment
#' default is 1
#' @param scaletransitions vector same length as
#' input metapopulations/environment_names that
#' scales all transitions in given metapop/environment
#' default is 1
#' @param scaleprocessbyname named list with either vectors of the same length
#' as the input metapopulations or length 1. Scales named processes in
#' each metapopulation.
#' @param scaleprocessbygroup named list with either vectors of the same length
#' as the input metapopulations or length 1. Scales grouped named processes in
#' each metapopulation.
#' @param basestates specifies which states can
#' visit metapopulation/environment.
#' default is "" which specifies all basestates
#' @param groups specifies which groups can visit metapopulation. This can be a
#' named list with grouptypes as names and groups as elements
#' or a vector of group names
#' default is "" which species all groups
#' @return tibble combining all information.
#' @export
#' @importFrom rlang .data
make_metapopulations <- function(
    metapopulation = "", scaleinteractions = 1, scaletransitions = 1,
    scaleprocessbyname = list(), scaleprocessbygroup = list(),
    basestates = "", groups = "") {
  if (!is.list(scaleprocessbyname)) {
    stop("Input scaleprocessbyname must be a list")
  }
  if (!is.list(scaleprocessbygroup)) {
    stop("Input scaleprocessbygroup must be a list")
  }
  # check that interactionscale and transitionscale are correct size
  metapoplength <- length(metapopulation)
  scaletlength <- length(scaletransitions)
  scaleilength <- length(scaleinteractions)
  if (metapoplength == 1) {
    if (scaletlength > 1) {
      stop("Only one metapopulation named but multiple scaletransitions named")
    }
    if (scaleilength > 1) {
      stop("Only one metapopulation named but multiple scaleinteractions named")
    }
  } else {
    if (scaletlength == 1) {
      if (scaletransitions != 1) {
        warning("Multiple metapopulations named but single scaleinteraction
                named. It will be repeated")
      }
    }
    scaletransitions <- matchlength(scaletransitions, metapoplength)

    if (scaleilength == 1) {
      if (scaleinteractions != 1) {
        warning("Multiple metapopulations named but single scaleinteractions
                named. It will be repeated")
      }
      scaleinteractions <- matchlength(scaleinteractions, metapoplength)
    }
  }
  scaleprocessbyname <- matchlength_namedlist(
    scaleprocessbyname,
    metapoplength
  )
  scaleprocessbygroup <- matchlength_namedlist(
    scaleprocessbygroup,
    metapoplength
  )

  if (!is.list(groups)) {
    groups <- list(groups)
  }
  tblout <- tibble::tibble(
    metapopulation = metapopulation,
    scaleinteractions = scaleinteractions,
    scaletransitions = scaletransitions,
    scaleprocessbyname = scaleprocessbyname,
    scaleprocessbygroup = scaleprocessbygroup,
    basestates = list(basestates),
    groups = groups
  )
  return(tblout)
}
