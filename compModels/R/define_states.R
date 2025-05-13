#' Initializes model instructions with given base states
#'
#' @param base_state character vector of base states.
#' @param metapopulation character vector of metapopulation names
#' default is ""
#' @return list of model instructions
#' @export
define_states <- function(base_state, metapopulation = "") {
  # check space input
  if (length(metapopulation) > 1) {
    stop("Only name a single metapopulation using define_states")
  }

  peterlist <- list()
  peterlist$states <- base_state

  peterlist$space <- tibble::tibble(
    metapopulation = metapopulation,
    scaleinteractions = 1,
    scaletransitions = 1,
    scaleprocessbyname = list(list()),
    scaleprocessbygroup = list(list()),
    basestates = list(base_state),
    groups = ""
  )

  # use this to edit processes by name
  peterlist$process <- tibble::tibble(
    statesin = list(),
    statesout = list(),
    processname = character(),
    processgroup = character(),
    processtype = character(),
    metapopulation = character()
  )

  peterlist$travel <- tibble::tibble(
    frommetapopulation = character(),
    tometapopulation = character(),
    rate = character(),
    processname = character(),
    processgroup = character(),
    travelbasestates = list(),
    travelgroups = list()
  )

  peterlist$interactions <- tibble::tibble(
    states_in = list(),
    states_out = list(),
    rate = character(),
    normlogic = logical(),
    metapopulation = character(),
    processname = character(),
    processgroup = character(),
    groupname = character(),
    grouptype = character()
  )

  peterlist$transitions <- tibble::tibble(
    fromstate = character(),
    tostate = character(),
    fromchain = numeric(),
    tochain = numeric(),
    fromgroups = list(),
    togroups = list(),
    rate = character(),
    percapitastate = character(),
    metapopulation = character(),
    processname = character(),
    processgroup = character(),
  )

  peterlist$groups <- tibble::tibble(
    groupname = character(),
    grouptype = character(),
    basestates = list(),
    scaleinteractions = character(),
    scaletransitions = character(),
    scalemigrations = character(),
    scaleprocessbyname = list(),
    scaleprocessbygroup = list()
  )
  # named list to specify, '' for all interactions

  peterlist$combinetypes <- list()

  return(peterlist)
}
