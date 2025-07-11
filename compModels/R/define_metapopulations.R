#' Define metapopulation names
#'
#' Removes all prior add_metapopulation instructions and sets
#' them according to input
#'
#' @param peterlist list of instructions for piping |>
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
#' @return model instructions
#' @family model_building
#' @export
define_metapopulations <- function(peterlist,
                                   metapopulation = "",
                                   scaleinteractions = 1,
                                   scaletransitions = 1,
                                   scaleprocessbyname = list(),
                                   scaleprocessbygroup = list(),
                                   basestates = "",
                                   groups = "") {
  peterlist$space <- make_metapopulations(
    metapopulation = metapopulation,
    scaleinteractions = scaleinteractions,
    scaletransitions = scaletransitions,
    scaleprocessbyname =
      scaleprocessbyname,
    scaleprocessbygroup =
      scaleprocessbygroup,
    basestates = basestates,
    groups = groups
  )

  return(peterlist)
}
