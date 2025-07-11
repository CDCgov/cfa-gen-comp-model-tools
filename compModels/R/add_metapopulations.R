#' Adds metapopulations to model instructions
#'
#' Adds metapopulations to instructions, automatically renames
#' empty metapopulation names
#'
#' @param peterlist list of instructions for piping |>
#' @param metapopulation character vector of
#' metapopulation names to add.
#' default is "" where compilation automatically
#' generates a placeholder name for each add_metapopulation use
#' @param scaleinteractions character/numeric vector
#' specifying multiplicative scaling of interaction
#' rates in each specifed metapopulation
#' default is 1
#' @param scaletransitions character/numeric vector
#' specifying multiplicative scaling of transition
#' rates in each specifed metapopulation
#' default is 1
#' @param scaleprocessbyname named list with either vectors of the same length
#' as the input metapopulations or length 1. Scales named processes in
#' each metapopulation.
#' @param scaleprocessbygroup named list with either vectors of the same length
#' as the input metapopulations or length 1. Scales grouped named processes in
#' each metapopulation.
#' @param basestates specifies which basestates can
#' visit metapopulation
#' default is "" which species all basestates
#' @param groups specifies which groups can visit metapopulation. This can be a
#' named list with grouptypes as names and groups as elements
#' or a vector of group names
#' default is "" which species all groups
#' @return updated instruction list
#' @family model_building
#' @export
add_metapopulations <- function(
    peterlist, metapopulation = "",
    scaleinteractions = 1, scaletransitions = 1,
    scaleprocessbyname = list(), scaleprocessbygroup = list(),
    basestates = "", groups = "") {
  # there should only be one "" so this is robust -- perhaps update
  if ("" %in% peterlist$space$metapopulation) {
    warning("Prior metapopulation not named. Renaming to Metapopulation 1.")
    peterlist$space$metapopulation <- "Metapopulation 1"
  }

  newinputs <- make_metapopulations(
    metapopulation = metapopulation,
    scaleinteractions = scaleinteractions,
    scaletransitions = scaletransitions,
    scaleprocessbyname = scaleprocessbyname,
    scaleprocessbygroup = scaleprocessbygroup,
    basestates = basestates,
    groups = groups
  )
  if ("" %in% newinputs$metapopulation) {
    filteredspace <- peterlist$space |>
      dplyr::filter(grepl("Metapopulation ", metapopulation))
    if (nrow(filteredspace) > 0) {
      # this will break if someone inputs "metapopulation_string",
      # where string is non-numeric. fix later
      maxidx <- max(as.numeric(sub(
        ".*Metapopulation ", "",
        filteredspace |>
          dplyr::pull(metapopulation)
      )))
      newidx <- maxidx + 1
      warning(paste0("Added metapopulation not named.
                     Renaming to Metapopulation ", as.character(newidx), "."))
      newinputs$metapopulation <-
        paste0("Metapopulation ", as.character(newidx))
    } else {
      warning("Added metapopulation not named. Renaming to Metapopulation 1.")
      newinputs$metapopulation <- "Metapopulation 1"
    }
  }
  peterlist$space <- rbind(peterlist$space, newinputs)
  return(peterlist)
}
