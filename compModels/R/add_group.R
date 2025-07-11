#' Stratify population into groups
#'
#' @param peterlist list of instructions for piping |>
#' @param groupnames character vector of groups
#' in a given grouptype (e.g., c("Old","Young"))
#' @param grouptype character vector of grouptype (e.g., c("Age"))
#' default is NA which automatically generates a type during
#' model compilation
#' @param basestates character vector of basestates
#' to stratify. default is "" which specifies all basestates
#' @param metapopulation chracter vector of metapopulations where
#' basestates are split
#' @param scaleinteractions character/numeric specifies
#' how to scale interaction rates for each group. default is 1
#' @param scaletransitions character/numeric specifies
#' how to scale transition rates for each group. default is 1.
#' @param scalemigrations character/numeric specifies
#' how to scale migration rates for each group. default is 1.
#' @param scaleprocessbyname named list w/ character/numeric values that
#' scale named processes, allows user defined flexibility
#' @param scaleprocessbygroup named list w/ character/numeric values that
#' scale processes by grouped name, allows user defined flexibility
#' @return updated model instructions
#' @family model_building
#' @export
add_group <- function(
    peterlist, groupnames, grouptype = NA, basestates = "", metapopulation = "",
    scaleinteractions = 1, scaletransitions = 1, scalemigrations = 1,
    scaleprocessbyname = list(), scaleprocessbygroup = list()) {
  if (!is.vector(groupnames)) {
    stop("Input groupnames must be a character or numeric vector")
  }
  if (!is.vector(basestates)) {
    stop("Input basestates must be a character or numeric vector,
     applying to all groupnames for a  single grouptype")
  }
  if (length(grouptype) > 1) {
    stop("More than one grouptype specified, only one type (e.g., Age) may be
         specified even if inputting multiple groupnames (e.g.,c(Young,Old))")
  }

  if (is.na(grouptype)) {
    priortypes <- unique(peterlist$groups$grouptype)
    priordummyidx <- grep("dummygroup", priortypes)
    grouptype <- paste0(
      "dummygroup",
      as.character(length(priordummyidx) + 1)
    )
  }
  # coerce input to correct shape to apply
  numgroups <- length(groupnames)
  grouptype <- matchlength(grouptype, numgroups)
  scaleinteractions <- matchlength(scaleinteractions, numgroups)
  scaletransitions <- matchlength(scaletransitions, numgroups)
  scalemigrations <- matchlength(scalemigrations, numgroups)

  scaleprocessbyname <- matchlength_namedlist(
    scaleprocessbyname,
    numgroups
  )
  scaleprocessbygroup <- matchlength_namedlist(
    scaleprocessbygroup,
    numgroups
  )
  bindthese <- list()
  for (seqidx in seq(numgroups)) {
    currnamedlist <- list(
      groupname = groupnames[[seqidx]],
      grouptype = grouptype[[seqidx]],
      basestates = basestates,
      scaleinteractions = scaleinteractions[[seqidx]],
      scaletransitions = scaletransitions[[seqidx]],
      scalemigrations = scalemigrations[[seqidx]]
    )
    bindthese[[seqidx]] <- namedlist2tibblerow(currnamedlist)
  }
  tblout <- do.call("rbind", bindthese) |>
    dplyr::mutate(
      scaleprocessbyname = scaleprocessbyname,
      scaleprocessbygroup = scaleprocessbygroup
    )
  peterlist$groups <- rbind(peterlist$groups, tblout)

  return(peterlist)
}
