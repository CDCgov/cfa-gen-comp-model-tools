#' add interactions to model
#'
#' general form where users can specify states up
#' and down. Assumes per capita rates of interaction!
#'
#' @param peterlist list of instructions for piping |>
#' @param states_in character vector of base state names
#' that interact
#' @param states_out character vector of base
#' state names after interactions. Assume states_in
#' and states_out are ordered (e.g., S,I -> I,I)
#' @param rate character/numeric value specifying
#' PER CAPITA rate of interaction
#' @param normlogic logical specifying if rate
#' should be divided by total population in given
#' metapopulation
#' @param metapopulation character vector of
#' metapopulations where this interaction occurs
#' default is "" which specifies all metapopulations
#' @param processname character value for referencing the added process
#' by other functions
#' default to NA
#' @param processgroup character value that groups named process to quickly
#' reference multiple processes by other functions
#' default to NA
#' @param groupnames specifies which stratified groups interact.
#' Either a character vector of groupnames or named list with grouptype names
#' and groupname values or un-named list same length as states_in
#' default is "" which specifies all groups.
#' @param crossgroupnames to specify cross infection terms
#' Either a character vector of groupnames or named list with grouptype names
#' and groupname values or un-named list
#' default is "" which generates all cross terms, specify with vector or named
#' list. specify multiple cross terms by specifying in un-named list  with each
#' value a character vector of groupnames or named list with grouptype names
#' @param symmetric logical denoting whether rate specifies symmetric
#' interactions
#' Relevant when groupnames is an un-named list
#' @return updated instruction list
#' @family model_building
#' @export
add_interaction <- function(
    peterlist, states_in, states_out, rate,
    normlogic = TRUE, metapopulation = "", processname = NA, processgroup = NA,
    groupnames = "",
    crossgroupnames = NA, symmetric = TRUE) {
  samelogic <- TRUE
  if (("" %in% states_in) || ("" %in% states_in)) {
    stop("Empty state listed in interaction.
    Birth/death functionality currently not supported.
    Use add_import or add_export for importation/exportation.")
  }
  if (length(states_in) != length(states_out)) {
    stop("interaction can only lead to changes in state.
    states_in and states_out must be same length")
  }

  tibble2add <- namedlist2tibblerow(list(
    states_in = states_in,
    states_out = states_out,
    rate = rate,
    normlogic = normlogic,
    metapopulation = metapopulation,
    processname = processname,
    processgroup = processgroup,
    groupnames = groupnames,
    crossgroupnames = crossgroupnames,
    symmetric = symmetric
  ))

  peterlist$interactions <- rbind(peterlist$interactions, tibble2add)
  return(peterlist)
}
