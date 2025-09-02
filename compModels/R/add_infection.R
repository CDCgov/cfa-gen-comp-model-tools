#' add infection to model
#'
#' Helper function to add specific type of interaction
#'
#' @param peterlist list of instructions for piping |>
#' @param infector base state that is infectious
#' @param infectee base state that is susceptible
#' @param infected base state that infectee
#' converts to following infectious
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
#' @param groupnames specifies which stratified groups interact
#' Either a character vector of groupnames or named list with grouptype names
#' and groupname values
#' default is "" which specifies all groups.
#' @param crossgroupnames to specify cross infection terms
#' Either a character vector of groupnames or named list with grouptype names
#' and groupname values or un-named list
#' default is "" which generates all cross terms, specify with vector or named
#' list. specify multiple cross terms by specifying in un-named list  with each
#' value a character vector of groupnames or named list with grouptype names
#' @param symmetric logical denoting whether rate specifies symmetric
#' interactions. Relevant when groupnames is an un-named list. if FALSE then
#' groupnames specifies infector and crossgroupnames specify infectee.
#' @return updated instruction list
#' @family model_building
#' @export
add_infection <- function(
    peterlist,
    infector,
    infectee,
    infected,
    rate,
    normlogic = TRUE,
    metapopulation = "",
    processname = NA,
    processgroup = NA,
    groupnames = "",
    crossgroupnames = "",
    symmetric = TRUE) {
  if (is.list(crossgroupnames) && is.null(names(crossgroupnames))) {
    for (currcrossgroupname in crossgroupnames) {
      peterlist <- add_interaction(peterlist,
        c(infector, infectee),
        c(infector, infected),
        rate = rate,
        normlogic = normlogic,
        metapopulation = metapopulation,
        processname = processname,
        processgroup = processgroup,
        groupnames = groupnames,
        crossgroupnames = currcrossgroupname,
        symmetric = symmetric
      )
    }
  } else {
    peterlist <- add_interaction(peterlist,
      c(infector, infectee),
      c(infector, infected),
      rate = rate,
      normlogic = normlogic,
      metapopulation = metapopulation,
      processname = processname,
      processgroup = processgroup,
      groupnames = groupnames,
      crossgroupnames = crossgroupnames,
      symmetric = symmetric
    )
  }

  return(peterlist)
}
