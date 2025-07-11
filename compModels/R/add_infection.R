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
#' @param groupnames specifies which stratified groups
#' transition
#' default is "" which specifies all groups.
#' @return updated instruction list
#' @family model_building
#' @export
add_infection <- function(
    peterlist, infector,
    infectee, infected, rate, normlogic = TRUE, metapopulation = "",
    processname = NA, processgroup = NA,
    groupnames = "") {
  peterlist <- add_interaction(peterlist,
    c(infector, infectee),
    c(infector, infected),
    rate = rate, normlogic = normlogic,
    metapopulation = metapopulation,
    processname = processname, processgroup = processgroup,
    groupnames = groupnames
  )
  return(peterlist)
}
