#' add travel between metapopulations in model
#'
#' @param peterlist list of instructions for piping |>
#' @param rates character/numeric vector movement rate/s
#' @param frommetapopulation character vector of origin
#' metapopulations
#' default is "" which specifies all metapopulations
#' @param tometapopulation character vector of destination
#' metapopulations. default is "" which specifies all
#' metapopulations
#' @param processname string to name process
#' @param processgroup string to group process
#' @param travelbasestates character vector of basestates that
#' travel between metapopulations
#' default is "" which specifies all basestates
#' @param travelgroups character vector of groups that
#' travel between metapopulations
#' default is "" which specifies all basestates
#' @return updated instruction list
#' @export
add_travel <- function(peterlist,
                       rates,
                       frommetapopulation = "",
                       tometapopulation = "",
                       processname = NA,
                       processgroup = NA,
                       travelbasestates = "",
                       travelgroups = "") {
  if (length(frommetapopulation) != length(tometapopulation)) {
    stop("to and from metapopulations are not same length.")
  }
  numtravel <- length(frommetapopulation)

  rates <- matchlength(rates, numtravel)
  processname <- matchlength(processname, numtravel)
  processgroup <- matchlength(processgroup, numtravel)

  if (!is.list(travelbasestates)) {
    travelbasestates <- list(travelbasestates)
  }
  travelbasestates <- matchlength(travelbasestates, numtravel)

  if (!is.list(travelgroups)) {
    travelgroups <- list(travelgroups)
  }

  travelgroups <- matchlength(travelgroups, numtravel)
  # scaleprocessbyname <- matchlength_namedlist(scaleprocessbyname,
  #                                            numtravel)
  # scaleprocessbygroup <- matchlength_namedlist(scaleprocessbygroup,
  #                                             numtravel)

  peterlist$travel <- dplyr::bind_rows(
    peterlist$travel,
    tibble::tibble(
      frommetapopulation = frommetapopulation,
      tometapopulation = tometapopulation,
      rate = rates,
      processname = processname,
      processgroup = processgroup,
      travelbasestates = travelbasestates,
      travelgroups = travelgroups
    )
  )

  return(peterlist)
}
