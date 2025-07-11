#' add replacement in model
#'
#' Helper function that is same as transiton,
#' but simplifies specifying independent transitions
#' from multiple states to a single state.
#' @param peterlist list of instructions for piping |>
#' @param statesout character vector of state names
#' to transition from
#' @param statesin character vector of basestates to
#' transition to
#' @param meantime character/numeric value specifying
#' average time to go from statesout to statesin
#' @param rate character/numeric value specifying rate to go from statesout to
#' statesin
#' @param processname character value for referencing the added process
#' by other functions
#' default is "replacement"
#' @param processgroup character value that groups named process to quickly
#' reference multiple processes by other functions
#' default to NA
#' @param chainlength total steps from statesout to
#' statesin (i.e., boxcars or length of pitchfork)
#' Alters distribution of transition times during
#' stochastic implementation
#' default to 1 which gives exponentially distributed transition times
#' @param chaintimescale character/numeric vector
#' with length chainlength scales rate transitioning
#' between steps along chains. Allows deviating
#' transition time distribution from a gamma distributions
#' default to NA which is same transition times between
#' all chained states (e.g., gamma distributed wait times)
#' @param percapita specifies rates are given as per
#' capita value so the total change to population
#' should be scaled by the tostate
#' default is TRUE
#' @param metapopulation character vector of metapopulation
#' names this transitions occurs in.
#' default "" is which specifies all metapopulations
#' @param groupname Groups the transition applies to.
#' Either a character vector of groupnames or named list with grouptype names
#' and groupname values
#' default is "" which specifies all groups.
#' @return updated instruction list
#' @family model_building
#' @export
add_replacement <- function(
    peterlist,
    statesout,
    statesin,
    meantime,
    rate,
    processname = "replacement",
    processgroup = NA,
    chainlength = 1,
    chaintimescale = NA,
    percapita = TRUE,
    metapopulation = "",
    groupname = "") {
  if (length(statesin) != 1) {
    if (length(statesout) != length(statesin)) {
      stop("Error: vectors defining states
    replacing and replaced must be same length
    or the replacing vector must be length 1.")
    }
  } else {
    statesin <- rep_len(statesin, length(statesout))
  }

  for (seqidx in seq_along(statesout)) {
    peterlist <- peterlist |>
      add_transition(statesout[[seqidx]],
        statesin[[seqidx]], meantime, rate,
        processname = processname,
        chainlength = chainlength, chaintimescale = chaintimescale,
        percapita = percapita, metapopulation = metapopulation,
        groupname = groupname
      )
  }
  return(peterlist)
}
