#' add transition between groups
#'
#' Add transitions between basestates AND specified groups, fixing all other
#' state features (e.g., other groups, metapopulations). Assumes
#' a "pitchfork" shape where 1 state transitions to one of multiple states after
#' one or multiple steps.
#'
#' @param peterlist list of instructions for piping |>
#' @param fromgroup character specifying group name to transition from
#' @param togroups character vector specifying named group(s) to transition to
#' @param meantime character/numeric value specifying
#' average time to go from fromstate to all tostates
#' @param rate character/numeric value specifying rate to go from fromstate to
#' all tostates
#' @param processname character value for referencing the added process
#' by other functions
#' default to NA
#' @param processgroup character value that groups named process to quickly
#' reference multiple processes by other functions
#' default to NA
#' @param chainlength total steps from fromstate to
#' tostates (i.e., boxcars or length of pitchfork)
#' Alters distribution of transition times during
#' stochastic implementation
#' default to 1 which is exponentially
#' distrubuted transition times
#' @param chaintimescale character/numeric vector
#' with length chainlength scales rate transitioning
#' between steps along chains. Allows deviating
#' transition time distribution from a gamma distributions
#' default to NA which is same transition times between
#' all chained states (e.g., gamma distributed wait times)
#' @param forkprobability character/numeric vector
#' specifying relative weight to transtion to
#' different tostates when multiple are given
#' default to NA which specifies equal probability
#' transitioning to each final state
#' @param percapita specifies rates are given as per
#' capita value so the total change to population
#' should be scaled by the tostate
#' default is TRUE
#' @param fromstate basestate to transition from
#' @param tostates character vector of basestates to
#' transition to
#' @param metapopulation character vector of metapopulation
#' names this transitions occurs in.
#' default "" is which specifies all metapopulations
#' @return updated instruction list
#' @export
add_grouptransition <- function(peterlist, # nolint: cyclocomp_linter.
                                fromgroup,
                                togroups,
                                meantime = NA, rate = NA,
                                processname = NA, processgroup = NA,
                                chainlength = 1, chaintimescale = NA,
                                forkprobability = NA,
                                percapita = TRUE,
                                fromstate = "", tostates = "",
                                metapopulation = "") {
  # check the right length of inputs
  if (identical(fromstate, "")) {
    fromstate <- peterlist$states
  }
  if (identical(tostates, "")) {
    tostates <- peterlist$states
  }

  if (length(fromgroup) > 1) {
    stop("Transitions can only come from one group, multiple groups are input.")
  }

  multilogic <- FALSE
  if (length(fromstate) > 1) {
    if (length(fromstate) != length(tostates)) {
      stop("Input fromstate must be length = 1 or be same length as tostates")
    }
  } else {
    if (length(tostates) > 1) {
      multilogic <- TRUE
      if (length(tostates) != length(togroups)) {
        stop("Input tostate must be same length as fromstate or fromgroup")
      }
    }
  }



  if (
    (!(NA %in% chaintimescale)) &&
      (chainlength > 1) &&
      (length(chaintimescale) != chainlength)
  ) {
    stop("Length of rate scaling between transitions
         don't match input number of transitions.")
  }
  # grab length, allow users to optionally input chaintimescale or chainlength
  if (!(NA %in% chaintimescale)) {
    pitchforklength <- length(chaintimescale)
    chaintimescale <- normalize2mean1(chaintimescale)
  } else {
    pitchforklength <- chainlength
  }

  if (!(NA %in% forkprobability)) {
    if (length(forkprobability) != length(togroups)) {
      stop("Length of groups to transition into is
      different than the input forkprobability rates.")
    }
  } else {
    # assume equal probability of splitting between states
    if (length(togroups) > 1) {
      warning("Multiple final groups but relative rates (forkprobability)
      are not defined or contain NA. Assuming equal
      probability to transition between them")
    }
    forkprobability <- rep_len(1, length(togroups))
  }
  forkprobability <- normalize2probability(forkprobability)

  # Specify process rate
  if (!is.na(meantime) && !is.na(rate)) {
    stop("Redundant input arguments Specify either meantime or rate, not both!")
  }
  if (is.na(meantime) && is.na(rate)) {
    stop("Missing argument. Specify either meantime or rate, but not both.")
  }
  if (!is.na(meantime)) {
    baserate <- time2rate(meantime)
  } else {
    baserate <- as.character(rate)
  }

  # pitchfork shaft
  if (pitchforklength > 1) {
    baserate <- paste0(
      as.character(pitchforklength),
      "*",
      as.character(baserate)
    )
    if (!(NA %in% chaintimescale)) {
      # rates vary across pitchfork
      baserate <- paste0(
        as.character(chaintimescale),
        "*",
        as.character(baserate)
      )
    } else {
      baserate <- rep_len(as.character(baserate), pitchforklength)
    }

    toptibble <- tibble::tibble(
      fromstate = list(fromstate),
      fromchain = seq(pitchforklength - 1),
      tochain = seq(pitchforklength - 1) + 1,
      fromgroups = fromgroup,
      togroups = fromgroup,
      rate = baserate[1:(pitchforklength - 1)],
      percapitastate = percapita,
      metapopulation = metapopulation,
      processname = processname,
      processgroup = processgroup
    ) |>
      tidyr::unnest("fromstate") |>
      dplyr::mutate(tostate = fromstate)
  } else {
    toptibble <- tibble::tibble(
      fromstate = character(),
      tostate = character(),
      fromchain = numeric(),
      tochain = numeric(),
      fromgroups = list(),
      togroups = list(),
      rate = character(),
      percapitastate = logical(),
      metapopulation = character(),
      processname = character(),
      processgroup = character()
    )
  }
  # pitchfork fork
  bottomrate <- baserate[length(baserate)]
  if (length(forkprobability) > 1) {
    bottomrate <- paste0(as.character(forkprobability), "*", bottomrate)
  }

  if (multilogic) {
    bottomtibble <- tibble::tibble(
      fromstate = fromstate,
      tostate = tostates,
      fromchain = pitchforklength,
      tochain = 1,
      fromgroups = fromgroup,
      togroups = togroups,
      rate = bottomrate,
      percapitastate = percapita,
      metapopulation = metapopulation,
      processname = processname,
      processgroup = processgroup
    )
  } else {
    bottomtibble <- tibble::tibble(
      fromstate = list(fromstate),
      fromchain = pitchforklength,
      tochain = 1,
      fromgroups = fromgroup,
      togroups = togroups,
      rate = bottomrate,
      percapitastate = percapita,
      metapopulation = metapopulation,
      processname = processname,
      processgroup = processgroup
    ) |>
      tidyr::unnest("fromstate") |>
      dplyr::mutate(tostate = fromstate)
  }

  if (nrow(peterlist$transitions) == 0) {
    compileid <- 1
  } else {
    compileid <- max(peterlist$transitions$compileid) + 1
  }
  tibbleout <- rbind(toptibble, bottomtibble) |>
    dplyr::mutate(compileid = compileid)
  peterlist$transitions <- rbind(peterlist$transitions, tibbleout)
  return(peterlist)
}
