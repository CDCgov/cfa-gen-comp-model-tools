#' Set initial population states for compiled
#' model using base states
#'
#' Sets initial conditions globally based on base states.
#'
#' @param compiledmodel compiled model output
#' @param basestatevector numeric vector specifying initial populations
#' for named basestates
#' default is c() which specifies all 0 population
#' for each basestate
#' @return tibble with columns of updated state names
#' (updatedstates) and initial conditions (X0)
#' @export
#' @importFrom rlang .data
define_initialstate <- function(compiledmodel, basestatevector = c()) {
  # ensure proper order
  tblupdatestate <- tibble::tibble(
    updatedstate =
      compiledmodel$modeloutstructions$updatedstates
  ) |>
    dplyr::left_join(
      compiledmodel$modelinstructions$tblupdatedstates |>
        dplyr::distinct(.data$updatedstate, .keep_all = TRUE),
      by = "updatedstate"
    )

  tblout <- tblupdatestate |> dplyr::mutate(X0 = 0)
  if (length(basestatevector) > 0) {
    currbasestates <- tblupdatestate |>
      dplyr::distinct(.data$basestates) |>
      dplyr::pull()
    # check that the input names are uniquely defined basestates
    bstatenames <- names(basestatevector)
    if (FALSE %in% (bstatenames %in% currbasestates)) {
      stop("initial condition vector must have names that match basestates")
    }
    if (length(bstatenames) != length(unique(bstatenames))) {
      stop("basestates must be uniquely")
    }
    jointhistbl <- tibble::enframe(basestatevector) |>
      dplyr::rename(basestates = "name", X0 = "value")
    tblout <- tblupdatestate |>
      dplyr::left_join(jointhistbl, by = "basestates") |>
      tidyr::replace_na(list(X0 = 0))
  } else {
    tblout <- tblout |> dplyr::mutate(X0 = 0)
  }
  return(tblout)
}
