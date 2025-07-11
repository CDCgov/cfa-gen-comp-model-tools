#' Generates all combinations groups with different grouptypes
#'
#' Combines groups based on input group types. Useful when compiling to build
#' updated state table.
#'
#' @param combinethesetypes list of character vectors that
#' specify grouptypes to combine groups.
#' @param tblgroup tibble specifying groups in each group type
#' @return tibble where each row is a combination of groups
#' with grouptype columns and scaled process rates
#' @family internal
#' @importFrom rlang .data
#' @importFrom rlang :=
groupcombinations <- function(combinethesetypes, tblgroup) {
  if (length(combinethesetypes) > 1) {
    currtblgroup <- tblgroup |>
      dplyr::filter(.data$grouptype %in% combinethesetypes)
    currtblgrouptypes <- currtblgroup |>
      dplyr::distinct(.data$grouptype) |>
      dplyr::pull()
    listoftypes <- currtblgroup |> dplyr::group_split(.data$grouptype)
    names(listoftypes) <- currtblgrouptypes
    currbasestates <- currtblgroup$basestates[[1]]
    # types to get combinations
    list2cross <- lapply(listoftypes, function(x) {
      x |> dplyr::pull("groupname")
    })
    crossframe <- do.call(expand.grid, list2cross)

    tblout <- crossframe |>
      dplyr::mutate(basestates = list(currbasestates))
  } else {
    tblout <- tblgroup |>
      dplyr::filter(.data$grouptype == combinethesetypes) |>
      dplyr::select("groupname", "basestates") |>
      dplyr::rename(!!combinethesetypes := .data$groupname)
  }
  return(tblout)
}
