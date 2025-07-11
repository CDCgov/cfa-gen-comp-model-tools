#' Combines groups by type
#'
#' Combines groups based on input group types. Useful when compiling to build
#' updated state table.
#'
#' @param combinethesetypes character vector specifying types to combine groups
#' across
#' @param tblgrouptype tibble with given type and group columns
#' @param typecolumn column name for types in tblgrouptype
#' @param groupcolumn column name for groups in tblgrouptype
#' @return tibble of all group combinations with types as columns
#' @family internal
#' @importFrom rlang .data
#' @importFrom rlang :=
grouptypecombinations <- function(combinethesetypes,
                                  tblgrouptype,
                                  typecolumn,
                                  groupcolumn) {
  if (length(combinethesetypes) == 0) {
    stop("Vector specifying types to combine is empty")
  }
  if (is.numeric(tblgrouptype[[typecolumn]])) {
    stop("Types must be characters, not numeric")
  }

  if ((length(typecolumn) != 1) || (is.numeric(typecolumn))) {
    stop("A single type column must be specified")
  }

  if ((length(groupcolumn) != 1) || (is.numeric(groupcolumn))) {
    stop("A single group column must be specified")
  }

  if (length(combinethesetypes) > 1) {
    currtblgroup <- tblgrouptype |>
      dplyr::filter(!!as.symbol(typecolumn) %in% combinethesetypes)
    currtblgrouptypes <- currtblgroup |>
      dplyr::distinct(!!as.symbol(typecolumn)) |>
      dplyr::pull()
    listoftypes <- currtblgroup |> dplyr::group_split(!!as.symbol(typecolumn))
    names(listoftypes) <- currtblgrouptypes
    # types to get combinations
    list2cross <- lapply(listoftypes, function(x) {
      x |> dplyr::pull(!!as.symbol(groupcolumn))
    })
    tblout <- do.call(expand.grid, list2cross)
  } else {
    tblout <- tblgrouptype |>
      dplyr::filter(!!as.symbol(typecolumn) == combinethesetypes) |>
      dplyr::select(!!as.symbol(groupcolumn)) |>
      dplyr::rename(!!combinethesetypes := !!as.symbol(groupcolumn))
  }
  return(tblout)
}
