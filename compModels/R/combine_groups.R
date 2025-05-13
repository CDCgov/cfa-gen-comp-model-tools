#' Combine group types
#'
#' Adds instructions to add states that are combinations of a given type
#'
#' @param peterlist list of instructions for piping |>
#' @param grouptypes2combine character vector of grouptypes to combine
#' @return updated model instructions
#' @export
combine_groups <- function(
    peterlist, grouptypes2combine = "") {
  if (!is.vector(grouptypes2combine)) {
    stop("Input grouptypes to combine must be a character vector")
  }

  peterlist$combinetypes[[length(peterlist$combinetypes) + 1]] <-
    grouptypes2combine

  return(peterlist)
}
