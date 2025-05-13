#' Simplify auto-generated math expressions
#'
#' This function appends "*" to rates and then converts the rates "1*"
#' and "*" to empty strings "".
#' This facilitates and simplifies specifying rates
#' auto-generated during model compilation
#'
#'
#' @param currtbl A dataframe/tibble with columns
#' "scaleinteractions", "scaletransitions", "scalemigrations"
#' @return tibble with updated scaleinteractions, scaletransitions, and
#' scalemigrations values
#' @importFrom rlang .data
cleanscale <- function(currtbl) {
  loopcols <- c("scaleinteractions", "scaletransitions", "scalemigrations")
  loopcols <- intersect(colnames(currtbl), loopcols)

  cleanfun <- function(x) {
    currnames <- names(x)
    x <- paste0(as.character(x), "*")
    x <- dplyr::case_when(x == "*" ~ "",
      x == "1*" ~ "",
      .default = x
    )
    names(x) <- currnames
    return(x)
  }
  # scale process name
  loopcols <- c(
    "scaleinteractions",
    "scaletransitions",
    "scalemigrations",
    "scaleprocessbyname",
    "scaleprocessbygroup"
  )
  loopcols <- intersect(colnames(currtbl), loopcols)
  for (currcol in loopcols) {
    if (is.list(currtbl[[currcol]])) {
      currtbl[[currcol]] <- lapply(currtbl[[currcol]], cleanfun)
    } else {
      currtbl[[currcol]] <- cleanfun(currtbl[[currcol]])
    }
  }

  return(currtbl)
}
