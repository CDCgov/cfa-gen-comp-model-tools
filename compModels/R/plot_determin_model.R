#' Plots deterministic compartmental models
#'
#' Plots deterministic compartmental model output in using [ggplot2::ggplot()]
#'
#' @param output data frame output from a compartmental model
#' @param stratify_by optional argument where user can name stratifications to
#' include. Each name will result in one plot per strata
#' @param show_intervention_period optional logical argument (default FALSE) to
#' add line(s) showing the start and/or end of the intervention
#' @param intervention_period optional argument (default NULL) to add a vector
#' (type: numeric) with the start and end time of an intervention
#' @return ggplot2 plot of compartmental model output
#' @import ggplot2
#' @importFrom rlang .data
#' @name .data
#' @rdname plot_determin_model
#' @export
#' @examples
#' \dontrun{
#' modelout <- run_sir(
#'   init = c(s = 1e05 - 1, i = 1, r = 0),
#'   time = seq(0.1, 100, by = 0.1),
#'   parms = c(beta = 0.00001, gamma = 0.1)
#' )
#' plot_determin_model(modelout)
#'
#' modelout2 <- run_sir_intervention(
#'   init = c(s = 1e05 - 1, i = 1, r = 0),
#'   time = seq(0.1, 100, by = 0.1),
#'   parms = list(
#'     beta = 0.00001, gamma = 0.1,
#'     intervention_start_time = 10,
#'     intervention_end_time = 20,
#'     intervention_impact = 0.3
#'   )
#' )
#' plot_determin_model(modelout2,
#'   show_intervention_period = TRUE,
#'   intervention_period = c(10, 20)
#' )
#' }
plot_determin_model <- function(output, stratify_by = NULL,
                                show_intervention_period = FALSE,
                                intervention_period = NULL) {
  # rrlang::.data used to address R package notes for no visible binding

  out_long <- as.data.frame(
    tidyr::pivot_longer(as.data.frame(output), -.data$time) |>
      dplyr::rename(variable = .data$name)
  )

  if (!is.null(stratify_by)) {
    # Filter rows where variable names contain the stratify_by value
    out_long <- out_long[grepl(stratify_by, out_long$variable), ]
  }

  if (show_intervention_period) {
    ggplot(
      out_long,
      aes(x = .data$time, y = .data$value, colour = .data$variable)
    ) +
      geom_line(lwd = 2) +
      xlab("Time") +
      ylab("Number") +
      labs(color = "State") +
      theme_classic() +
      geom_vline(
        xintercept = intervention_period[1],
        linetype = "longdash"
      ) +
      geom_vline(
        xintercept = intervention_period[2],
        linetype = "longdash"
      )
  } else {
    ggplot(
      out_long,
      aes(x = .data$time, y = .data$value, colour = .data$variable)
    ) +
      geom_line(lwd = 2) +
      xlab("Time") +
      ylab("Number") +
      labs(color = "State") +
      theme_classic()
  }
}
