#' Plots stochastic compartmental models
#'
#' Plots stochastic compartmental model output using [ggplot2::ggplot()]
#'
#' @param output data frame output from a compartmental model
#' @param compartments character string specifying which compartment to plot,
#' defaults to "i" for infected
#' @param time_var character string specifying the name of the time variable,
#' default is NULL, internal logic checks vector of possible time variable names
#' (t, time)
#' @param show_intervention_period optional logical argument (default FALSE) to
#' add line(s) showing the start and/or end of the intervention
#' @param intervention_period optional argument (default NULL) to add a vector
#' (type: numeric) with the start and end time of an intervention
#' @param colors optional argument taking a vector of colors to use in plotting,
#' default is RColorBrewer::brewer.pal(8, "Dark2") and colors are used in
#' [ggplot2::scale_color_manual()] values argument. To have more colors, modify
#' the following for the palette and number you might need (e.g. 40 colors):
#' colors = colorRampPalette(RColorBrewer::brewer.pal(8, "Dark2"))(40)
#' @return ggplot2 object of compartmental model output
#' @family plotting
#' @import ggplot2
#' @export
#' @examples
#' \dontrun{
#' modelout <- run_sir_stochastic(
#'   0.001, 0.1, 499, 100, 0, 500, 100,
#'   c("beta*s*i", "gamma*i"),
#'   matrix(c(-1, 0, +1, -1, 0, +1),
#'     nrow = 3, byrow = TRUE
#'   ), 10
#' )
#' plot_stoch_model(modelout, compartments = c("s", "i", "r"))
#'
#' modelout2 <- run_sir_stochastic_tau(
#'   0.00001, 0.1, 1e05 - 1, 1, 0, 1e05, 100,
#'   list(c(s = -1, i = 1), c(i = -1, r = 1)),
#'   10
#' )
#' plot_stoch_model(modelout2)
#' }
plot_stoch_model <- function(output,
                             compartments = c("i"),
                             time_var = NULL,
                             show_intervention_period = FALSE,
                             intervention_period = NULL,
                             colors = RColorBrewer::brewer.pal(8, "Dark2")) {
  possible_time_vars <- c("t", "time")

  if (is.null(time_var)) {
    found_time_var <- FALSE
    for (possible_time_var in possible_time_vars) {
      if (possible_time_var %in% colnames(output[[1]])) {
        time_var <- possible_time_var
        found_time_var <- TRUE
        print(paste0("Found time variable:", possible_time_var))
        break
      }
    }
    if (!found_time_var) {
      stop("Time variable not found in output data.")
    }
  }

  combined_sims_df <- dplyr::bind_rows(lapply(seq_along(output), function(i) {
    cbind(simulation = i, output[[i]])
  }), .id = "simulation_id")

  unique_compartments <- unique(compartments)
  num_colors <- length(unique_compartments)
  colors <- colors[1:num_colors]
  print(paste0(
    "There are ", num_colors, " unique compartments called ",
    paste(unique_compartments, collapse = ", "),
    ", which are given these colors:",
    paste(colors, collapse = ", ")
  ))

  combined_sims_df <- combined_sims_df |>
    tidyr::pivot_longer(
      cols = tidyselect::all_of(compartments),
      names_to = "compartment",
      values_to = "value"
    )

  p <- ggplot(
    combined_sims_df,
    aes(
      x = !!sym(time_var),
      y = .data$value,
      color = .data$compartment,
      group = interaction(
        .data$simulation_id,
        .data$compartment
      )
    )
  ) +
    geom_line(alpha = 0.5) +
    theme_classic()

  if (show_intervention_period == TRUE && !is.null(intervention_period)) {
    p <- p + geom_vline(
      xintercept = intervention_period[1],
      linetype = "longdash"
    ) + geom_vline(
      xintercept = intervention_period[2],
      linetype = "longdash"
    )
  }

  # Add labels and colors
  p <- p + labs(
    title = "Stochastic Model Simulations",
    x = "Time",
    y = "Number of Individuals"
  ) + scale_color_manual(values = colors, name = "Compartment")

  return(p)
}
