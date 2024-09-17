#' Plots stochastic compartmental models
#'
#' Plots stochastic compartmental model output using [ggplot2::ggplot()]
#'
#' @param output data frame output from a compartmental model
#' @param compartment character string specifying which compartment to plot,
#' defaults to "i" for infected
#' @param time_var character string specifying the name of the time variable,
#' defaults to t
#' @param show_intervention_period optional logical argument (default FALSE) to
#' add line(s) showing the start and/or end of the intervention
#' @param intervention_period optional argument (default NULL) to add a vector
#' (type: numeric) with the start and end time of an intervention
#' @return ggplot2 object of compartmental model output
#' @import ggplot2
#' @export
#' @examples
#' \dontrun{
#' modelout <- run_sir_stochastic(
#'   0.001, 0.1, 499, 100, 0, 500, 100,
#'   matrix(c(-1, 0, +1, -1, 0, +1),
#'     nrow = 3, byrow = TRUE
#'   ), 10
#' )
#' plot_stoch_model(modelout)
#'
#' modelout2 <- run_sir_stochastic_tau(
#'   0.00001, 0.1, 1e05 - 1, 1, 0, 1e05, 100,
#'   list(c(s = -1, i = 1), c(i = -1, r = 1)),
#'   10
#' )
#' plot_stoch_model(modelout2)
#' }
plot_stoch_model <- function(output,
                             compartment = "i",
                             time_var = "t",
                             show_intervention_period = FALSE,
                             intervention_period = NULL) {
  combined_sims_df <- dplyr::bind_rows(lapply(seq_along(output), function(i) {
    cbind(simulation = i, output[[i]])
  }), .id = "simulation_id")

  if (show_intervention_period == TRUE) {
    ggplot(
      combined_sims_df,
      aes(
        x = !!rlang::sym(time_var),
        y = !!rlang::sym(compartment)
      )
    ) +
      # rrlang::.data used to address R package notes for no visible binding
      geom_line(aes(group = .data$simulation_id),
        alpha = 0.5
      ) +
      labs(
        title = paste(
          "Stochastic SIR Model Simulations - ",
          compartment, " Compartment"
        ),
        x = "Time",
        y = paste("Number of ", compartment, " Individuals")
      ) +
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
      combined_sims_df,
      aes(
        x = !!rlang::sym(time_var),
        y = !!rlang::sym(compartment)
      )
    ) +
      geom_line(aes(group = .data$simulation_id),
        alpha = 0.5
      ) +
      labs(
        title = paste(
          "Stochastic SIR Model Simulations - ",
          compartment, " Compartment"
        ),
        x = "Time",
        y = paste("Number of ", compartment, " Individuals")
      ) +
      theme_classic()
  }
}
