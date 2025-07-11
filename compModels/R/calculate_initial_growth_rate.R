#' Calculates initial growth rate of outbreak
#'
#' Helper function that calls [compModels::apply_growth_rate_calc()] to
#' calculate the initial growth rate for the outbreak from user supplied
#' compartmental model output (deterministic = one data frame, stochastic = one
#' data frame or a list of data frames). The calculation is accomplished by
#' regressing the log of the incidence on time to estimate the rate of
#' exponential increase r. This r can then be used to estimate R0 with the
#' relationship R0 = V*r + 1 where V is the serial interval.
#'
#' #' Currently this calculation is for an SIR model and uses the equation
#' exp(i0) * exp((beta - gamma) * t)
#'
#' @param output data frame output from a compartmental model
#' @param time_var character string specifying the name of the time variable,
#' defaults to "time"
#' @param i_var character string specifying the name of the infected state
#' variable of interest, defaults to "i"
#' @param beta numeric value for transmission rate, default is NULL
#' @param gamma numeric value for recovery rate, default is NULL
#' @param lower_time numeric value for time step on which will serve as lower
#' bound in initial growth rate calculation, default is NULL
#' @param upper_time numeric value for time step on which will serve as upper
#' bound in initial growth rate calculation, default is NULL
#' @return prints initial growth rate(s) to console (numeric) and returns one
#' or more plots of the simulated observations, calculated theoretical growth
#' rate in the user-specified time range, and observed growth rate based on the
#' calculated growth rate r.
#' @family calculations
#' @export
#' @examples
#' \dontrun{
#' # Deterministic Model
#' out <- run_sir(
#'   init = c(s = 1e05 - 1, i = 1, r = 0),
#'   time = seq(0.1, 100, by = 0.1),
#'   parms = c(beta = 0.5, gamma = 0.1)
#' )
#' calculate_initial_growth_rate(out,
#'   time_var = "time",
#'   beta = 0.5, gamma = 0.1,
#'   lower_time = 10,
#'   upper_time = 25
#' )
#'
#' # Stochastic Model with 10 simulations
#' init_vals <- c(s = 499, i = 1, r = 0)
#' change_matrix <- matrix(
#'   c(
#'     -1, 0, # S -> I #nolint
#'     +1, -1, # I -> R #nolint
#'     0, +1
#'   ),
#'   nrow = length(init_vals),
#'   byrow = TRUE,
#'   dimnames = list(names(init_vals), NULL)
#' )
#'
#' modelout <- run_gen_stochastic(
#'   parms_vec = c(
#'     beta = 0.5, gamma = 0.1
#'   ),
#'   propensity_fns = c(
#'     "(beta * s * i)/n",
#'     "gamma * i"
#'   ),
#'   init_vals = init_vals,
#'   n_timesteps = 100,
#'   change_matrix = change_matrix,
#'   n_sims = 10
#' )
#' plot_stoch_model(modelout, compartments = c("s", "i", "r"))
#' calculate_initial_growth_rate(modelout,
#'   time_var = "t", i_var = "i",
#'   beta = 0.5, gamma = 0.1,
#'   lower_time = 0,
#'   upper_time = 25
#' )
#' }
calculate_initial_growth_rate <- function(output, time_var = NULL,
                                          i_var = "i",
                                          beta = NULL, gamma = NULL,
                                          lower_time = NULL,
                                          upper_time = NULL) {
  if (is.data.frame(output)) {
    if (!i_var %in% colnames(output)) {
      stop(paste("Column", i_var, "not found in the output data frame."))
    }
    if (!time_var %in% colnames(output)) {
      stop(paste("Column", time_var, "not found in the output data frame."))
    }
    print("output is a single data frame")
    return(apply_growth_rate_calc(
      output, time_var, i_var, beta, gamma,
      lower_time, upper_time
    ))
  } else if (is.list(output)) {
    print("output is a list of data frames")
    lapply(output, function(df) {
      apply_growth_rate_calc(
        df, time_var, i_var, beta, gamma,
        lower_time, upper_time
      )
    })
  } else {
    stop("Invalid input: output must be a data frame or a list of data frames.")
  }
}
