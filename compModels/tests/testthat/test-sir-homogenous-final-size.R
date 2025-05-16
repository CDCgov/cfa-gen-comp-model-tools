test_that("Iterative test of final size", {
  base_states <- c("S", "I", "R")
  sir <- define_states(base_states) |>
    add_infection("I", "S", "I", "beta") |>
    add_transition("I", "R", "tau")
  sircompiled <- compilemodel(sir)
  sir_rates <- sircompiled$modeloutstructions$processrates
  sir_peter <- sircompiled$modeloutstructions$petermatrix
  sir_states <- sircompiled$modeloutstructions$updatedstates
  init_vals <- c("S" = 9999, "I" = 1, "R" = 0)
  parameters <- c(beta = 2, tau = 1)

  dyn <- wrap_ode(init_vals, seq(0, 25, by = 0.01), sircompiled, parameters)
  plot_determin_model(dyn)

  fs_sim <- tail(as.data.frame(dyn)$R, n = 1)
  n <- sum(init_vals)

  # Final Size test for homogenous SIR single group and fully susceptible
  r_0 <- parameters[["beta"]] / parameters[["tau"]] # same as parms above
  homog_sir_final_size_pred <- function(r_0,
                                        eps = 10^(-8),
                                        maxit = 1000000) {
    cat("Running with R0 =", r_0, "\n")
    tol <- eps^2 # for determining when to stop calculations
    a_old <- eps
    counter <- 0

    while (counter < maxit) {
      a_new <- 1 - exp(-r_0 * a_old)
      if (abs(a_old - a_new) < tol) { # stop if A doesn't change much
        break
      }
      a_old <- a_new
      counter <- counter + 1
    }

    cat("the convergence required", counter, "steps\n")
    return(a_new)
  }

  print(paste("provided R0:", r_0))
  r_0_values <- c(
    0.1, 0.25, 0.5, 0.75, 0.9, 0.99, 1,
    1.01, 1.1, 1.25, 1.5, 1.75, 2.0, r_0
  )
  print(paste("R0 values:", r_0_values))

  plot(NULL,
    xlim = range(r_0_values), ylim = c(0, 1),
    xlab = "r0", ylab = "A", main = "Plot of A vs r0"
  )

  for (r_0 in r_0_values) {
    a <- homog_sir_final_size_pred(r_0)
    cat("found A=", a, "\n\n")
    points(r_0, a, col = "blue", pch = 19)
  }
  points(r_0, homog_sir_final_size_pred(r_0), col = "red", pch = 3) # iterative
  points(r_0, fs_sim / n, col = "green", pch = 4) # simulation

  # Compare (with conversion from probability to count for fs_analytic)
  cat("Final size (simulation)", fs_sim / n, "\n")
  cat("Analytical expected final size:", a, "\n")
  expect_equal(fs_sim / n, a, tolerance = 0.0001)
})
