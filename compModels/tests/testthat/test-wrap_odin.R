test_that("Simulation results are as expected", {
  simulation_results_raw <- "
  2,266
  3,392
  6,174
  8,21
  10,651
  11,18
  12,596
  19,599
  20,451
  21,324
  24,516
  32,499
  35,302
  37,527
  39,55
  41,364
  42,568
  43,461
  44,323
  47,599
  49,410
  50,124
  51,465
  53,561
  62,206
  63,165
  65,18
  67,381
  69,545
  70,567
  72,307
  74,504
  76,459
  79,479
  80,506
  84,94
  85,461
  87,179
  88,84
  89,522
  94,197
  95,610
  96,498
  97,509
  100,420"

  # Split into lines and remove empty lines
  lines <- strsplit(simulation_results_raw, "\n")[[1]]
  lines <- lines[lines != ""]

  # Split each line by comma and convert to data frame
  data_list <- strsplit(lines, ",")
  simulation_results <- data.frame(
    sim = as.integer(sapply(data_list, `[`, 1)),
    tot_inf = as.integer(sapply(data_list, `[`, 2))
  )


  base_states <- c("S", "E", "I", "R")
  infector_state <- "I"
  infectee_state <- "S"

  seirmodel <- define_states(base_states) |>
    add_infection(infector_state, infectee_state, "E", "beta") |>
    add_transition("E", "I", "alpha") |>
    add_transition("I", "R", "gamma")

  seircompiled <- compilemodel(seirmodel)

  initial_states_vec <- c("S" = 999, "I" = 1, "R" = 0, "E" = 0)
  param_vector <- c("beta" = 0.3, "alpha" = 3, "gamma" = 5)
  nsims <- 100
  ntimesteps <- 100

  odin_mod_results <- wrap_odin(
    init_vals = initial_states_vec,
    compiledmodel = seircompiled,
    param_vector = param_vector,
    n_timesteps = ntimesteps,
    n_sims = nsims
  )

  tot_case_df <- odin_mod_results |>
    dplyr::group_by(sim) |>
    dplyr::summarise(tot_inf = max(R)) |>
    dplyr::filter(tot_inf > 10)


  compare_mod_results <- ks.test(
    tot_case_df$tot_inf,
    simulation_results$tot_inf
  )

  compare_mod_results_p <- compare_mod_results$p.value

  expect_gt(compare_mod_results_p, 0.05)
})
