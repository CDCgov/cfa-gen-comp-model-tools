# Draft model to replicate the modeling work done during the Chicago Measles
# 2024 response. This is a work in progress between Catherine Herzog & Bradford
# Taylor.

# https://www.sciencedirect.com/science/article/pii/S2468266723001305?via%3Dihub#sec1 # nolint
# https://www.cdc.gov/mmwr/volumes/73/wr/mm7319a2.htm
# https://github.com/CDCgov/measles-model-chicago-2024/tree/main

# Loading necessary libraries
library(devtools)
library(Matrix)
library(dplyr)
library(tibble)
library(tidyr)
library(tidyselect)
library(purrr)
library(stringr)
library(deSolve)
library(ggplot2)
# load package w/o installing.
load_all("compModels")

# Currently this function is on a different branch so needs to be brought in
# until it is merged into the main branch.
wrap_adaptivetau <- function(init_vals, compiledmodel, rate_func = NULL,
                             parameters, n_timesteps, n_sims,
                             method = "exact") {
  x0 <- define_initialstate(
    compiledmodel,
    init_vals
  ) |>
    output_initialstate()
  model_rates <- compiledmodel$modeloutstructions$processrates # rateeqns
  model_peter <- compiledmodel$modeloutstructions$petermatrix # transitions
  parameters <- parameters

  t <- n_timesteps

  if (is.null(rate_func)) {
    rate_func <- generalized_rates
  }

  if (method == "exact") {
    sims <- lapply(1:n_sims, function(i) {
      adaptivetau::ssa.exact(
        init.values = x0,
        transitions = as.matrix(model_peter),
        rateFunc = rate_func(as.list(model_rates)),
        params = parameters,
        tf = t
      )
    })
  } else if (method == "adaptivetau") {
    sims <- lapply(1:n_sims, function(i) {
      adaptivetau::ssa.adaptivetau(
        init.values = x0,
        transitions = as.matrix(model_peter),
        rateFunc = rate_func(as.list(model_rates)),
        params = parameters,
        tf = t
      )
    })
  } else {
    stop("Method not recognized. Please use 'exact' or 'adaptivetau'")
  }

  lapply(sims, function(x) data.frame(x))
}

# Measles Models
# The approach here is to build up each piece of functionality in the
# OAW Measles Model then add as many of the modifications from the Chicago
# Measles response as possible.

# First we set up a basic SEIR model with a chain of length 2 in the
# E compartment.
# SE2IR
base_states <- c("S", "E", "I", "R")
se2ir <- define_states(base_states) |>
  add_infection("I", "S", "E", "beta") |>
  add_transition("I", "R", "tau") |>
  add_transition("E", "I", "taue", chainlength = 2)
se2ircompiled <- compilemodel(se2ir)
se2ir_rates <- se2ircompiled$modeloutstructions$processrates
se2ir_peter <- se2ircompiled$modeloutstructions$petermatrix
se2ir_states <- se2ircompiled$modeloutstructions$updatedstates

# Examples with each wrapper, going forward will stick with adaptive tau-leaping
# Both the OAW study and Chicago Measles 2024 response used adaptive tau-leaping
dyn1_g <- wrap_gillespie(
  c("S" = 999, "I" = 1, "R" = 0, "E" = 0),
  se2ircompiled,
  c(beta = 2, tau = 1, taue = .5),
  25,
  10
)
plot_stoch_model(dyn1_g, compartments = se2ir_states)

dyn1_t1 <- wrap_adaptivetau(
  c("S" = 999, "I" = 1, "R" = 0, "E" = 0),
  se2ircompiled,
  rate_func = NULL, # this defaults to compModels::generalized_rates()
  c(beta = 2, tau = 1, taue = .5),
  25,
  10
)
plot_stoch_model(dyn1_t1, compartments = se2ir_states)

dyn1_t2 <- wrap_adaptivetau(
  c("S" = 999, "I" = 1, "R" = 0, "E" = 0),
  se2ircompiled,
  rate_func = NULL, # this defaults to compModels::generalized_rates()
  c(beta = 2, tau = 1, taue = .5),
  25,
  10,
  "adaptivetau"
)
plot_stoch_model(dyn1_t2, compartments = se2ir_states)


# Now we add an intervention: vaccination
# We follow the equations in the supplement (pg 8) of Masters et al 2023 on OAW
# 𝑑𝑆𝑖(𝑡)/𝑑𝑡 = −𝛽𝐼(𝑡)𝑆𝑖(𝑡) − 𝜃(𝑡)𝑆𝑖(𝑡)
# 𝑑𝑆𝑣𝑖(𝑡)/𝑑𝑡 = 𝑝 ∗ 𝜃(𝑡)𝑆𝑣𝑖(𝑡) −𝛽𝐼(𝑡)𝑆𝑣𝑖(𝑡)
# 𝑑𝐸𝑖(𝑡)/𝑑𝑡 = 𝛽𝐼(𝑡)𝑆𝑖(𝑡) + 𝛽𝐼(𝑡)𝑆𝑣𝑖(𝑡)− 𝜎𝐸𝑖(𝑡)
# 𝑑𝐼𝑖(𝑡)/𝑑𝑡 = 𝜎𝐸𝑖 (𝑡) − 𝛾𝐼𝑖(𝑡)
# 𝑑𝑅𝑖(𝑡)/𝑑𝑡 = 𝛾𝐼𝑖(𝑡)+ (1 − 𝑝) ∗ 𝜃(𝑡)𝑆𝑖(𝑡)
# where 𝐼(𝑡) = ∑𝐼𝑖(𝑡) and the force of infection, 𝜆(𝑡) = 𝛽𝐼(𝑡)

# First we add vaccination. V represents compartment Sv, which contained
# once vaccinated individuals who remained susceptible (primary vaccine failure)
# and who could contribute to transmission but who were not vaccinated a second
# time during the initial OAW mass vaccination campaigns. Vaccine efficacy is
# represented with a proportion p.

# SE2IRV (no fork)
base_states <- c("S", "E", "I", "R", "V")
se2irv <- define_states(base_states) |>
  add_infection("I", "S", "E", "beta") |>
  add_transition("I", "R", "tau") |>
  add_transition("E", "I", "taue", chainlength = 2) |>
  add_transition("S", "V", "1/(p*theta)") |>
  # the above and below show how you indicate a rate, not a time
  add_transition("S", "R", "1/((1-p)*theta)") |>
  add_infection("I", "V", "E", "beta")

se2irvcompiled <- compilemodel(se2irv)
se2irv_rates <- se2irvcompiled$modeloutstructions$processrates
se2irv_peter <- se2irvcompiled$modeloutstructions$petermatrix
se2irv_states <- se2irvcompiled$modeloutstructions$updatedstates

dyn2 <- wrap_adaptivetau(
  c("S" = 999, "I" = 1, "R" = 0, "E" = 0, "V" = 0),
  se2irvcompiled,
  rate_func = NULL, # this defaults to compModels::generalized_rates()
  c(beta = 2, tau = 1, taue = .5, theta = .01, p = 0.4),
  25,
  10,
  "adaptivetau"
)
plot_stoch_model(dyn2, compartments = se2irv_states)


# Here we make use of the forking functionality to condense two add_transition()
# into one. This is another way to represent the model.

# SE2IRV (with fork)
se2irvf <- define_states(base_states) |>
  add_infection("I", "S", "E", "beta") |>
  add_transition("I", "R", "tau") |>
  add_transition("E", "I", "taue", chainlength = 2) |>
  add_transition("S", c("V", "R"), "1/(theta)", # indicating a rate
    forkprobability = c("p", "1-p")
  ) |>
  add_infection("I", "V", "E", "beta")

se2irvfcompiled <- compilemodel(se2irvf)
se2irvf_rates <- se2irvfcompiled$modeloutstructions$processrates
se2irvf_peter <- se2irvfcompiled$modeloutstructions$petermatrix
se2irvf_states <- se2irvfcompiled$modeloutstructions$updatedstates

dyn3 <- wrap_adaptivetau(
  c("S" = 999, "I" = 1, "R" = 0, "E" = 0, "V" = 0),
  se2irvfcompiled,
  rate_func = NULL, # this defaults to compModels::generalized_rates()
  c(beta = 2, tau = 1, taue = .5, theta = .01, p = 0.4),
  25,
  10,
  "adaptivetau"
)
plot_stoch_model(dyn3, compartments = se2irvf_states)

# Next we try to handle groups. In the original OAW model there were 5 groups
# based on eligibility for the vaccine: < 6 months, 6-11 months, 1-11 years,
# >= 12 years (not pregnant) and >= 12 years (pregnant). Those < 6 months and
# >=12 years (pregnant) did not receive MMR vaccination. Homogenous mixing was
# assumed given the shelter conditions.

# WORK IN PROGRESS
# Currently we can add groups easily, and as notes show, we are working on
# functionality to remove groups from specific functions (eg from being
# vaccinated in the forked add_transition call)

# SE2IRV with groups
se2irvf_grp <- define_states(base_states) |>
  add_infection("I", "S", "E", "beta") |>
  add_transition("I", "R", "tau") |>
  add_transition("E", "I", "taue", chainlength = 2) |>
  add_transition("S", c("V", "R"), "1/(theta)",
    forkprobability = c("p", "1-p"), # indicating a rate
    groupname = "1"
  ) |> # in future will remove grps 1 and 5
  # runs but doesn't yet work to remove the grp 1 or grp 5,
  # changes needed in compilemodel()
  add_infection("I", "V", "E", "beta") |>
  add_group(as.character(1:5))

se2irvfgcompiled <- compilemodel(se2irvf_grp)
se2irvfg_rates <- se2irvfgcompiled$modeloutstructions$processrates
se2irvfg_peter <- se2irvfgcompiled$modeloutstructions$petermatrix
se2irvfg_states <- se2irvfgcompiled$modeloutstructions$updatedstates

dyn4 <- wrap_adaptivetau(
  c("S" = 999, "I" = 1, "R" = 0, "E" = 0, "V" = 0),
  se2irvfgcompiled,
  rate_func = NULL, # this defaults to compModels::generalized_rates()
  c(beta = 2, tau = 1, taue = .5, theta = .01, p = 0.4),
  25,
  10,
  "adaptivetau"
)
plot_stoch_model(dyn4,
  compartments = se2irvfg_states,
  colors =
    colorRampPalette(RColorBrewer::brewer.pal(8, "Dark2"))(30)
)


# Intervention timing
# Mass vaccinations were provided in specific time blocks so there is a need
# to run the model in blocks of time, stop on mass vaccination dates and adjust
# the state compartments and rates accordinging, then start again until the
# next intervention time. The results of simulations in each time block will
# need to be stitched together.

# WORK IN PROGRESS
# Pull out final line of one simulation
last_row <- tail(dyn4[[1]], n = 1)
last_row



## Future Work & Functionality Needs

# Handling pulsed interventions and stitching together the simulations
# from each time block. Propose 1+ wrappers to address this and enable a
# compiled model object that stores the model for each time block (CH)

# Enable modifications of chained compartment distributions to address
# biological reality in the timing and progression of the prodromal period and
# symptoms. (see add_transitions())  (BT)

# Enable easy movement of individuals between groups (BT)

# Improve plotting style (CH)

# Link to / create ABC functionality to recreate model priors

# Add filtering functionality to be able to filter stochastic trajectories to
# those observed on a given day

# Data import function suite, e.g. for easy incorporation of external data such
# country and age-specific immunity profile used in MMWR

## Align simulations with MMWR, specifically:
# 365 days starting 2/1/2024 with 10k stochastic sims per scenario

# Generate experiment/scenario functionality to run counterfactual scenarios
# of vaccinating earlier or later (eg 3/1/24 or 3/15/24 compared to 3/8/24)

# Ensure calibration functionality to align with: "Outbreak size forecasts were
# calibrated by selecting 100 simulations with the smallest absolute difference
# between predicted and observed daily cumulative measles cases among shelter
# residents."
