# Live SIR demo code:








# Measles model draft replicate the model development and simulation work done
# during the Chicago Measles 2024 response.
# Catherine Herzog & Bradford # Taylor.

# Background
# https://www.sciencedirect.com/science/article/pii/S2468266723001305?via%3Dihub#sec1 # nolint
# https://www.cdc.gov/mmwr/volumes/73/wr/mm7319a2.htm
# https://github.com/CDCgov/measles-model-chicago-2024/tree/main

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
load_all("compModels")

# Measles Models Details:
# Both the OAW study and Chicago Measles 2024 response used adaptive tau-leaping
# Intervention:  vaccination
# We follow the equations in the supplement (pg 8) of Masters et al 2023 on OAW
# compartment Sv contains once vaccinated individuals who remained susceptible
# (primary vaccine failure) and who could contribute to transmission but who
# were not vaccinated a second time during the initial OAW mass vaccination
# campaigns.
# 𝑑𝑆𝑖(𝑡)/𝑑𝑡 = −𝛽𝐼(𝑡)𝑆𝑖(𝑡) − 𝜃(𝑡)𝑆𝑖(𝑡)
# 𝑑𝑆𝑣𝑖(𝑡)/𝑑𝑡 = 𝑝 ∗ 𝜃(𝑡)𝑆𝑣𝑖(𝑡) −𝛽𝐼(𝑡)𝑆𝑣𝑖(𝑡)
# 𝑑𝐸𝑖(𝑡)/𝑑𝑡 = 𝛽𝐼(𝑡)𝑆𝑖(𝑡) + 𝛽𝐼(𝑡)𝑆𝑣𝑖(𝑡)− 𝜎𝐸𝑖(𝑡)
# 𝑑𝐼𝑖(𝑡)/𝑑𝑡 = 𝜎𝐸𝑖 (𝑡) − 𝛾𝐼𝑖(𝑡)
# 𝑑𝑅𝑖(𝑡)/𝑑𝑡 = 𝛾𝐼𝑖(𝑡)+ (1 − 𝑝) ∗ 𝜃(𝑡)𝑆𝑖(𝑡)
# where 𝐼(𝑡) = ∑𝐼𝑖(𝑡) and the force of infection, 𝜆(𝑡) = 𝛽𝐼(𝑡)

# Original OAW model there were 5 groups based on eligibility for the vaccine:
# < 6 months, 6-11 months, 1-11 years, >= 12 years (not pregnant) and
# >= 12 years (pregnant). Those < 6 months and >=12 years (pregnant) did not
# receive MMR vaccination. Homogenous mixing was assumed given the shelter
# conditions.

# Measles Model: SE2IRV with groups
base_states <- c("S", "E", "I", "R", "V")
measlesmodel <- define_states(base_states) |>
  add_infection("I", "S", "E", "beta") |>
  add_transition("I", "R", "tau") |>
  add_transition("E", "I", "taue", chainlength = 2) |>
  add_transition("S", c("V", "R"), "1/(theta)",
    forkprobability = c("p", "1-p"),
    groupname = "1"
  ) |>
  add_infection("I", "V", "E", "beta") |>
  add_group(as.character(1:5))

measlescompiled <- compilemodel(measlesmodel)

measles_rates <- measlescompiled$modeloutstructions$processrates
measles_peter <- measlescompiled$modeloutstructions$petermatrix
measles_states <- measlescompiled$modeloutstructions$updatedstates
measles_rates
measles_peter
measles_states

# Simulation and Plotting
dyn <- wrap_adaptivetau(
  c("S" = 999, "I" = 1, "R" = 0, "E" = 0, "V" = 0),
  measlescompiled,
  rate_func = NULL, # defaults to compModels::generalized_rates()
  c(beta = 2, tau = 1, taue = .5, theta = .01, p = 0.4),
  25,
  10,
  "adaptivetau"
)
plot_stoch_model(dyn,
  compartments = measles_states,
  colors =
    colorRampPalette(RColorBrewer::brewer.pal(8, "Dark2"))(30)
)
