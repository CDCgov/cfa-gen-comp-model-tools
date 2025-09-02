test_that("cross infection arguments filters correctly", {
  basestates <- c("S", "I", "R")

  modelinstructions_base <- define_states(basestates) |>
    add_transition("I", "R", "tau") |>
    add_group(c("group1", "group2"), grouptype = "type1") |>
    add_group(c("group3", "group4"), grouptype = "type2") |>
    combine_groups()

  # ex1
  modelinstructions <- modelinstructions_base |>
    add_infection("I", "S", "I", "beta")
  compiledmodel <- compilemodel(modelinstructions)
  checktbl <- compiledmodel$modelinstructions$tblprocesses |>
    dplyr::filter(processlabel == "interaction")
  expect_equal(nrow(checktbl), 16)

  # ex2
  modelinstructions <- modelinstructions_base |>
    add_infection("I", "S", "I", "beta", groupnames = "group1")
  compiledmodel <- compilemodel(modelinstructions)
  checktbl <- compiledmodel$modelinstructions$tblprocesses |>
    dplyr::filter(processlabel == "interaction")
  expect_equal(nrow(checktbl), 12)

  # ex2_sym
  modelinstructions <- modelinstructions_base |>
    add_infection("I", "S", "I", "beta",
      groupnames = "group1",
      symmetric = FALSE
    )
  compiledmodel <- compilemodel(modelinstructions)
  checktbl <- compiledmodel$modelinstructions$tblprocesses |>
    dplyr::filter(processlabel == "interaction")
  expect_equal(nrow(checktbl), 8)

  # ex3
  modelinstructions <- modelinstructions_base |>
    add_infection("I", "S", "I", "beta",
      groupnames = "group1",
      crossgroupnames = "group2"
    )
  compiledmodel <- compilemodel(modelinstructions)
  checktbl <- compiledmodel$modelinstructions$tblprocesses |>
    dplyr::filter(processlabel == "interaction")
  expect_equal(nrow(checktbl), 8)

  # ex3_sym
  modelinstructions <- modelinstructions_base |>
    add_infection("I", "S", "I", "beta",
      groupnames = "group1",
      crossgroupnames = "group2", symmetric = FALSE
    )
  compiledmodel <- compilemodel(modelinstructions)
  checktbl <- compiledmodel$modelinstructions$tblprocesses |>
    dplyr::filter(processlabel == "interaction")
  expect_equal(nrow(checktbl), 4)

  # ex4
  modelinstructions <- modelinstructions_base |>
    add_infection("I", "S", "I", "beta",
      groupnames = list(type1 = "group1", type2 = "group3"),
      crossgroupnames = list(type1 = "group2", type2 = "group4")
    )
  compiledmodel <- compilemodel(modelinstructions)
  checktbl <- compiledmodel$modelinstructions$tblprocesses |>
    dplyr::filter(processlabel == "interaction")
  expect_equal(nrow(checktbl), 2)

  # ex4_sym
  modelinstructions <- modelinstructions_base |>
    add_infection("I", "S", "I", "beta",
      groupnames = list(type1 = "group1", type2 = "group3"),
      crossgroupnames = list(type1 = "group2", type2 = "group4"),
      symmetric = FALSE
    )
  compiledmodel <- compilemodel(modelinstructions)
  checktbl <- compiledmodel$modelinstructions$tblprocesses |>
    dplyr::filter(processlabel == "interaction")
  expect_equal(nrow(checktbl), 1)

  # ex5
  modelinstructions <- modelinstructions_base |>
    add_infection("I", "S", "I", "beta",
      groupnames = list(type1 = "group1", type2 = "group3"),
      crossgroupnames = list(type1 = "group2")
    )
  compiledmodel <- compilemodel(modelinstructions)
  checktbl <- compiledmodel$modelinstructions$tblprocesses |>
    dplyr::filter(processlabel == "interaction")
  expect_equal(nrow(checktbl), 4)
})
