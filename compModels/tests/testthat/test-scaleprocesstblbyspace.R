test_that("scaleprocesstblbyspace scales processes right", {
  tblprocess <- tibble::tibble(
    rate = c("1", "2", "3"),
    processname = c("inter1", "inter2", "inter3"),
    processgroup = c("grp1", "grp1", "grp2"),
    processlabel = c(
      "interaction",
      "interaction",
      "interaction"
    ),
    metapopulation = c("UK", "USA", "UK")
  )
  tblspace <- tibble::tibble(
    metapopulation = c("UK", "USA"),
    scaleinteractions = c("5*", "10*"),
    scaleprocessbyname = list(
      list(inter1 = "2*", inter3 = "20*"),
      list(inter2 = "15*")
    ),
    scaleprocessbygroup = list(
      list(grp1 = "9*", grp2 = "900*"),
      list(grp2 = "90*")
    )
  )
  scalecol <- "scaleinteractions"
  tblprocess_scaled <- scaleprocesstblbyspace(tblprocess, tblspace, scalecol)
  expect_equal(
    tblprocess_scaled |> dplyr::pull(rate),
    c("2*9*5*1", "15*10*2", "20*900*5*3")
  )
})
