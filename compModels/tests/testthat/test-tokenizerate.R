test_that("tokenizerate splits strings appropriately", {
  expect_equal(length(tokenizerate("this*is/a^test")), 7)
})
