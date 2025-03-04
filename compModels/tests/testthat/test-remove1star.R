test_that("remove1star removes 1* and nothing else", {
  teststr1 <- "1*"
  teststr2 <- "1*1*2"
  teststr3 <- "3*1*1*2"
  teststr4 <- "2.1*"
  teststr5 <- "1*(a+complicated)*1.1+2.51+exp(1*1)+function*1*1"
  expect_equal(remove1star(teststr1), "")
  expect_equal(remove1star(teststr2), "2")
  expect_equal(remove1star(teststr3), "3*2")
  expect_equal(remove1star(teststr4), "2.1*")
  expect_equal(
    remove1star(teststr5),
    "(a+complicated)*1.1+2.51+exp(1)+function*1"
  )
})
