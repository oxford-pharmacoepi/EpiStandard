test_that("load standard population", {

  expect_true(inherits(standardPopulation(), "data.frame"))
  expect_true(inherits(standardPopulation("esp2013"), "data.frame"))
  expect_true(inherits(standardPopulation("wsp2025"), "data.frame"))
  expect_true(inherits(standardPopulation("espSex2013"), "data.frame"))
  expect_true(inherits(standardPopulation("wspSex2025"), "data.frame"))

  # unsupported datase
  expect_error(standardPopulation(name = "another"))

})
