test_that("load standard population", {

  expect_true(inherits(standardPopulation(), "data.frame"))
  expect_true(inherits(standardPopulation("esp2013"), "data.frame"))
  expect_true(inherits(standardPopulation("wsp2025"), "data.frame"))
  expect_true(inherits(standardPopulation("esp2013_by_sex"), "data.frame"))
  expect_true(inherits(standardPopulation("wsp2025_by_sex"), "data.frame"))

  # unsupported datase
  expect_error(standardPopulation(name = "another"))

})
