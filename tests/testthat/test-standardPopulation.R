test_that("load standard population", {

  expect_true(inherits(standardPopulation(), "data.frame"))
  expect_true(inherits(standardPopulation("Europe"), "data.frame"))
  expect_true(inherits(standardPopulation("World"), "data.frame"))

  # unsupported datase
  expect_error(standardPopulation(region = "Asia"))

})
