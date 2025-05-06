test_that("load age groups", {

  expect_true(inherits(ageGroups(), "character"))
  expect_true(inherits(ageGroups("esp2013"), "character"))
  expect_true(inherits(ageGroups("wsp2025"), "character"))

  # unsupported datase
  expect_error(ageGroups(name = "another"))

})
