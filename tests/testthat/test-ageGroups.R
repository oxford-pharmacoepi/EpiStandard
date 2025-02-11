test_that("load age groups", {

  expect_true(inherits(ageGroups(), "list"))
  expect_true(inherits(ageGroups("esp2013"), "list"))
  expect_true(inherits(ageGroups("wsp2025"), "list"))

  # unsupported datase
  expect_error(ageGroups(name = "another"))

})
