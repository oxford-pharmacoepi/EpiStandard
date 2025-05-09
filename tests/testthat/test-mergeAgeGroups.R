test_that("merge example", {
  # example from original dsr package
  # An example of calculating directly standardized rates
  # Data from Table 1, Page 132 of Schoenbach (2000)

  # State specific death counts and fu

  # US standard population
  df_ref  <- data.frame(age_group=c('0-14','15-24','25-44','45-64','65-80'),
                        pop=c(23961000,15420000,21353000,19601000,10685000))

  # Directly Standardized Rates (per 1000) - 95% CI's using the gamma method
  expect_no_error(my_results <- mergeAgeGroups(df_ref, c("0-24", "25-64", "65-80")))
  expect_true(inherits(my_results, "data.frame"))

})

test_that("age group format error", {

  df_ref  <- data.frame(age_group=c('0-14','15-24','25-44','45-64','65-80'),
                        pop=c(23961000,15420000,21353000,19601000,10685000))

  # Directly Standardized Rates (per 1000) - 95% CI's using the gamma method
  expect_error(my_results <- mergeAgeGroups(df_ref, c("0-15", "16-64", "65-80")))

})
