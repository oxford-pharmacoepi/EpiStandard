test_that("omopTidy example", {
  # example from original dsr package
  # An example of calculating directly standardized rates
  # Data from Table 1, Page 132 of Schoenbach (2000)

  # State specific death counts and fu
  df_study <- data.frame(state=rep(c('Miami',"Alaska"), c(5,5)),
                         age=rep(c('00-14','15-24','25-44','45-64','65+'),2),
                         deaths=c(136,57,208,1016,3605,59,18,37,90,81),
                         fu=c(114350,80259,133440,142670,92168,37164,20036,32693,14947,2077))


  # Directly Standardized Rates (per 1000) - 95% CI's using the gamma method
  expect_error(my_results <- omopTidy(df_study))

})
