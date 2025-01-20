test_that("dsr example", {
  # example from original dsr package
  # An example of calculating directly standardized rates
  # Data from Table 1, Page 132 of Schoenbach (2000)

  # State specific death counts and fu
  df_study <- data.frame(state=rep(c('Miami',"Alaska"), c(5,5)),
                        age=rep(c('00-14','15-24','25-44','45-64','65+'),2),
                        deaths=c(136,57,208,1016,3605,59,18,37,90,81),
                        fu=c(114350,80259,133440,142670,92168,37164,20036,32693,14947,2077))

  # US standard population
  df_ref  <- data.frame(age=c('00-14','15-24','25-44','45-64','65+'),
                       pop=c(23961000,15420000,21353000,19601000,10685000))

  # Directly Standardized Rates (per 1000) - 95% CI's using the gamma method
  expect_no_error(my_results <- dsr(data=df_study,
                   event="deaths",
                   time="fu",
                   strata="state",
                   age = "age",
                   refdata=df_ref,
                   method="gamma",
                   sig=0.95,
                   mp=1000,
                   decimals=4))
  expect_true(inherits(my_results, "data.frame"))

  expect_error(my_results <- dsr(data=df_study,
                                    event="deaths",
                                    time="fu",
                                    strata="state",
                                    age = NULL,
                                    refdata=df_ref,
                                    method="gamma",
                                    sig=0.95,
                                    mp=1000,
                                    decimals=4))

  expect_error(my_results <- dsr(data=df_study,
                                 event="deaths",
                                 time="fu",
                                 strata="state",
                                 age = "age_group",
                                 refdata=df_ref,
                                 method="gamma",
                                 sig=0.95,
                                 mp=1000,
                                 decimals=4))

  expect_true(inherits(my_results, "data.frame"))

  # ADD EXPECTED VALUES

})

test_that("using package populations", {

  # european
  stud_result <- data.frame(
    age_group = c(
      "0-4 years", "5-9 years", "10-14 years",
      "15-19 years", "20-24 years", "25-29 years",
      "30-34 years", "35-39 years", "40-44 years",
      "45-49 years", "50-54 years", "55-59 years",
      "60-64 years", "65-69 years", "70-74 years",
      "75-79 years","80-84 years","85-89 years",
      "90plus years"
    ),
    deaths= rep(5, 19),
    time = rep(100, 19))
  expect_no_error(dsr(data = stud_result,
      event = "deaths", time = "time",
      refdata  = standardPopulation("esp2013")))

  stud_result <- data.frame(
    var_1 = c(rep("a", 19),
              rep("b", 19)),
    age_group = rep(c(
      "0-4 years", "5-9 years", "10-14 years",
      "15-19 years", "20-24 years", "25-29 years",
      "30-34 years", "35-39 years", "40-44 years",
      "45-49 years", "50-54 years", "55-59 years",
      "60-64 years", "65-69 years", "70-74 years",
      "75-79 years","80-84 years","85-89 years",
      "90plus years"
    ),2),
    deaths= c(rep(5, 19),rep(10, 19)),
    time = rep(100, 38))
  expect_no_error(dsr(data = stud_result,
                      event = "deaths", time = "time",
                      strata = "var_1",
                      refdata  = standardPopulation("esp2013")))
  stud_result <- data.frame(
    var_1 = c(rep("a", 19),
              rep("b", 19)),
    var_2 = rep(c("c","d"), 19),
    age_group = rep(c(
      "0-4 years", "5-9 years", "10-14 years",
      "15-19 years", "20-24 years", "25-29 years",
      "30-34 years", "35-39 years", "40-44 years",
      "45-49 years", "50-54 years", "55-59 years",
      "60-64 years", "65-69 years", "70-74 years",
      "75-79 years","80-84 years","85-89 years",
      "90plus years"
    ),2),
    deaths= c(rep(5, 19),rep(10, 19)),
    time = rep(100, 38))
  expect_no_error(dsr(data = stud_result,
                      event = "deaths", time = "time",
                      strata = c("var_1", "var_2"),
                      refdata  = standardPopulation("esp2013")))



  # world

})

test_that("standardise by age and sex", {

  # european
  stud_result <- data.frame(
    age_group = rep(c(
      "0-4 years", "5-9 years", "10-14 years",
      "15-19 years", "20-24 years", "25-29 years",
      "30-34 years", "35-39 years", "40-44 years",
      "45-49 years", "50-54 years", "55-59 years",
      "60-64 years", "65-69 years", "70-74 years",
      "75-79 years","80-84 years","85-89 years",
      "90plus years"
    ),2),
    sex = rep(c("Male", "Female"), c(19,19)),
    deaths= rep(5, 19),
    time = rep(100, 19))

  expect_no_error(dsr(data = stud_result,
                      event = "deaths", time = "time",
                      sex = "sex",
                      refdata  = standardPopulation("esp2013_by_sex")))

  expect_no_error(dsr(data = stud_result,
                      event = "deaths", time = "time",
                      sex = "sex",
                      refdata  = standardPopulation("esp2013_by_sex")))

  stud_result <- data.frame(
    var_1 = c(rep("a", 38),
              rep("b", 38)),
    age_group = rep(c(
      "0-4 years", "5-9 years", "10-14 years",
      "15-19 years", "20-24 years", "25-29 years",
      "30-34 years", "35-39 years", "40-44 years",
      "45-49 years", "50-54 years", "55-59 years",
      "60-64 years", "65-69 years", "70-74 years",
      "75-79 years","80-84 years","85-89 years",
      "90plus years"
    ),4),
    sex = rep(c("Male", "Female", "Male", "Female"), c(19,19,19,19)),
    deaths= rep(c(rep(5, 19),rep(10, 19)),2),
    time = rep(rep(100, 38),2))

  expect_no_error(dsr(data = stud_result,
                      event = "deaths", time = "time",
                      sex = "sex",
                      strata = "var_1",
                      refdata  = standardPopulation("esp2013_by_sex")))

  expect_warning(dsr(data = stud_result,
                      event = "deaths", time = "time",
                      sex = "gender",
                      strata = "var_1",
                      refdata  = standardPopulation("esp2013_by_sex")))

  # world

})
