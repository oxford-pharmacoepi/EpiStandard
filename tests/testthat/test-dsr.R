test_that("dsr gamma", {
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

})

test_that("dsr normal", {
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
                                    method="normal",
                                    sig=0.95,
                                    mp=1000,
                                    decimals=4))

})

test_that("dsr lognormal", {
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
                                    method="lognormal",
                                    sig=0.95,
                                    mp=1000,
                                    decimals=4))

})

test_that("using package populations", {

  # european
  stud_result <- data.frame(
    age_group = c(
      "0 to 4","5 to 9","10 to 14",
      "15 to 19","20 to 24","25 to 29",
      "30 to 34","35 to 39","40 to 44",
      "45 to 49","50 to 54","55 to 59",
      "60 to 64","65 to 69","70 to 74",
      "75 to 79","80 to 84","85 to 89",
      "90 to 150"
    ),
    deaths= rep(5, 19),
    time = rep(100, 19))

  expect_no_error(dsr(data = stud_result,
      event = "deaths", time = "time",
      refdata  = standardPopulation("esp2013")))
})

test_that("using strata", {
  stud_result <- data.frame(
    var_1 = c(rep("a", 19),
              rep("b", 19)),
    age_group = rep(c(
      "0 to 4","5 to 9","10 to 14",
      "15 to 19","20 to 24","25 to 29",
      "30 to 34","35 to 39","40 to 44",
      "45 to 49","50 to 54","55 to 59",
      "60 to 64","65 to 69","70 to 74",
      "75 to 79","80 to 84","85 to 89",
      "90 to 150"
    ),2),
    deaths= c(rep(5, 19),rep(10, 19)),
    time = rep(100, 38))

  expect_no_error(dsr(data = stud_result,
                      event = "deaths", time = "time",
                      strata = "var_1",
                      refdata  = standardPopulation("esp2013")))
})

test_that("using multiple strata", {
  stud_result <- data.frame(
    var_1 = c(rep("a", 19),
              rep("b", 19)),
    var_2 = rep(c("c","d"), 19),
    age_group = rep(c(
      "0 to 4","5 to 9","10 to 14",
      "15 to 19","20 to 24","25 to 29",
      "30 to 34","35 to 39","40 to 44",
      "45 to 49","50 to 54","55 to 59",
      "60 to 64","65 to 69","70 to 74",
      "75 to 79","80 to 84","85 to 89",
      "90 to 150"
    ),2),
    deaths= c(rep(5, 19),rep(10, 19)),
    time = rep(100, 38))

  expect_no_error(dsr(data = stud_result,
                      event = "deaths", time = "time",
                      strata = c("var_1", "var_2"),
                      refdata  = standardPopulation("esp2013")))
})

test_that("error when using invalid method", {

  stud_result <- data.frame(
    age_group = c(
      "0 to 4","5 to 9","10 to 14",
      "15 to 19","20 to 24","25 to 29",
      "30 to 34","35 to 39","40 to 44",
      "45 to 49","50 to 54","55 to 59",
      "60 to 64","65 to 69","70 to 74",
      "75 to 79","80 to 84","85 to 89",
      "90 to 150"
    ),
    deaths= rep(5, 19),
    time = rep(100, 19))

  # european
  expect_error(dsr(data = stud_result,
                      event = "deaths", time = "time",
                      refdata  = standardPopulation("esp2013"),
                      method = "exponential"))



  # world

})

test_that("refdata is dataframe", {

  stud_result <- data.frame(
    age_group = c(
      "0 to 4","5 to 9","10 to 14",
      "15 to 19","20 to 24","25 to 29",
      "30 to 34","35 to 39","40 to 44",
      "45 to 49","50 to 54","55 to 59",
      "60 to 64","65 to 69","70 to 74",
      "75 to 79","80 to 84","85 to 89",
      "90 to 150"
    ),
    deaths= rep(5, 19),
    time = rep(100, 19))

  refdata = c(
    "0 to 4", "5 to 9", "10 to 14",
    "15 to 19","20 to 24","25 to 29",
    "30 to 34","35 to 39","40 to 44",
    "45 to 49","50 to 54","55 to 59",
    "60 to 64","65 to 69","70 to 74",
    "75 to 79","80 to 84","85 to 89",
    "90 to 150"
  )

  expect_error(dsr(data = stud_result,
                   event = "deaths", time = "time",
                   refdata  = refdata))

})

test_that("data is dataframe", {

  stud_result <- c(
    "0 to 4","5 to 9","10 to 14",
    "15 to 19","20 to 24","25 to 29",
    "30 to 34","35 to 39","40 to 44",
    "45 to 49","50 to 54","55 to 59",
    "60 to 64","65 to 69","70 to 74",
    "75 to 79","80 to 84","85 to 89",
    "90 to 150"
  )

  expect_error(dsr(data = stud_result,
                   event = "deaths", time = "time"))

})

test_that("event is column in data", {

  stud_result <- data.frame(
    age_group = c(
      "0 to 4","5 to 9","10 to 14",
      "15 to 19","20 to 24","25 to 29",
      "30 to 34","35 to 39","40 to 44",
      "45 to 49","50 to 54","55 to 59",
      "60 to 64","65 to 69","70 to 74",
      "75 to 79","80 to 84","85 to 89",
      "90 to 150"
    ),
    deaths= rep(5, 19),
    time = rep(100, 19))

  expect_error(dsr(data = stud_result,
                   event = "death_count", time = "time"))

})

test_that("time is column in data", {

  stud_result <- data.frame(
    age_group = c(
      "0 to 4","5 to 9","10 to 14",
      "15 to 19","20 to 24","25 to 29",
      "30 to 34","35 to 39","40 to 44",
      "45 to 49","50 to 54","55 to 59",
      "60 to 64","65 to 69","70 to 74",
      "75 to 79","80 to 84","85 to 89",
      "90 to 150"
    ),
    deaths= rep(5, 19),
    time = rep(100, 19))

  expect_error(dsr(data = stud_result,
                   event = "deaths", time = "time (months)"))

})

test_that("strata is column in data", {

  stud_result <- data.frame(
    age_group = c(
      "0 to 4","5 to 9","10 to 14",
      "15 to 19","20 to 24","25 to 29",
      "30 to 34","35 to 39","40 to 44",
      "45 to 49","50 to 54","55 to 59",
      "60 to 64","65 to 69","70 to 74",
      "75 to 79","80 to 84","85 to 89",
      "90 to 150"
    ),
    deaths= rep(5, 19),
    time = rep(100, 19))

  expect_error(dsr(data = stud_result,
                   event = "deaths", time = "time",
                   strata = "sex"))

})

test_that("strata is column in data", {

  stud_result <- data.frame(
    age_group = c(
      "0 to 4","5 to 9","10 to 14",
      "15 to 19","20 to 24","25 to 29",
      "30 to 34","35 to 39","40 to 44",
      "45 to 49","50 to 54","55 to 59",
      "60 to 64","65 to 69","70 to 74",
      "75 to 79","80 to 84","85 to 89",
      "90 to 150"
    ),
    deaths= rep(5, 19),
    time = rep(100, 19))

  expect_error(dsr(data = stud_result,
                   event = "deaths", time = "time",
                   strata = "sex"))

})

test_that("pop is column in data", {

  stud_result <- data.frame(
    age_group = c(
      "0 to 4","5 to 9","10 to 14",
      "15 to 19","20 to 24","25 to 29",
      "30 to 34","35 to 39","40 to 44",
      "45 to 49","50 to 54","55 to 59",
      "60 to 64","65 to 69","70 to 74",
      "75 to 79","80 to 84","85 to 89",
      "90 to 150"
    ),
    deaths= rep(5, 19),
    time = rep(100, 19))

  expect_error(dsr(data = stud_result,
                   event = "deaths", time = "time",
                   pop = "population"))

})

test_that("age is a column in data and refdata", {

  stud_result <- data.frame(
    age_groups = c(
      "0 to 4","5 to 9","10 to 14",
      "15 to 19","20 to 24","25 to 29",
      "30 to 34","35 to 39","40 to 44",
      "45 to 49","50 to 54","55 to 59",
      "60 to 64","65 to 69","70 to 74",
      "75 to 79","80 to 84","85 to 89",
      "90 to 150"
    ),
    deaths= rep(5, 19),
    time = rep(100, 19))

  expect_error(dsr(data = stud_result,
                   event = "deaths", time = "time",
                   age = "age_group"))

  df_study <- data.frame(state=rep(c('Miami',"Alaska"), c(5,5)),
                         age=rep(c('00-14','15-24','25-44','45-64','65+'),2),
                         deaths=c(136,57,208,1016,3605,59,18,37,90,81),
                         fu=c(114350,80259,133440,142670,92168,37164,20036,32693,14947,2077))

  # US standard population
  df_ref  <- data.frame(ages=c('00-14','15-24','25-44','45-64','65+'),
                        pop=c(23961000,15420000,21353000,19601000,10685000))

  expect_error(dsr(data = df_study,
                   event = "deaths", time = "fu",
                   refdata = df_ref,
                   age = "age"))

})

test_that("same age values in data and refdata", {

  df_study <- data.frame(state=rep(c('Miami',"Alaska"), c(5,5)),
                         age=rep(c('00-14','15-24','25-44','45-64','65+'),2),
                         deaths=c(136,57,208,1016,3605,59,18,37,90,81),
                         fu=c(114350,80259,133440,142670,92168,37164,20036,32693,14947,2077))

  # US standard population
  df_ref  <- data.frame(age=c('00-17','15-24','25-44','45-64','65+'),
                        pop=c(23961000,15420000,21353000,19601000,10685000))

  expect_error(dsr(data = df_study,
                   event = "deaths", time = "fu",
                   refdata = df_ref,
                   age = "age"))


})


