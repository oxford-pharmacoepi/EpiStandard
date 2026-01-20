# Standardise OMOP results

## Introduction

This vignette illustrates how `EpiStandard` can be used to standardise
results from the `IncidencePrevalence` package, a DARWIN-EU package for
estimating incidence and prevalence from data mapped to the OMOP Common
Data Model.

### Set-up

We first load the relevant packages and then create a mock
OMOP-formatted dataset containing a population and an outcome of
interest.

``` r
library(EpiStandard)
library(IncidencePrevalence)
library(omopgenerics)
library(dplyr)

cdm <- mockIncidencePrevalence(
  sampleSize = 10000,
  outPre = 0.25
)
```

### Estimate Incidence Rates

We use the IncidencePrevalence package to generate a denominator cohort
with age and sex stratifications, and then calculate overall outcome
incidence and incidence by calendar year for each stratum combination.
For more information on how to use this package, refer to its
[website](https://darwin-eu.github.io/IncidencePrevalence/).

Notice that the results are stored as a
[summarised_result](https://darwin-eu.github.io/omopgenerics/articles/summarised_result.html)
object.

``` r
cdm <- generateDenominatorCohortSet(
  cdm = cdm,
  name = "denominator",
  cohortDateRange = as.Date(c("2008-01-01", "2020-01-01")),
  ageGroup = list(c(0, 19), c(20, 64), c(65, 150), c(0, 150)),
  sex = c("Male", "Female"),
  daysPriorObservation = 0
)

inc <- estimateIncidence(
  cdm = cdm,
  denominatorTable = "denominator",
  outcomeTable = "outcome",
  interval = c("years", "overall"),
  outcomeWashout = 0,
  repeatedEvents = FALSE
)

inc |> glimpse()
#> Rows: 1,049
#> Columns: 13
#> $ result_id        <int> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,…
#> $ cdm_name         <chr> "mock", "mock", "mock", "mock", "mock", "mock", "mock…
#> $ group_name       <chr> "denominator_cohort_name &&& outcome_cohort_name", "d…
#> $ group_level      <chr> "denominator_cohort_8 &&& cohort_1", "denominator_coh…
#> $ strata_name      <chr> "overall", "overall", "overall", "overall", "overall"…
#> $ strata_level     <chr> "overall", "overall", "overall", "overall", "overall"…
#> $ variable_name    <chr> "Denominator", "Outcome", "Denominator", "Denominator…
#> $ variable_level   <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
#> $ estimate_name    <chr> "denominator_count", "outcome_count", "person_days", …
#> $ estimate_type    <chr> "integer", "integer", "numeric", "numeric", "numeric"…
#> $ estimate_value   <chr> "277", "14", "85765", "234.812", "5962.217", "3259.59…
#> $ additional_name  <chr> "incidence_start_date &&& incidence_end_date &&& anal…
#> $ additional_level <chr> "2008-01-01 &&& 2008-12-31 &&& years", "2008-01-01 &&…
```

To use `EpiStandard`, we first filter the incidence results to the age
groups of interest for standardisation and then convert the results into
a format that facilitates further manipulation.

### Standardise Incidence Rates

``` r
incidenceTidy <- inc |>
  filterSettings(denominator_age_group %in% c("0 to 19", "20 to 64", "65 to 150")) |>
  asIncidenceResult()

incidenceTidy |> glimpse()
#> Rows: 69
#> Columns: 26
#> $ cdm_name                             <chr> "mock", "mock", "mock", "mock", "…
#> $ denominator_cohort_name              <chr> "denominator_cohort_2", "denomina…
#> $ outcome_cohort_name                  <chr> "cohort_1", "cohort_1", "cohort_1…
#> $ incidence_start_date                 <date> 2008-01-01, 2009-01-01, 2010-01-…
#> $ incidence_end_date                   <date> 2008-12-31, 2009-12-31, 2010-12-…
#> $ analysis_interval                    <chr> "years", "years", "years", "years…
#> $ analysis_censor_cohort_name          <chr> "None", "None", "None", "None", "…
#> $ analysis_complete_database_intervals <chr> "TRUE", "TRUE", "TRUE", "TRUE", "…
#> $ analysis_outcome_washout             <chr> "0", "0", "0", "0", "0", "0", "0"…
#> $ analysis_repeated_events             <chr> "FALSE", "FALSE", "FALSE", "FALSE…
#> $ denominator_age_group                <chr> "0 to 19", "0 to 19", "0 to 19", …
#> $ denominator_days_prior_observation   <chr> "0", "0", "0", "0", "0", "0", "0"…
#> $ denominator_end_date                 <date> 2020-01-01, 2020-01-01, 2020-01-…
#> $ denominator_requirements_at_entry    <chr> "FALSE", "FALSE", "FALSE", "FALSE…
#> $ denominator_sex                      <chr> "Female", "Female", "Female", "Fe…
#> $ denominator_start_date               <date> 2008-01-01, 2008-01-01, 2008-01-…
#> $ denominator_target_cohort_name       <chr> "None", "None", "None", "None", "…
#> $ denominator_time_at_risk             <chr> "0 to Inf", "0 to Inf", "0 to Inf…
#> $ denominator_count                    <int> 38, 44, 38, 23, 16, 14, 8, 7, 4, …
#> $ outcome_count                        <int> 0, 1, 1, 0, 1, 0, 0, 0, 0, 3, 3, …
#> $ person_days                          <dbl> 12529, 12754, 10404, 7081, 5510, …
#> $ person_years                         <dbl> 34.303, 34.919, 28.485, 19.387, 1…
#> $ incidence_100000_pys                 <dbl> 0.000, 2863.770, 3510.620, 0.000,…
#> $ incidence_100000_pys_95CI_lower      <dbl> 0.000, 72.504, 88.881, 0.000, 167…
#> $ incidence_100000_pys_95CI_upper      <dbl> 10753.810, 15955.908, 19559.921, …
#> $ result_type                          <chr> "tidy_incidence", "tidy_incidence…
```

Now that the incidence results are in the correct format, we prepare the
reference population. In this example, we use the European Standard
Population (`esp2013`). As this population uses 5-year age bands, we
merge these groups to match those used to estimate incidence.

``` r
standardPop <- mergeAgeGroups(
  standardPopulation("esp2013"),
  newGroups = c("0 to 19", "20 to 64", "65 to 150")
) |>
  rename("denominator_age_group" = "age_group")
standardPop |> glimpse()
#> Rows: 3
#> Columns: 2
#> $ denominator_age_group <chr> "0 to 19", "20 to 64", "65 to 150"
#> $ pop                   <int> 21500, 59000, 19500
```

Finally, we use the
[`dsr()`](https://github.com/oxford-pharmacoepi/EpiStandard/reference/dsr.md)
function to standardise the incidence results to the reference
population. We specify the columns containing the event counts,
person-years, and population weights, as well as the column identifying
age groups. Since we want to standardise within each outcome, sex, and
calendar-time stratum, we use the `strata` argument as follows:

``` r
standardInc <- dsr(
  data = incidenceTidy,
  refdata = standardPop,
  event = "outcome_count",
  denominator = "person_years",
  age = "denominator_age_group",
  pop = "pop",
  strata = c("incidence_start_date", "denominator_sex", "analysis_interval", "outcome_cohort_name")
)
standardInc |> glimpse()
#> Rows: 26
#> Columns: 12
#> $ incidence_start_date    <date> 2008-01-01, 2009-01-01, 2010-01-01, 2011-01-0…
#> $ denominator_sex         <chr> "Female", "Female", "Female", "Female", "Femal…
#> $ analysis_interval       <chr> "years", "years", "years", "years", "years", "…
#> $ outcome_cohort_name     <chr> "cohort_1", "cohort_1", "cohort_1", "cohort_1"…
#> $ Numerator               <int> 14, 10, 14, 3, 4, 1, 1, 3, 1, 51, 13, 14, 9, 1…
#> $ Denominator             <dbl> 234.813, 233.057, 209.706, 160.778, 132.954, 1…
#> $ `Crude Rate (per 1000)` <dbl> 59.6219, 42.9080, 66.7601, 18.6593, 30.0856, 9…
#> $ `95% LCL (Crude)`       <dbl> 28.3906, 16.3138, 31.7897, -2.4553, 0.6023, -9…
#> $ `95% UCL (Crude)`       <dbl> 90.8532, 69.5021, 101.7306, 39.7738, 59.5689, …
#> $ `Std Rate (per 1000)`   <dbl> 60.2832, 42.7653, 68.6842, 17.3819, 30.7820, 1…
#> $ `95% LCL (Std)`         <dbl> 27.9721, 15.2756, 31.5812, -3.0871, -3.6202, -…
#> $ `95% UCL (Std)`         <dbl> 92.5942, 70.2551, 105.7872, 37.8510, 65.1843, …
```
