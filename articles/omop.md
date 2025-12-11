# Using EpiStandards

## Introduction

In this vignette, we will explore how the EpiStandard functions can be
used with results produced from the DARWIN package IncidencePrevalence.

### Create CDM

``` r
library(EpiStandard)
library(IncidencePrevalence)

cdm <- IncidencePrevalence::mockIncidencePrevalence(
  sampleSize = 10000,
  outPre = 0.25
)

cdm <- IncidencePrevalence::generateDenominatorCohortSet(
  cdm = cdm,
  name = "denominator",
  cohortDateRange = as.Date(c("2008-01-01", "2020-01-01")),
  ageGroup = list(c(0, 19), c(20, 64), c(65, 150)),
  sex = c("Male", "Female"),
  daysPriorObservation = 0
)
```

``` r
inc <- IncidencePrevalence::estimateIncidence(
  cdm = cdm,
  denominatorTable = "denominator",
  outcomeTable = "outcome",
  interval = "years",
  outcomeWashout = 0,
  repeatedEvents = FALSE
)
```

``` r
inc_tidy <- inc |>
  omopgenerics::addSettings() |>
  omopgenerics::splitAdditional() |>
  omopgenerics::splitGroup() |>
  dplyr::select(cdm_name, outcome_cohort_name, estimate_name, estimate_value, denominator_age_group, denominator_sex, incidence_start_date) |>
  dplyr::filter(estimate_name %in% c("person_years", "outcome_count")) |>
  dplyr::mutate(estimate_value = ifelse(estimate_value == "-", 4, estimate_value)) |>
  tidyr::pivot_wider(names_from = estimate_name, values_from = estimate_value) |>
  dplyr::rename(age_group = denominator_age_group) |>
  dplyr::mutate(outcome_count = as.numeric(outcome_count),
                person_years = as.numeric(person_years))
```

``` r
standardPop <- mergeAgeGroups(standardPopulation(), newGroups = c("0 to 19", "20 to 64", "65 to 150"))
```

``` r
standardInc <- dsr(data = inc_tidy,
                   refdata = standardPop,
                   event = "outcome_count",
                   denominator = "person_years",
                   age = "age_group",
                   pop = "pop",
                   strata = c("incidence_start_date", "denominator_sex", "outcome_cohort_name")
                   )
```

How to deal with minimum cell counts?
