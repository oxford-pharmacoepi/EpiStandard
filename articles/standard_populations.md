# Standard Populations

## Introduction

In this vignette, we will explore the EpiStandard functions designed to
retrieve or create a standard population data table. There are two
functions to help with this:

- [`standardPopulation()`](https://github.com/oxford-pharmacoepi/EpiStandard/reference/standardPopulation.md)
- [`mergeAgeGroups()`](https://github.com/oxford-pharmacoepi/EpiStandard/reference/mergeAgeGroups.md)

### Retrieve standard population data

The package contains two standard population data tables:

- ESP2013:
- WSP2025:

``` r
library(EpiStandard)
library(gt)

pop <- standardPopulation("esp2013")
gt(pop)
```

| age_group | pop  |
|-----------|------|
| 0 to 4    | 5000 |
| 5 to 9    | 5500 |
| 10 to 14  | 5500 |
| 15 to 19  | 5500 |
| 20 to 24  | 6000 |
| 25 to 29  | 6000 |
| 30 to 34  | 6500 |
| 35 to 39  | 7000 |
| 40 to 44  | 7000 |
| 45 to 49  | 7000 |
| 50 to 54  | 7000 |
| 55 to 59  | 6500 |
| 60 to 64  | 6000 |
| 65 to 69  | 5500 |
| 70 to 74  | 5000 |
| 75 to 79  | 4000 |
| 80 to 84  | 2500 |
| 85 to 89  | 1500 |
| 90 to 150 | 1000 |

You can create new age groups using the ‘mergeAgeGroups()’ function.
This allows you to combine the age groups in the standard population.

``` r
merged_data <- mergeAgeGroups(pop, newGroups = c("0-19", "20-64", "65-150"))
gt(merged_data)
```

| age_group | pop   |
|-----------|-------|
| 0-19      | 21500 |
| 20-64     | 21500 |
| 65-150    | 21500 |
