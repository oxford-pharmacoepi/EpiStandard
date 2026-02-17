# Adjusting Age Groups

## Introduction

This vignette illustrates how `EpiStandard` can be used to adjust age
groups in the standard population and in study results.

## Standard Population

`EpiStandard` includes two standard populations, which are: - European
Standard Population 2013 - World Standard Population 2025

Both use the same age groups, and can be used by using the function
`standardPopulation`. You can choose which of these to use by setting
the argument `region` to ‘Europe’ or ‘World’.

``` r
library(EpiStandard)
library(dplyr)

ageGroups <- standardPopulation(region = "Europe") 

ageGroups |>
  pull(age_group)
#>  [1] "0 to 4"    "5 to 9"    "10 to 14"  "15 to 19"  "20 to 24"  "25 to 29" 
#>  [7] "30 to 34"  "35 to 39"  "40 to 44"  "45 to 49"  "50 to 54"  "55 to 59" 
#> [13] "60 to 64"  "65 to 69"  "70 to 74"  "75 to 79"  "80 to 84"  "85 to 89" 
#> [19] "90 to 150"
```

However, some studies might use different age groups which are not
compatible with the standard populations. This can be solved by using
the function
[`mergeAgeGroups()`](https://github.com/oxford-pharmacoepi/EpiStandard/reference/mergeAgeGroups.md).For
example, if a study only uses the age groups ‘0-19’, ‘20-64’ and ‘65 to
150’, the standard population can be adjusted to match

``` r
newAgeGroups <- mergeAgeGroups(refdata = ageGroups, newGroups = c("0-19", "20-64", "65-150"))

newAgeGroups
#> # A tibble: 3 × 2
#>   age_group   pop
#>   <chr>     <int>
#> 1 0-19      21500
#> 2 20-64     59000
#> 3 65-150    19500
```

This will also work if using a bespoke standard population.

``` r
df_study <- data.frame(age=c('0-14','15-24','25-44','45-64','65-150'),
                       pop=c(114350,80259,133440,142670,92168))

new_df_study <- mergeAgeGroups(refdata = df_study, newGroups = c("0-24", "25-64", "65-150"),
                               age = "age",
                               pop = "pop")

new_df_study |> dplyr::glimpse()
#> Rows: 3
#> Columns: 2
#> $ age <chr> "0-24", "25-64", "65-150"
#> $ pop <dbl> 194609, 276110, 92168
```

## Study Results

Additionally, you can adjust your study results to merge age groups,
while taking into consideration additional stratifications of interest.
For example, the data set below shows study results for the UK and
France. If we want merge some age groups, but still look at each country
separately, we can use the argument `strata`.

``` r
df_study <- data.frame(country=rep(c('UK',"France"), c(5,5)),
                       age=rep(c('0-14','15-24','25-44','45-64','65-150'),2),
                       deaths=c(132,87,413,2316,3425,605,279,3254,9001,8182),
                       fu=c(114350,80259,133440,142670,92168,37164,20036,32693,14947,2077))

new_df_study <- mergeAgeGroups(refdata = df_study, newGroups = c("0-24", "25-64", "65-150"),
                               age = "age",
                               pop = "fu",
                               event = "deaths",
                               strata = "country")

new_df_study |> dplyr::glimpse()
#> Rows: 6
#> Columns: 4
#> $ country <chr> "France", "UK", "France", "UK", "France", "UK"
#> $ age     <chr> "0-24", "0-24", "25-64", "25-64", "65-150", "65-150"
#> $ deaths  <dbl> 884, 219, 12255, 2729, 8182, 3425
#> $ fu      <dbl> 57200, 194609, 47640, 276110, 2077, 92168
```

Note: All data used in this vignette is artificial.
