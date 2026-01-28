# Adjusting Age Groups

## Introduction

This vignette illustrates how `EpiStandard` can be used to adjust age
groups in the standard population and in study results.

## Standard Population

`EpiStandard` includes four standard populations, which are: - European
Standard Population 2013 - European Standard Population 2013 by Sex -
World Standard Population 2025 - World Standard Population 2025 by Sex

All four of these use the same age groups:

``` r
library(EpiStandard)
library(dplyr)

ageGroups <- standardPopulation(name = "esp2013") 

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

``` r
df_study <- data.frame(state=rep(c('Miami',"Alaska"), c(5,5)),
                       age=rep(c('0-14','15-24','25-44','45-64','65-150'),2),
                       deaths=c(136,57,208,1016,3605,59,18,37,90,81),
                       fu=c(114350,80259,133440,142670,92168,37164,20036,32693,14947,2077))

new_df_study <- mergeAgeGroups(refdata = df_study, newGroups = c("0-24", "25-64", "65-150"),
                               age = "age",
                               pop = "fu",
                               event = "deaths",
                               strata = "state")

new_df_study |> dplyr::glimpse()
#> Rows: 6
#> Columns: 4
#> $ state  <chr> "Alaska", "Miami", "Alaska", "Miami", "Alaska", "Miami"
#> $ age    <chr> "0-24", "0-24", "25-64", "25-64", "65-150", "65-150"
#> $ deaths <dbl> 77, 193, 127, 1224, 81, 3605
#> $ fu     <dbl> 57200, 194609, 47640, 276110, 2077, 92168
```
