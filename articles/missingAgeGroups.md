# Dealing with missing age groups

## Introduction

Many epidemiological studies do not look at entire populations. Most
often they will only include adults (18 and over), or age ranges
specific to their outcome of interest. In these cases, age
standardisation can be a bit tricky. This vignette will guide you
through how to deal with missing age groups.

## Specified Populations

Certain studies will specify specific age ranges they want to consider,
depending on the outcome of interest. The function `mergeAgeGroups`
allows you to define the age of your population of interest using the
argument `ageRange`.

``` r
library(EpiStandard)
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
standard_adult <- mergeAgeGroups(standardPopulation("Europe"),
                                 newGroups = c("20 to 29",
                                              "30 to 39",
                                              "40 to 49",
                                              "50 to 59",
                                              "60 to 69",
                                              "70 to 79",
                                              "80 to 89",
                                              "90 to 150"),
                                 ageRange = c(20,150))

standard_adult |> glimpse()
```

    ## Rows: 8
    ## Columns: 2
    ## $ age_group <chr> "20 to 29", "30 to 39", "40 to 49", "50 to 59", "60 to 69", …
    ## $ pop       <int> 12000, 13500, 14000, 13500, 11500, 9000, 4000, 1000

``` r
standard_child <- mergeAgeGroups(standardPopulation("Europe"),
                                 newGroups = c("0 to 9",
                                               "10 to 19"),
                                 ageRange = c(0,19))

standard_child |> glimpse()
```

    ## Rows: 2
    ## Columns: 2
    ## $ age_group <chr> "0 to 9", "10 to 19"
    ## $ pop       <int> 10500, 11000

## Missing Results

Sometimes, there won’t be any counts for certain age groups, but they
are still part of the target population. In these situations you want to
keep all the age groups in the standard population.This can be done
automatically using the argument `addMissingGroups` in
`directlyStandardiseRates`. The purpose of this is to ensure that the
weighting of each age group is proportional to the entire standard
population, and not just a subset of the population. This is not always
advised, and you should consider your overall study objectives before
doing this.

``` r
df_study <- data.frame(country=rep(c('UK',"France"), c(4,4)),
                       age_group=rep(c('15-24','25-44','45-64','65-150'),2),
                       deaths=c(87,413,2316,3425,279,3254,9001,8182),
                       fu=c(80259,133440,142670,92168,20036,32693,14947,2077))
```

So here we see that there are no results for the age group 0-14 in the
results. Firstly, we need to make sure that our age groups in the
standard population match the age groups used in the study.

``` r
standard <- mergeAgeGroups(standardPopulation("Europe"),
                           newGroups =c('0-14','15-24','25-44','45-64','65-150'))

standard |> glimpse()
```

    ## Rows: 5
    ## Columns: 2
    ## $ age_group <chr> "0-14", "15-24", "25-44", "45-64", "65-150"
    ## $ pop       <int> 16000, 11500, 26500, 26500, 19500

Now, when we perform standardisation with this standard population the
0-14 age group with automatically be added to the results data but with
event and denominator values set to 0.

``` r
res <- directlyStandardiseRates(
  data = df_study,
  event = "deaths",
  denominator = "fu",
  strata = c("country"),
  refdata = standard,
  addMissingGroups = TRUE
)

res |> glimpse()
```

    ## Rows: 2
    ## Columns: 9
    ## $ country                      <chr> "UK", "France"
    ## $ deaths                       <dbl> 6241, 20716
    ## $ fu                           <dbl> 448537, 69753
    ## $ crude_rate                   <dbl> 1391.412, 29699.081
    ## $ crude_rate_95CI_lower        <dbl> 1356.892, 29294.656
    ## $ crude_rate_95CI_upper        <dbl> 1425.933, 30103.506
    ## $ standardised_rate            <dbl> 1249.293, 95572.931
    ## $ standardised_rate_95CI_lower <dbl> 1218.224, 93873.604
    ## $ standardised_rate_95CI_upper <dbl> 1280.363, 97272.258

If you do not want to add missing age groups, then you’ll need to set
‘addMissingAgeGroups’ to FALSE. This will remove any age groups from
data that don’t appear in refdata, and vice versa. As a result, The
weights for each age group will not represent the standard population,
but the age range included in the study.

``` r
res <- directlyStandardiseRates(
  data = df_study,
  event = "deaths",
  denominator = "fu",
  strata = "country",
  refdata = standard,
  addMissingGroups = FALSE
)
```

    ## Warning: Removing age groups that don't appear in both data and refdata

``` r
res |> glimpse()
```

    ## Rows: 2
    ## Columns: 9
    ## $ country                      <chr> "UK", "France"
    ## $ deaths                       <dbl> 6241, 20716
    ## $ fu                           <dbl> 448537, 69753
    ## $ crude_rate                   <dbl> 1391.412, 29699.081
    ## $ crude_rate_95CI_lower        <dbl> 1356.892, 29294.656
    ## $ crude_rate_95CI_upper        <dbl> 1425.933, 30103.506
    ## $ standardised_rate            <dbl> 1487.254, 113777.299
    ## $ standardised_rate_95CI_lower <dbl> 1450.267, 111754.291
    ## $ standardised_rate_95CI_upper <dbl> 1524.242, 115800.307

As we can see, the standard rates vary based on whether
`addMissingGroups` is TRUE / FALSE, while the crude rates remain the
same.
