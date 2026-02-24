# Calculate directly standardised rates

Computes crude and directly standardised rates. Rates can be stratified
by variables of interest.

## Usage

``` r
directlyStandardiseRates(
  data,
  event,
  denominator,
  age = "age_group",
  pop = "pop",
  strata = NULL,
  addMissingGroups = TRUE,
  refdata = standardPopulation("Europe")
)
```

## Arguments

- data:

  A data frame with the event counts to be standardised.

- event:

  Name of the column in data that corresponds to the event counts.

- denominator:

  Name of the column in data that corresponds to the denominator
  population (in person-time, e.g person-days, person-years etc).

- age:

  Name of the column in data and refdata that corresponds to age groups.

- pop:

  Name of the column in refdata that corresponds to the standard
  population in each age group.

- strata:

  Name of the columns in data for which rates are calculated by.

- addMissingGroups:

  If TRUE, any age groups present in refdata but not in data will be
  added and set to 0. If false, these age groups will be removed from
  refdata.

- refdata:

  A data frame representing the standard population. It must contain two
  columns: age, with the different age groups (notice that this column
  name must be the same as in data, defined by the input age); and pop,
  with the number of individuals in each corresponding age group.

## Value

Data frame with crude and standardised rates.

## Examples

``` r
# An example of calculating directly standardised rates
# Data example is from Table 1 (p.132) of Fundamentals of Epidemiology by Schoenbach, 2000.

# The following table shows the number of deaths, for 5 different age groups,
# in the states of Miami and Alaska:
data <- data.frame(
      state = rep(c('Miami',"Alaska"), c(5,5)),
      age_groups = rep(c('00-14','15-24','25-44','45-64','65+'),2),
      deaths = c(136, 57, 208, 1016, 3605, 59, 18, 37, 90, 81),
      general_population = c(114350,80259,133440,142670,92168,37164,20036,32693,14947,2077))

# We aim to standardise the number of deaths per each state. To do that, we will use the following
# US standard population:
standardised_population <- data.frame(
                            age_groups = c('00-14','15-24','25-44','45-64','65+'),
                            pop = c(23961000,15420000,21353000,19601000,10685000))

# Now we will use the function dsr to calculate the direct standardised rates
# (per 1000 individuals) using a 95% CI calculated by the gamma method:
my_results <- directlyStandardiseRates(data = data,
                  event = "deaths",
                  denominator  = "general_population",
                  age   = "age_groups",
                  pop   = "pop",
                  strata = "state",
                  refdata = standardised_population)
# View results
my_results
#> # A tibble: 2 × 9
#>   state  deaths general_population crude_rate crude_rate_95CI_lower
#>   <chr>   <dbl>              <dbl>      <dbl>                 <dbl>
#> 1 Miami    5022             562887       892.                  868.
#> 2 Alaska    285             106917       267.                  236.
#> # ℹ 4 more variables: crude_rate_95CI_upper <dbl>, standardised_rate <dbl>,
#> #   standardised_rate_95CI_lower <dbl>, standardised_rate_95CI_upper <dbl>
```
