# Create new merged age groups

Create new merged age groups

## Usage

``` r
mergeAgeGroups(
  refdata,
  newGroups,
  event = NULL,
  age = "age_group",
  pop = "pop",
  strata = NULL
)
```

## Arguments

- refdata:

  Standard population dataset you want to use.

- newGroups:

  Create a list of new age groups you want to create.

- event:

  Column in refdata with outcome counts.

- age:

  Column in refdata with age values.

- pop:

  Column in refdata with population counts, preferably in person-time.

- strata:

  Column or columns to stratify by.

## Value

Table

## Examples

``` r
# \donttest{

refdata <- standardPopulation(name = "esp2013")

refdata |> dplyr::glimpse()
#> Rows: 19
#> Columns: 2
#> $ age_group <chr> "0 to 4", "5 to 9", "10 to 14", "15 to 19", "20 to 24", "25 …
#> $ pop       <int> 5000, 5500, 5500, 5500, 6000, 6000, 6500, 7000, 7000, 7000, …

merged_data <- mergeAgeGroups(refdata, c("0-19", "20-64", "65-150"))
#> Warning: Unknown or uninitialised column: `age_high`.
#> Warning: Unknown or uninitialised column: `age_low`.

merged_data |> dplyr::glimpse()
#> Rows: 3
#> Columns: 2
#> $ age_group <chr> "0-19", "20-64", "65-150"
#> $ pop       <int> 21500, 21500, 21500

# }
```
