
esp2013 <- readr::read_csv(
  here::here("inst", "european_standard_population.csv"),
  col_types = list(
    AgeGroup = "character",
    EuropeanStandardPopulation = "integer")) |>
  dplyr::rename("age_group" = AgeGroup,
                "pop" = EuropeanStandardPopulation)

wsp2025 <- readr::read_csv(
  here::here("inst", "world_standard_population.csv"),
  col_types = list(
    AgeGroup = "character",
    WorldStandardPopulation = "integer"))|>
  dplyr::rename("age_group" = AgeGroup,
                "pop" = WorldStandardPopulation)

usethis::use_data(esp2013, wsp2025,
                  overwrite = TRUE)
