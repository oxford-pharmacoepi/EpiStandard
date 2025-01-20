
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

esp2013_by_sex <- readr::read_csv(
  here::here("inst", "european_standard_population_by_sex.csv"),
  col_types = list(
    AgeGroup = "character",
    Sex = "character",
    EuropeanStandardPopulation = "integer")) |>
  dplyr::rename("age_group" = AgeGroup,
                "sex" = Sex,
                "pop" = EuropeanStandardPopulation)

esp2013_by_sex <- esp2013_by_sex[, !names(esp2013_by_sex) %in% "_id"]

wsp2025_by_sex <- readr::read_csv(
  here::here("inst", "world_standard_population_by_sex.csv"),
  col_types = list(
    AgeGroup = "character",
    Sex = "character",
    WorldStandardPopulation = "integer"))|>
  dplyr::rename("age_group" = AgeGroup,
                "sex" = Sex,
                "pop" = WorldStandardPopulation)

wsp2025_by_sex <- wsp2025_by_sex[, !names(wsp2025_by_sex) %in% "_id"]

usethis::use_data(esp2013, wsp2025,
                  esp2013_by_sex, wsp2025_by_sex,
                  overwrite = TRUE)
