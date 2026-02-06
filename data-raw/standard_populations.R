
esp2013 <- readr::read_csv(
  here::here("inst", "european_standard_population.csv"),
  col_types = list(
    AgeGroup = "character",
    EuropeanStandardPopulation = "integer")) |>
  dplyr::rename("age_group" = AgeGroup,
                "pop" = EuropeanStandardPopulation) %>%
  dplyr::mutate(age_group = stringr::str_remove(age_group, " years")) %>%
  dplyr::mutate(age_group = stringr::str_replace(age_group, "-", " to ")) %>%
  dplyr::mutate(age_group = stringr::str_replace(age_group, "plus", " to 150"))

wsp2025 <- readr::read_csv(
  here::here("inst", "world_standard_population.csv"),
  col_types = list(
    AgeGroup = "character",
    WorldStandardPopulation = "integer"))|>
  dplyr::rename("age_group" = AgeGroup,
                "pop" = WorldStandardPopulation) %>%
  dplyr::mutate(age_group = stringr::str_remove(age_group, " years")) %>%
  dplyr::mutate(age_group = stringr::str_replace(age_group, "-", " to ")) %>%
  dplyr::mutate(age_group = stringr::str_replace(age_group, "plus", " to 150"))

usethis::use_data(esp2013, wsp2025,
                  internal = TRUE,
                  overwrite = TRUE)
