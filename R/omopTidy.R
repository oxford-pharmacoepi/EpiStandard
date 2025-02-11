#' Tidy incidence results.
#'
#' Tidy summarised results produced by `IncidencePrevalence`.
#'
#' @param data A data frame with incidence estimates produced by the
#' function `estimateIncidence()` in the IncidencePrevalence package.
#' @return Data frame
#'
#' @export
#'

omopTidy <- function(data) {

  omopgenerics::validateResultArgument(data)

  cleanedData <- data |>
    omopgenerics::splitAdditional() |>
    omopgenerics::splitGroup() |>
    omopgenerics::addSettings() |>
    dplyr::filter(variable_name == "Outcome") |>
    omopgenerics::pivotEstimates(pivotEstimatesBy = "estimate_name")

  cleanedData_1 <- data |>
    omopgenerics::splitAdditional() |>
    omopgenerics::splitGroup() |>
    omopgenerics::addSettings() |>
    dplyr::filter(variable_name == "Denominator") |>
    omopgenerics::pivotEstimates(pivotEstimatesBy = "estimate_name") |>
    dplyr::select(person_days,
           person_years,
           denominator_count
    )

  inc_tidy <- dplyr::bind_cols(cleanedData, cleanedData_1) |>
    dplyr::rename(age_group = denominator_age_group)

  inc_tidy
}
