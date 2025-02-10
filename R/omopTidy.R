#' Tidy incidence results.
#'
#' Tidy summarised results produced by `IncidencePrevalence`.
#'
#' @param data A data frame with incidence estimates produced by the
#' function `estimateIncidence()` in the IncidencePrevalence package.
#' @return Data frame
#' @examples
#' # example code
#' con <- DBI::dbConnect(duckdb::duckdb(), CDMConnector::eunomiaDir("GiBleed"))
#'
#' cdm <- CDMConnector::cdmFromCon(con, cdmSchema = "main", writeSchema = "main")
#'
#' cdm$gibleed <- cdm |>
#' CohortConstructor::conceptCohort(conceptSet = list("gibleed" = 192671L),
#' name = "gibleed")
#'
#' cdm <- IncidencePrevalence::generateDenominatorCohortSet(
#' cdm = cdm,
#' name = "denominator_esp",
#' ageGroup = ageGroups(name = "esp2013"),
#' sex = "Both",
#' cohortDateRange = as.Date(c("2012-01-01", "2022-01-01"))
#' )
#'
#' inc_data_esp <- IncidencePrevalence::estimateIncidence(
#' cdm = cdm,
#' denominatorTable = "denominator_esp",
#' outcomeTable = "gibleed",
#' interval = "years",
#' repeatedEvents = TRUE,
#' outcomeWashout = 30,
#' completeDatabaseIntervals = FALSE
#' )
#'
#' inc_tidy_esp <- omopTidy(inc_data_esp)
#'
#' @export
#'

omopTidy <- function(data) {

  omopgenerics::validateResultArgument(data)

  cleanedData <- data %>%
    omopgenerics::splitAdditional() %>%
    omopgenerics::splitGroup() %>%
    omopgenerics::addSettings() %>%
    dplyr::filter(variable_name == "Outcome") %>%
    omopgenerics::pivotEstimates(pivotEstimatesBy = "estimate_name")

  cleanedData_1 <- data %>%
    omopgenerics::splitAdditional() %>%
    omopgenerics::splitGroup() %>%
    omopgenerics::addSettings() %>%
    dplyr::filter(variable_name == "Denominator") %>%
    omopgenerics::pivotEstimates(pivotEstimatesBy = "estimate_name") %>%
    dplyr::select(person_days,
           person_years,
           denominator_count
    )

  inc_tidy <- dplyr::bind_cols(cleanedData, cleanedData_1) %>%
    rename(age_group = denominator_age_group)

  inc_tidy
}
