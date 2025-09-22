#' Calculate directly standardised rates
#'
#' Computes crude and directly standardised rates. Rates can be stratified by
#' variables of interest.
#'
#' @param data A data frame with the event counts to be standardised.
#' @param refdata A data frame representing the standard population. It must contain two columns:
#' age, with the different age groups (notice that this column name must be the same as
#' in data, defined by the input age); and pop, with the number of individuals in each corresponding
#' age group.
#' @param event Name of the column in data that corresponds to the event counts.
#' @param time Name of the column in data that corresponds to the unit-time (denominator).
#' @param age Name of the column in data and refdata that corresponds to age groups.
#' @param pop Name of the column in refdata that corresponds to the standard population in each age group.
#' @param strata Name of the columns in data for which rates are calculated by.
#' @param multiplier A constant to multiply rates by (e.g. multiplier = 1000 for rates per 1000).
#' @param method Choose between normal, lognormal and gamma confidence intervals
#' for crude and standardised rates. The default method is normal.
#' @param sig The desired level of confidence in computing confidence intervals.
#' The default is 0.95 for 95 percent CIs.
#'
#' @importFrom rlang .data
#' @importFrom rlang ":="
#'
#' @examples
#' # An example of calculating directly standardised rates
#' # Data example is from Table 1 (p.132) of Fundamentals of Epidemiology by Schoenbach, 2000.
#'
#' # The following table shows the number of deaths, for 5 different age groups,
#' # in the states of Miami and Alaska:
#' data <- data.frame(
#'       state = rep(c('Miami',"Alaska"), c(5,5)),
#'       age_groups = rep(c('00-14','15-24','25-44','45-64','65+'),2),
#'       deaths = c(136, 57, 208, 1016, 3605, 59, 18, 37, 90, 81),
#'       general_population = c(114350,80259,133440,142670,92168,37164,20036,32693,14947,2077))
#'
#' # We aim to standardise the number of deaths per each state. To do that, we will use the following
#' # US standard population:
#' standardised_population <- data.frame(
#'                             age_groups = c('00-14','15-24','25-44','45-64','65+'),
#'                             pop = c(23961000,15420000,21353000,19601000,10685000))
#'
#' # Now we will use the function dsr to calculate the direct standardised rates
#' # (per 1000 individuals) using a 95% CI calculated by the gamma method:
#' my_results <- dsr(data = data,
#'                   refdata = standardised_population,
#'                   event = "deaths",
#'                   time  = "general_population",
#'                   age   = "age_groups",
#'                   pop   = "pop",
#'                   strata = "state",
#'                   multiplier = 1000,
#'                   method = "gamma",
#'                   sig = 0.95)
#' # View results
#' my_results
#' @export
dsr <- function(data,
                refdata  = standardPopulation("esp2013"),
                event,
                time,
                age = "age_group",
                pop = "pop",
                strata = NULL,
                multiplier = 1000,
                method = "normal",
                sig = 0.95) {

  #validations

  if(isFALSE(is.data.frame(data))){
    cli::cli_abort("'data' must be a dataframe")
  }

  if(isFALSE(is.data.frame(refdata))){
    cli::cli_abort("'refdata' must be a dataframe")
  }

  if(!event %in% names(data)) {
    cli::cli_abort("'event' must be a column in 'data'")
  }

  if(!time %in% names(data)) {
    cli::cli_abort("'time' must be a column in 'data'")
  }

  if(!pop %in% names(refdata)) {
    cli::cli_abort("'pop' must be a column in 'refdata'")
  }

  if(!age %in% names(refdata) |!age %in% names(data) ) {
    cli::cli_abort("'age' must be a column in 'refdata' and 'data'")
  }

  if(is.null(strata) == FALSE){
  if(!all(strata %in% names(data))) {
    cli::cli_abort("'strata' must be a column or columns in 'data'")
  }
  }

  if (!setequal(unique(data[age]), unique(refdata[age]))) {
    cli::cli_abort("'age' values differ between 'data' and 'refdata'")
  }

  #function

  all_data_st <- data |>
    dplyr::left_join(refdata, by = dplyr::join_by(!!rlang::sym(age)))

  if (!is.null(strata)) {
    all_data_st <- all_data_st |>
      dplyr::group_by(!!!rlang::syms(strata))
  }
  all_data_st <- all_data_st |>
    dplyr::mutate(n = sum(!!rlang::sym(event)),
                  d = sum(!!rlang::sym(time))) |>
    dplyr::mutate(
      cr_rate = .data$n / .data$d,
      cr_var = .data$n / .data$d ^ 2,
      wts = !!rlang::sym(pop) / sum(!!rlang::sym(pop)),
      st_rate = sum(.data$wts * (!!rlang::sym(event) / !!rlang::sym(time))),
      st_var = sum(as.numeric((.data$wts ^ 2) * (
        !!rlang::sym(event) / (!!rlang::sym(time)) ^ 2
      )))
    ) |>
    dplyr::distinct(!!!strata, .keep_all = TRUE) |>
    dplyr::select(dplyr::all_of(c(
      strata, "n", "d", "cr_rate", "cr_var", "st_rate", "st_var"
    )))

  if (!is.null(strata)) {
    all_data_st <- all_data_st |>
      dplyr::ungroup()
  }

  # Compute Confidence Intervals (CI) according to method. The default is 'normal'
  if (method == "gamma") {
    tmp1 <- all_data_st |>
      dplyr::mutate(
        c_rate = multiplier * .data$cr_rate,
        c_lower = multiplier * stats::qgamma((1 - sig) / 2, shape = .data$cr_rate ^
                                       2 / (.data$cr_var)) / (.data$cr_rate / .data$cr_var),
        c_upper = multiplier * stats::qgamma(1 - ((1 - sig) / 2), shape = 1 + .data$cr_rate ^
                                       2 / (.data$cr_var)) / (.data$cr_rate / .data$cr_var),
        s_rate = multiplier * .data$st_rate,
        s_lower = multiplier * stats::qgamma((1 - sig) / 2, shape = .data$st_rate ^
                                       2 / .data$st_var) / (.data$st_rate / .data$st_var),
        s_upper = multiplier * stats::qgamma(1 - ((1 - sig) / 2), shape = 1 + (.data$st_rate ^
                                                                         2 / .data$st_var)) / (.data$st_rate / .data$st_var)
      )

  } else if (method == "normal") {
    tmp1 <- all_data_st |>
      dplyr::mutate(
        c_rate = multiplier * .data$cr_rate,
        c_lower = multiplier * (.data$cr_rate + stats::qnorm((1 - sig) / 2) * sqrt(.data$cr_var)),
        c_upper = multiplier * (.data$cr_rate - stats::qnorm((1 - sig) / 2) * sqrt(.data$cr_var)),
        s_rate = multiplier * .data$st_rate,
        s_lower = multiplier * (.data$st_rate + stats::qnorm((1 - sig) / 2) * sqrt(.data$st_var)),
        s_upper = multiplier * (.data$st_rate - stats::qnorm((1 - sig) / 2) * sqrt(.data$st_var))
      )

  } else if (method == "lognormal") {
    tmp1 <- all_data_st |>
      dplyr::mutate(
        c_rate = multiplier * .data$cr_rate,
        c_lower = multiplier * exp((
          log(.data$cr_rate) + stats::qnorm((1 - sig) / 2) * sqrt(.data$cr_var) /
            (.data$cr_rate)
        )),
        c_upper = multiplier * exp((
          log(.data$cr_rate) - stats::qnorm((1 - sig) / 2) * sqrt(.data$cr_var) /
            (.data$cr_rate)
        )),
        s_rate = multiplier * .data$st_rate,
        s_lower = multiplier * exp((
          log(.data$st_rate) + stats::qnorm((1 - sig) / 2) * sqrt(.data$st_var) /
            (.data$st_rate)
        )),
        s_upper = multiplier * exp((
          log(.data$st_rate) - stats::qnorm((1 - sig) / 2) * sqrt(.data$st_var) /
            (.data$st_rate)
        ))
      )
  } else {
    cli::cli_abort("method must be set as 'normal', 'lognormal', or 'gamma'")
  }

  #Clean up and output
  tmp1 <- tmp1 |>
    dplyr::mutate(across(c(c_rate, c_lower, c_upper, s_rate, s_lower, s_upper),
                  ~ round(.x, digits = 4)))

  c_rate_name <- paste0('Crude Rate (per ', multiplier, ')')
  c_lower_name <- paste0(sig * 100, '% LCL (Crude)')
  c_upper_name <- paste0(sig * 100, '% UCL (Crude)')
  s_rate_name <- paste0('Std Rate (per ', multiplier, ')')
  s_lower_name <- paste0(sig * 100, '% LCL (Std)')
  s_upper_name <- paste0(sig * 100, '% UCL (Std)')

  tmp1 <- tmp1 |>
    dplyr::select(
      strata,
      "Numerator" = "n",
      "Denominator" = "d",
      !!c_rate_name := "c_rate",
      !!c_lower_name := "c_lower",
      !!c_upper_name := "c_upper",
      !!s_rate_name := "s_rate",
      !!s_lower_name := "s_lower",
      !!s_upper_name := "s_upper") |>
    dplyr::distinct()

  tmp1

}


