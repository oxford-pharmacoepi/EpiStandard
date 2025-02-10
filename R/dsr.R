#' Calculate directly standardised rates
#'
#' Computes crude and directly standardised rates. Rates can be stratified by
#' variables of interest.
#'
#' @param data A data frame with counts and unit-times summarised by the
#' standardisation variables.
#' @param event Name of the variable in data that corresponds to the event
#' counts.
#' @param time Name of the variable in data that corresponds to the unit-time.
#' @param age Name of the variable in data that corresponds to age
#' @param pop Name of the variable in data that corresponds to population
#' @param strata A variable within the input data frame for which rates are
#' calculated by.
#' @param refdata A data frame with population unit-times summarised by the
#' standardisation variables. The unit-time variable name must named pop.
#' @param mp A constant to multiply rates by (e.g. mp=1000 for rates per 1000).
#' @param method Choose between normal, lognormal and gamma confidence intervals
#' for crude and standardised rates. The default method is normal.
#' @param sig The desired level of confidence in computing confidence intervals.
#' The default is 0.95 for 95 percent CIs.
#' @param decimals Round estimates to a desired decimal place.
#'
#' @importFrom rlang .data
#' @importFrom rlang ":="
#'
#' @examples
#' #An example of calculating directly standardised rates
#' #Data from Table 1, Page 132 of Schoenbach (2000)
#'
#' #State specific death counts and fu
#' df_study <- data.frame(state=rep(c('Miami',"Alaska"), c(5,5)),
#'                       age=rep(c('00-14','15-24','25-44','45-64','65+'),2),
#'                       deaths=c(136,57,208,1016,3605,59,18,37,90,81),
#'                       fu=c(114350,80259,133440,142670,92168,37164,20036,32693,14947,2077))
#'
#' #US standard population
#' df_ref  <- data.frame(age=c('00-14','15-24','25-44','45-64','65+'),
#'                      pop=c(23961000,15420000,21353000,19601000,10685000))
#'
#' #Directly standardised Rates (per 1000) - 95% CI's using the gamma method
#' my_results <- dsr(data = df_study,
#'                   event = "deaths",
#'                   time = "fu",
#'                   strata = "state",
#'                   age = "age",
#'                   refdata = df_ref)
#' #View results
#' my_results
#' @export

dsr <- function(data,
                event,
                time,
                age = "age_group",
                pop = "pop",
                strata = NULL,
                refdata  = standardPopulation("esp2013"),
                mp = 1000,
                method = "normal",
                sig = 0.95,
                decimals = 4) {

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
      wts = .data$pop / sum(.data$pop),
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
        c_rate = mp * .data$cr_rate,
        c_lower = mp * stats::qgamma((1 - sig) / 2, shape = .data$cr_rate ^
                                       2 / (.data$cr_var)) / (.data$cr_rate / .data$cr_var),
        c_upper = mp * stats::qgamma(1 - ((1 - sig) / 2), shape = 1 + .data$cr_rate ^
                                       2 / (.data$cr_var)) / (.data$cr_rate / .data$cr_var),
        s_rate = mp * .data$st_rate,
        s_lower = mp * stats::qgamma((1 - sig) / 2, shape = .data$st_rate ^
                                       2 / .data$st_var) / (.data$st_rate / .data$st_var),
        s_upper = mp * stats::qgamma(1 - ((1 - sig) / 2), shape = 1 + (.data$st_rate ^
                                                                         2 / .data$st_var)) / (.data$st_rate / .data$st_var)
      )

  } else if (method == "normal") {
    tmp1 <- all_data_st |>
      dplyr::mutate(
        c_rate = mp * .data$cr_rate,
        c_lower = mp * (.data$cr_rate + stats::qnorm((1 - sig) / 2) * sqrt(.data$cr_var)),
        c_upper = mp * (.data$cr_rate - stats::qnorm((1 - sig) / 2) * sqrt(.data$cr_var)),
        s_rate = mp * .data$st_rate,
        s_lower = mp * (.data$st_rate + stats::qnorm((1 - sig) / 2) * sqrt(.data$st_var)),
        s_upper = mp * (.data$st_rate - stats::qnorm((1 - sig) / 2) * sqrt(.data$st_var))
      )

  } else if (method == "lognormal") {
    tmp1 <- all_data_st |>
      dplyr::mutate(
        c_rate = mp * .data$cr_rate,
        c_lower = mp * exp((
          log(.data$cr_rate) + stats::qnorm((1 - sig) / 2) * sqrt(.data$cr_var) /
            (.data$cr_rate)
        )),
        c_upper = mp * exp((
          log(.data$cr_rate) - stats::qnorm((1 - sig) / 2) * sqrt(.data$cr_var) /
            (.data$cr_rate)
        )),
        s_rate = mp * .data$st_rate,
        s_lower = mp * exp((
          log(.data$st_rate) + stats::qnorm((1 - sig) / 2) * sqrt(.data$st_var) /
            (.data$st_rate)
        )),
        s_upper = mp * exp((
          log(.data$st_rate) - stats::qnorm((1 - sig) / 2) * sqrt(.data$st_var) /
            (.data$st_rate)
        ))
      )
  }

  #Clean up and output
  tmp1$c_rate  <- round(tmp1$c_rate, digits = decimals)
  tmp1$c_lower <- round(tmp1$c_lower, digits = decimals)
  tmp1$c_upper <- round(tmp1$c_upper, digits = decimals)
  tmp1$s_rate  <- round(tmp1$s_rate, digits = decimals)
  tmp1$s_lower <- round(tmp1$s_lower, digits = decimals)
  tmp1$s_upper <- round(tmp1$s_upper, digits = decimals)

  c_rate_name <- paste0('Crude Rate (per ', mp, ')')
  c_lower_name <- paste0(sig * 100, '% LCL (Crude)')
  c_upper_name <- paste0(sig * 100, '% UCL (Crude)')
  s_rate_name <- paste0('Std Rate (per ', mp, ')')
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
    distinct()

  tmp1

}


