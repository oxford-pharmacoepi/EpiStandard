#' Create new merged age groups
#'
#' @param refdata Standard population dataset you want to use.
#' @param newGroups Create a list of new age groups you want to create.
#' @param event Column in refdata with outcome counts.
#' @param age Column in refdata with age values.
#' @param pop Column in refdata with population counts, preferably in person-time.
#' @param strata Column or columns to stratify by.
#' @return Table
#' @export
#' @examples
#' \donttest{
#'
#'
#' esp2013|> dplyr::glimpse()
#'
#' merged_data <- mergeAgeGroups(standardPopulation("esp2013"), c("0-19", "20-64", "65-150"))
#'
#' merged_data |> dplyr::glimpse()
#'
#' }
#'

mergeAgeGroups <- function(refdata,
                           newGroups,
                           event = NULL,
                           age = "age_group",
                           pop = "pop",
                           strata = NULL) {

  if(isFALSE(is.data.frame(refdata))){
    cli::cli_abort("'refdata' must be a dataframe")
  }

  if(isFALSE(is.vector(newGroups))){
    cli::cli_abort("'newGroups' must be a vector")
  }

  # Check input columns
  if (!age %in% names(refdata)) {
    cli::cli_abort("Input data must contain {.field age_group}")
  }

  if (!pop %in% names(refdata)) {
    cli::cli_abort("Input data must contain {.field pop}")
  }

  if(is.null(event) == FALSE){
    if(!all(event %in% names(refdata))){
    cli::cli_abort("Input data must contain {.field event}.")
    }
  }

  if(is.null(strata) == FALSE){
    if(!all(strata %in% names(refdata))) {
      cli::cli_abort("'strata' must be a column or columns in 'refdata'")
    }
  }

  newRefdata <- refdata |>
    dplyr::mutate(age_low = stringr::str_extract(.data[[age]], "\\d+"),
                  age_high = stringr::str_extract(.data[[age]], "\\d+$")) |>
    dplyr::mutate(
      age_low  = as.integer(.data$age_low),
      age_high = as.integer(.data$age_high)
    )

  # Validate parsed bounds
  if (anyNA(newRefdata$age_high) | anyNA(newRefdata$age_low)) {
    bad_vals <- newRefdata[[age]][is.na(newRefdata$age_low) | is.na(newRefdata$age_high)]
    cli::cli_abort(c(
      "Some {.field age_group} values could not be parsed.",
      "x" = "Invalid labels: {.val {bad_vals}}",
      "i" = "Use format that includes lower and upper bound of each age group,
      for example {.val '0-4'} or {.val '0 to 4'}."
    ))
  }

  if(sum(is.na(newRefdata$age_high)) > 0 | sum(is.na(newRefdata$age_low))){
    cli::cli_abort("The minimum and maximum age for each age group in refdata
    must be defined. For example, cannot have age group '65+'.")
  }

  merged_list <- vector("list", length(newGroups))

  for (i in seq_along(newGroups)) {
    start <- as.integer(stringr::str_extract(newGroups[[i]], "\\d+"))
    end   <- as.integer(stringr::str_extract(newGroups[[i]], "\\d+$"))

    # Rows fully inside [start, end]
    in_range <- newRefdata[newRefdata$age_low >= start & newRefdata$age_high <= end, , drop = FALSE]

    # Check alignment with existing boundaries
    if (nrow(in_range) == 0L ||
        min(in_range$age_low) != start ||
        max(in_range$age_high) != end) {
      cli::cli_abort(c( + "Cannot create custom range {.val {newGroups[[i]]}}.",
                        "x" = "It does not align with existing age group boundaries."
      ))
      }

    if(is.null(strata) & is.null(event)){

    total_pop <- sum(in_range$pop, na.rm = TRUE)

    merged_list[[i]] <- tibble::tibble(
      !!rlang::sym(age) := newGroups[[i]],
      !!rlang::sym(pop) := total_pop,
      stringsAsFactors = FALSE
    )
    } else if(!is.null(strata) & !is.null(event)){

    total_pop <- in_range |>
      dplyr::group_by(!!!rlang::syms(strata)) |>
      dplyr::summarise(!!rlang::sym(pop) := sum(.data[[pop]]),
                       !!rlang::sym(event) := sum(.data[[event]]))

    merged_list[[i]] <- total_pop |>
      dplyr::mutate(!!rlang::sym(age) := newGroups[i])

    } else if(!is.null(strata) & is.null(event)){

      total_pop <- in_range |>
        dplyr::group_by(!!!rlang::syms(strata)) |>
        dplyr::summarise(!!rlang::sym(pop) := sum(.data[[pop]]))

      merged_list[[i]] <- total_pop |>
        dplyr::mutate(!!rlang::sym(age) := newGroups[i])

    }
  }

  # If you want a single data frame at the end:
  result <- dplyr::bind_rows(merged_list) |>
    dplyr::select(colnames(refdata))

  rownames(result) <- NULL

  return(result)
}
