#' Create new merged age groups
#'
#' @param refdata Standard population dataset you want to use.
#' @param age Column in refdata with age values.
#' @param pop Column in refdata with population counts.
#' @param newGroups Create a list of new age groups you want to create.
#' @return Table
#' @export
#' @examples
#' \donttest{
#'
#' refdata <- standardPopulation(name = "esp2013")
#'
#' refdata |> dplyr::glimpse()
#'
#' merged_data <- mergeAgeGroups(refdata, c("0-19", "20-64", "65-150"))
#'
#' merged_data |> dplyr::glimpse()
#'
#' }

mergeAgeGroups <- function(refdata,
                           newGroups,
                           age = "age_group",
                           pop = "pop") {

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

  refdata <- refdata |>
    dplyr::mutate(age_low = stringr::str_extract(.data[[age]], "\\d+"),
                  age_high = stringr::str_extract(.data[[age]], "\\d+$")) |>
    dplyr::mutate(
      age_low  = as.integer(age_low),
      age_high = as.integer(age_high)
    )

  # Validate parsed bounds
  if (anyNA(refdata$age_high) | anyNA(refdata$age_low)) {
    bad_vals <- refdata[[age]][is.na(refdata$age_low) | is.na(refdata$age_high)]
    cli::cli_abort(c(
      "Some {.field age_group} values could not be parsed.",
      "x" = "Invalid labels: {.val {bad_vals}}",
      "i" = "Use format that includes lower and upper bound of each age group,
      for example {.val '0-4'} or {.val '0 to 4'}."
    ))
  }

  merged_list <- vector("list", length(newGroups))

  for (i in seq_along(newGroups)) {
    # Parse target range for this group
    start <- as.integer(stringr::str_extract(newGroups[[i]], "\\d+"))
    end   <- as.integer(stringr::str_extract(newGroups[[i]], "\\d+$"))

    # Rows fully inside [start, end]
    in_range <- refdata[refdata$age_low >= start & refdata$age_high <= end, , drop = FALSE]

    # Check alignment with existing boundaries
    if (nrow(in_range) == 0L ||
        min(in_range$age_low,  na.rm = TRUE) != start ||
        max(in_range$age_high, na.rm = TRUE) != end) {
      cli::cli_abort(c(
        "Cannot create custom range {.val {newGroups[[i]]}}.",
        "x" = "It does not align with existing age group boundaries."
      ))
    }

    total_pop <- sum(in_range$pop, na.rm = TRUE)

    merged_list[[i]] <- data.frame(
      age_group = newGroups[[i]],
      pop = total_pop,
      stringsAsFactors = FALSE
    )
  }

  # If you want a single data frame at the end:
  result <- dplyr::bind_rows(merged_list)

  rownames(result) <- NULL

  return(result)
}
