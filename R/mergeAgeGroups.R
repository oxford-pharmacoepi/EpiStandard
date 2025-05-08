#' Create new merged age groups
#'
#' @param data Standard population dataset you want to use.
#' @param newGroups Create a list of new age groups you want to create
#' @return Table
#' @export
#' @examples
#' \donttest{
#'
#' data <- standardPopulation(name = "esp2013", ageType = "narrow")
#'
#' data |> dplyr::glimpse()
#'
#' merged_data <- mergeAgeGroups(data, c("0-19", "20-64", "65-150"))
#'
#' merged_data |> dplyr::glimpse()
#'
#' }

mergeAgeGroups <- function(data, new_ranges) {

  # Check input columns
  if (!all(c("age_group", "pop") %in% colnames(data))) {
    cli::cli_abort("Input data must contain {.field age_group} and {.field pop} columns.")
  }

  # Helper: parse age group bounds (supports "X-Y" and "X to Y")
  parse_age_bounds <- function(age_group) {
    normalized <- gsub(" to ", "-", age_group)  # Normalize "to" to "-"
    if (!grepl("^\\d+-\\d+$", normalized)) return(c(NA, NA))
    start <- as.numeric(sub("-.*", "", normalized))
    end   <- as.numeric(sub(".*-", "", normalized))
    return(c(start, end))
  }

  # Parse age group bounds
  age_bounds <- t(sapply(data$age_group, parse_age_bounds))
  data$age_start <- age_bounds[, 1]
  data$age_end   <- age_bounds[, 2]

  # Validate parsed bounds
  if (anyNA(data$age_start) | anyNA(data$age_end)) {
    bad_vals <- data$age_group[is.na(data$age_start) | is.na(data$age_end)]
    cli::cli_abort(c(
      "Some {.field age_group} values could not be parsed.",
      "x" = "Invalid labels: {.val {bad_vals}}",
      "i" = "Use formats like {.val '0-4'} or {.val '0 to 4'} only."
    ))
  }

  merged_list <- list()

  for (range_label in new_ranges) {
    # Parse target range
    range_vals <- parse_age_bounds(range_label)
    if (anyNA(range_vals)) {
      cli::cli_abort("Invalid custom age range format: {.val {range_label}}")
    }
    start <- range_vals[1]
    end <- range_vals[2]

    # Find matching rows in data
    in_range <- data[data$age_start >= start & data$age_end <= end, ]

    # Check for misalignment with original groups
    if (nrow(in_range) == 0 || min(in_range$age_start) > start || max(in_range$age_end) < end) {
      cli::cli_abort(c(
        "Cannot create custom range {.val {range_label}}.",
        "x" = "It does not align with existing age group boundaries."
      ))
    }

    total_pop <- sum(in_range$pop)

    merged_list[[length(merged_list) + 1]] <- data.frame(
      age_group = range_label,
      pop = total_pop
    )
  }

  result <- do.call(rbind, merged_list)
  rownames(result) <- NULL
  return(result)
}
