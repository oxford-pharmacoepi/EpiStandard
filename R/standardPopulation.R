
#' Standard population
#'
#' @param name Dataset to use. One of "esp2013" or wsp2025".
#' @param ageType Choose whether to use broad or narrow age groups.
#'
#' @return Tibble
#' @export

standardPopulation <- function(name = "esp2013",
                               ageType = "narrow"){

  supported_names <- c("esp2013", "wsp2025")
  if(!name %in% supported_names){
    cli::cli_abort("{name} not available - name must be one of: {supported_names}")
  }

  if(name == "esp2013" & ageType == "narrow"){
    return(get("esp2013"))
  }

  if(name == "wsp2025" & ageType == "narrow"){
    return(get("wsp2025"))
  }

  if(name == "esp2013" & ageType == "broad"){
    esp_broad <- get("esp2013") %>%
      dplyr::mutate(age_group = ifelse(age_group %in% c("0 to 4", "5 to 9", "10 to 14", "15 to 19"), "0 to 19", age_group)) %>%
      dplyr::mutate(age_group = ifelse(age_group %in% c("20 to 24", "25 to 29", "30 to 34", "35 to 39", "40 to 44", "45 to 49",
                                                        "50 to 54", "55 to 59", "60 to 64"), "20 to 64", age_group)) %>%
      dplyr::mutate(age_group = ifelse(age_group %in% c("65 to 69", "70 to 74", "75 to 79", "80 to 84", "85 to 89", "90 to 150"), "65 to 150", age_group)) %>%
      dplyr::group_by(age_group) %>%
      dplyr::summarise(pop = sum(pop), .groups = 'drop')
    return(esp_broad)
  }

  if(name == "wsp2025" & ageType == "broad"){
    wsp_broad <- get("wsp2025") %>%
      dplyr::mutate(age_group = ifelse(age_group %in% c("0 to 4", "5 to 9", "10 to 14", "15 to 19"), "0 to 19", age_group)) %>%
      dplyr::mutate(age_group = ifelse(age_group %in% c("20 to 24", "25 to 29", "30 to 34", "35 to 39", "40 to 44", "45 to 49",
                                                        "50 to 54", "55 to 59", "60 to 64"), "20 to 64", age_group)) %>%
      dplyr::mutate(age_group = ifelse(age_group %in% c("65 to 69", "70 to 74", "75 to 79", "80 to 84", "85 to 150"), "65 to 150", age_group)) %>%
      dplyr::group_by(age_group) %>%
      dplyr::summarise(pop = sum(pop), .groups = 'drop')
    return(wsp_broad)
  }

}
