
#' Standard population
#'
#' @param name Dataset to use. One of "esp2013", "wsp2025", "esp2013_by_sex" or "wsp2025_by_sex".
#'
#' @return Tibble
#' @export
#'
#' @examples
#' standardPopulation()
standardPopulation <- function(name = "esp2013"){

  supported_names <- c("esp2013", "wsp2025", "esp2013_by_sex", "wsp2025_by_sex")
  if(!name %in% supported_names){
    cli::cli_abort("{name} not available - name must be one of: {supported_names}")
  }

  if(name == "esp2013"){
    return(get("esp2013"))
  }

  if(name == "wsp2025"){
    return(get("wsp2025"))
  }

  if(name == "esp2013_by_sex"){
    return(get("esp2013_by_sex"))
  }

  if(name == "wsp2025_by_sex"){
    return(get("wsp2025_by_sex"))
  }

}
