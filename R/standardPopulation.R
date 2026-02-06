#' Standard population
#'
#' @param region Region of standard population. Can be either 'Europe' or 'World'.
#'
#' @return Tibble
#' @export
#' @examples
#' \donttest{
#' standard_data <- standardPopulation(region = "Europe")
#' }
#'

standardPopulation <- function(region = "Europe"){

  supported_regions <- c("Europe", "World")
  if(!region %in% supported_regions){
    cli::cli_abort("{region} not available - name must be one of: {supported_regions}")
  }

  if(region == "Europe"){
    return(get("esp2013"))
  }

  if(region == "World"){
    return(get("wsp2025"))
  }
}
