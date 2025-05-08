
#' Standard population
#'
#' @param name Dataset to use. One of "esp2013" or "wsp2025".
#'
#' @return Tibble
#' @export
#' @examples
#' \donttest{
#' standard_data <- standardPopulation("esp2013")
#' }
#'

standardPopulation <- function(name = "esp2013"){

  supported_names <- c("esp2013", "wsp2025")
  if(!name %in% supported_names){
    cli::cli_abort("{name} not available - name must be one of: {supported_names}")
  }

  if(name == "esp2013"){
    return(get("esp2013"))
  }

  if(name == "wsp2025"){
    return(get("wsp2025"))
  }
}
