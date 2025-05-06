#' Age groups
#'
#' @param name Dataset - One of "esp2013" or ""wsp2025".
#' @param type Choose whether to use broad or narrow age groups.
#' @return List
#' @export
#'

ageGroups <- function(name = "esp2013", type = "broad"){

  supported_names <- c("esp2013", "wsp2025")
  if(!name %in% supported_names){
    cli::cli_abort("{name} not available - name must be one of: {supported_names}")
  }

  supported_types <- c("broad", "narrow")
  if(!type %in% supported_types){
    cli::cli_abort("{type} not available - name must be one of: {supported_types}")
  }

  if(name == "esp2013" & type == "narrow"){
    groups <- list(
      c(0,4),
      c(5,9),
      c(10,14),
      c(15,19),
      c(20,24),
      c(25,29),
      c(30,34),
      c(35,39),
      c(40,44),
      c(45,49),
      c(50,54),
      c(55,59),
      c(60,64),
      c(65,69),
      c(70,74),
      c(75,79),
      c(80,84),
      c(85,89),
      c(90,150)
    )

    # Convert to readable labels like "0-4"
    labels <- sapply(groups, function(x) paste0(x[1], "-", x[2]))
    return(labels)
  }

  if(name == "esp2013" & type == "broad"){
    groups <- list(
      c(0,19),
      c(20,64),
      c(65,150)
    )

    # Convert to readable labels like "0-4"
    labels <- sapply(groups, function(x) paste0(x[1], "-", x[2]))
    return(labels)
  }

  if(name == "wsp2025" & type == "narrow"){
    groups <- list(
      c(0,4),
      c(5,9),
      c(10,14),
      c(15,19),
      c(20,24),
      c(25,29),
      c(30,34),
      c(35,39),
      c(40,44),
      c(45,49),
      c(50,54),
      c(55,59),
      c(60,64),
      c(65,69),
      c(70,74),
      c(75,79),
      c(80,84),
      c(85,150)
    )

    # Convert to readable labels like "0-4"
    labels <- sapply(groups, function(x) paste0(x[1], "-", x[2]))
    return(labels)
  }

  if(name == "wsp2025" & type == "broad"){
    groups <- list(
      c(0,19),
      c(20,64),
      c(65,150)
    )

    # Convert to readable labels like "0-4"
    labels <- sapply(groups, function(x) paste0(x[1], "-", x[2]))
    return(labels)
  }

}
