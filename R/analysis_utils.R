#' Select Options
#' @param fieldbook A data.frame with the fieldbook data
#' @author Omar Benites
#' @description This function give item option for shinySelectize in ANOVA ui

select_options <- function(fieldbook){
  if(is.null(fieldbook)) headers <- c("no-traits")
  headers <- names(fieldbook)
  headers
}