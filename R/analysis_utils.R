#'Anova Select Options
#'
#' @author Omar Benites
#' @description This function give item option for shinySelectize in ANOVA ui

anova_select_options <- function(fieldbook){
  if(is.null(fieldbook)) headers <- c("no-traits")
  headers <- names(fieldbook)
  headers
}