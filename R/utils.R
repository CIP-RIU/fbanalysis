#' Select Options
#' @param fieldbook A data.frame with the fieldbook data
#' @author Omar Benites
#' @description This function give item option for shinySelectize in ANOVA ui

select_options <- function(fieldbook){
  if(is.null(fieldbook)) headers <- c("no-traits")
  headers <-  setdiff(names(fieldbook), c("BOOK","DATE","PLOT"))
  headers
}
  

#' Error handler in fieldbook analysis
#' @param code source code to try
#' @param trait trait
#' @author Omar Benites
#' @description Error always happens during data analysis. This function aims to show a error message for end-users to
#' detect what it wrong in their data.
#' @export

single_error <- function(code, trait) {

aov_e <- try(code)
n <- length(aov_e)

  if(!is.data.frame(aov_e)) {
    aov_e <- attributes(aov_e)
    aov_e <- unlist(aov_e$condition)
    trait <- trait
    aov_e_msg <- aov_e$message
  } else {
    trait <- trait
    aov_e_msg <- "OK"
  }
  
  #In case the plant trait is ok
  trait_ok <- "There are no missing values to estimate."
  if(aov_e_msg==trait_ok){aov_e_msg <- "OK"} #put NULL into the message
 
  out <- list(trait = trait, error = aov_e_msg)
}




