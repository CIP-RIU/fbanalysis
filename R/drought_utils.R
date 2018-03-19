#' #'Function to calculate the Drought Suceptibility Index (DSI) using mean yield values
#' #'
#' #'@param ylmeans A data frame with yield means. Included genotypes and Factor.
#' #'@param clabel The label of water control factor. Example: "irrigation","normal irrigation"
#' #'@param slabel The label of water stress factor. Example: "stress", "inttermitent 
#' #'@return The drought suceptible index using means
#' #'@author Omar Benites
#' #'@details This function returns the drought suceptible index (DSI) using means.
#' #'@references Comparison of yield based drought tolerance indices in improved varieties, genetic stocks and landraces
#' #'of potato (Solanum tuberosum L.)
#' #'@keywords index, drought
#' #'@family index
#' #'@export
#' 
#' #DSI <- function(dfmeans,clabel=NA, slabel=NA)
#' DSI.means<- function(ylmeans,clabel=NA, slabel=NA)
#' {
#'   if(missing(ylmeans)){
#'     stop("Please enter data frame of yield means") 
#'   }
#'   
#'   if(missing(clabel)){
#'     stop("Please enter the control condition")
#'   }
#'   
#'   if(missing(slabel)){
#'     stop("Please enter the stress condition")
#'   }
#'   
#'   #   mysubset <- function(df, ...) {
#'   #     ssubset <- deparse(substitute(...))
#'   #     subset(df, eval(parse(text = ssubset)))
#'   #   }
#'   mysubset <- function(df, ssubset) {
#'     subset(df, eval(parse(text=ssubset)))
#'   }
#'   
#'   yield.mean<- function(yf){ 
#'     ym <- mean(yf[,3],na.rm = TRUE) 
#'     ym
#'   }
#'   
#'   if(!is.na(clabel)){
#'     #wc <- subset(ylmeans,FACTOR==clabel) #control
#'     wc <- mysubset(ylmeans,paste("FACTOR","==",paste("'",clabel,"'",sep = ""),sep = ""))
#'     names(wc) <- c("INSTN", "FACTOR_C", "TTWP_Mean_C")
#'   }
#'   if(!is.na(slabel)){
#'     #ws <- subset(ylmeans,FACTOR==slabel) #stress
#'     #ws <- mysubset(ylmeans,FACTOR==slabel)
#'     ws <- mysubset(ylmeans,paste("FACTOR","==",paste("'",slabel,"'",sep = ""),sep = ""))
#'     names(ws) <- c("INSTN", "FACTOR_S", "TTWP_Mean_S")
#'   }
#'   
#'   #yfactor: yield mean by factor
#'   
#'   Mc <- yield.mean(wc) #control
#'   Ms <- yield.mean(ws) #stress
#'   
#'   indextable<- merge(wc,ws,by = "INSTN") #merge the two tables by INSTN
#'   indextable$DSI <- NA
#'   
#'   if(length(indextable$TTWP_Mean_S)>0 & length(indextable$TTWP_Mean_C)>0) indextable=within(indextable,{    
#'     DSI  <- round((1-(indextable$TTWP_Mean_S/indextable$TTWP_Mean_C))/(1-(Ms/Mc)),2)
#'     #     DTI <- round((TTWP_Mean_S*TTWP_Mean_C)/(Mc)^2,2)
#'     #     TOL <- round((TTWP_Mean_C-TTWP_Mean_S),2)
#'     #     MP <- round((TTWP_Mean_C+TTWP_Mean_S)/2,2)
#'     #     GM <-  sqrt(TTWP_Mean_S*TTWP_Mean_C)
#'     #     TDWS <- (TTWP_Mean_S/TTWP_Mean_C)*100
#'   })
#'   
#'   return(indextable)
#' }
#' 
#' 
#' # if(trial == "Abiotic stress") {
#' # 
#' #   lvl1 <- as.character(hot_params()$hot_factor_lvl1)
#' #   lvl2 <- as.character(hot_params()$hot_factor_lvl2)
#' #   
#' #   fb <- readxl::read_excel(hot_file, sheet ="Fieldbook")
#' #   di <- drought_index(fb, lvl1, lvl2)
#' #   
#' #   if(!is.null(di)){
#' #   
#' #     sheet <- "Drought_Indexes"
#' #     sheets <- readxl::excel_sheets(path = hot_file)
#' #     
#' #     if(sheet %in% sheets){
#' #       openxlsx::removeWorksheet(wb = wb, sheet = "AbioticS_Indexes")
#' #     }
#' #     
#' #     openxlsx::addWorksheet(wb = wb, sheetName = sheet, gridLines = TRUE)
#' #   
#' #     try(openxlsx::writeDataTable(wb, sheet = sheet, x = di ,colNames = TRUE, withFilter = FALSE))
#' #     try(openxlsx::saveWorkbook(wb = wb, file = hot_file, overwrite = TRUE) )
#' #   
#' #   }
#' #   
#' # }
#' # 
#' # https://cgiar-my.sharepoint.com/personal/o_benites_cgiar_org/_layouts/15/onedrive.aspx?view=3