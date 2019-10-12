
#' Fieldbook cleaner for AGROFIMS DATA
#' @param fb data.frame. Field book data
#' @description Clean headers,special characters like "/", whitespaces. In addition, it remove TIMESTAMP columns and empty traits.
#' @importFrom purrr map
#' @importFrom stringr str_replace_all str_detect
#' @author Omar Benites
#' @export

clean_fb <- function(fb, trend= FALSE){
   #Detect and remove empty columns
   #wd: with data (at least one value)
   wd <-purrr::map(.x = fb, function(x) (all(!is.na(x)==TRUE)) )
   fb <- fb[,unlist(wd)]
   #Remove TIMESTAMP columns
   #TODO: GENERALIZE 
   
   if(!trend){
      fb<- fb[,!stringr::str_detect(names(fb),"TIMESTAMP_")]
   }
   print("paso 3")
   #fb <- fb %>% as.data.frame(stringsAsFactors=FALSE)
   #Change whitespaces with "_"
   names(fb) <-  stringr::str_replace_all(string= names(fb) , pattern =  "[[:space:]]", replacement = "_")
   #Change // for "_" to readable traits
   names(fb) <- gsub("([//])","_", names(fb))
   #names(fb) <- gsub("\\(","", names(fb))
   #names(fb) <- gsub("\\)","", names(fb))
   ### END FOR TEST
   print("paso 4")
   fb<- fb %>% as.data.frame(stringsAsFactors=FALSE)
   fb
}


#' Get factors provided by different designs
#' 
#' @param fb data.frame. Field book data
#' @description character vector. Get factors according to the statistical design
#' @author Omar Benites
#' @export

get_design_factors <- function(design = "Completely Randomized Design (CRD)"){
   
   #if(design == "UNDR")   {out <- "Unreplicated Design with No Randomization (UNDR)"  }
   if(design == "Randomized Complete Block Design (RCBD)")  {out <- c("PLOT","ROW", "COL", "TREATMENT","BLOCK" )  }
   if(design == "Completely Randomized Design (CRD)")       {out <- c("PLOT","ROW", "COL", "TREATMENT" ) }
   if(design == "Factorial with CRD")   {out <- c("PLOT","ROW", "COL", "TREATMENT" ) }
   if(design == "Factorial with RCBD")  {out <- c("PLOT","ROW", "COL", "TREATMENT" ) }
   if(design == "Split Plot with Plots Design")  {out <- c("PLOT","ROW", "COL", "TREATMENT" ) } # #R.Eyzaguirre recommend to use just one split design under rcbd
   if(design == "Split-Splot Plot Design")  {out <- c("PLOT","ROW", "COL", "TREATMENT" ) } # #R.Eyzaguirre recommend to use just one split design under rcbd
   #if(design == "---")  {out <- "-----"}
   # if(design == "STRIP")  {out <- "Strip Plot Design (STRIP)"}
   # if(design == "FCRD")   {out <- "Factorial with CRD"}
   # if(design == "FRCBD")  {out <- "Factorial with RCBD"}
   # if(design == "AD")     {out <- "Alpha Design(0,1) (AD)"}
   # if(design == "WD")     {out <- "Westcott Design (AD)"}
   
   out
   
}


#' Get Fieldbook with subsample data according to the traits
#' 
#' @param fb data.frame. Field book data
#' @param design character Whole statistical design name
#' @param trait character Trait name in the field book
#' @description Clean headers, 
#' @importFrom purrr map
#' @importFrom tidyr gather_
#' @importFrom dplyr mutate
#' @importFrom stringr str_replace_all str_detect
#' @author Omar Benites
#' @export

get_fbsubsample <- function(fb, design="Completely Randomized Design (CRD)", trait, trend= FALSE, trend_col=NULL ){
   
   design <- design
   factors <- get_design_factors(design)
   
   #Get traits from UI
   gather_cols<- names(fb)[stringr::str_detect(string = names(fb), trait)]
   #if(!trend){ #if not trend, remove timestamp column
   gather_cols<- gather_cols[!stringr::str_detect(gather_cols,"TIMESTAMP_")]
   #}
   
   
   if(length(gather_cols)>1){
      #Columns to gather and Select columns
      fb_sub <- fb[,c(factors , names(fb)[stringr::str_detect(names(fb),pattern = trait)] )]
      fb_sub <- fb_sub[, !stringr::str_detect(names(fb_sub),"TIMESTAMP_")]
      
      #gather_cols <- names(fb_sub)[stringr::str_detect(string = names(fb_sub), trait)]
      ## Transpose data from previous data :fb_sub
    
      #gather_cols <- names(fb_sub)[stringr::str_detect(string = names(fb_sub), trait)]
      ## Transpose data from previous data :fb_sub
      fb_sub <- fb_sub %>% tidyr::gather_("SUBSAMPLE",trait, gather_cols)
      if(trend){
         fb_sub <- fb_sub %>% tidyr::separate(col = "SUBSAMPLE",into =c("DATE", "SUBSAMPLE"), sep=":")
         #fb_sub$DATE <- trend_col #replace for date data
         fb_sub$DATE <- as.double(fb_sub$DATE)
         fb_sub <- fb_sub %>% dplyr::select(-DATE, dplyr::everything())
         ncol_fb_sub<- ncol(fb_sub)
         ncol_fb_sub<- c(1:(ncol_fb_sub-2), ncol_fb_sub, ncol_fb_sub-1)
         fb_sub<- fb_sub[,ncol_fb_sub]
      }
      
      
      fb_sub <- fb_sub %>% dplyr::mutate(SUBSAMPLE=gsub(".*__","",fb_sub$SUBSAMPLE))                            
      fb_sub[,trait] <- as.numeric(fb_sub[,trait])
   } 
   else {
      fb_sub <- fb[,c(factors, trait)]
      fb_sub[,trait] <- as.numeric(fb_sub[,trait])
   }
   fb_sub
}


#' @export
#'
get_fbtrait <- function(trait_names, trend=FALSE){
   #TODO: GENERALIZE FOR ALL FACTORS IN EXP. DESIGN
   
    out <- setdiff(trait_names,c("PLOT","ROW","COL","TREATMENT")) %>%  
            stringr::str_replace_all(.,"__[:digit:]+","") %>% unique()
   if(trend){
      out <-  gsub(".*:", "", out) %>% unique()
   }               
   out            
           
}


#' @export
#
get_fbtrait_trend <- function(trait_names, trend=FALSE){
   #TODO: GENERALIZE FOR ALL FACTORS IN EXP. DESIGN
   
   out <- setdiff(trait_names,c("PLOT","ROW","COL","TREATMENT")) %>%  
      stringr::str_replace_all(.,"__[:digit:]+","") %>% unique()
   out <-  gsub(".*:", "", out) %>% unique()
   out            
   
}



# Get Fieldbook with subsample data according to the traits
# 
# @param fb data.frame. Field book data
# @param design character Whole statistical design name
# @param trait character Trait name in the field book
# @description Clean headers,
# @importFrom purrr map
# @importFrom tidyr gather_
# @importFrom dplyr mutate
# @importFrom stringr str_replace_all str_detect
# @author Omar Benites
# @export
# 
# get_fbtrend <- function(fb, design){
#     
#     
#     
#     
#  }
