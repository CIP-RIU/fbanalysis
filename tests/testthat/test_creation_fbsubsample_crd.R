context("Single fieldbook analysis for AGROFIMS")

test_that("Create fieldbook with sub samples under CRD", {
  
  fb_path <- rprojroot::find_testthat_root_file("dataset/test_crd_2.rds") 
  fb <- readRDS(fb_path)
  #fb<-readRDS("/home/obenites/HIDAP_SB_1.0.0/fbanalysis/tests/testthat/dataset/test_crd_2.rds")
  traits <- "1:Rice_Grain_Plant_density_plant_hill"
  design <- "Completely Randomized Design (CRD)"
  if(design=="Completely Randomized Design (CRD)"){
      factors <- c("PLOT","ROW", "COL", "TREATMENT")  
  }
  #Get traits from UI  
  gather_cols<- names(fb)[stringr::str_detect(string = names(fb), traits)]
  
  if(length(gather_cols)>1){
    #Columns to gather and Select columns
    fb_sub <- fb[,c(factors , names(fb)[stringr::str_detect(names(fb),pattern = traits)] )]   
    #gather_cols <- names(fb_sub)[stringr::str_detect(string = names(fb_sub), traits)]
    ## Transpose data from previous data :fb_sub
    fb_sub <- fb_sub %>% tidyr::gather_("SUBSAMPLE",traits, gather_cols) 
    fb_sub <- fb_sub %>% dplyr::mutate(SUBSAMPLE=gsub(".*__","",fb_sub$SUBSAMPLE))                            
    fb_sub[,traits] <- as.numeric(fb_sub[,traits])
  } else {
    fb_sub <- fb[,c(factors, traits)]
    fb_sub[,traits] <- as.numeric(fb_sub[,traits])
  }
  fb_sub
  
  testthat::expect_equal(nrow(fb_sub),8)
  testthat::expect_equal(names(fb_sub)[ncol(fb_sub)],"1:Rice_Grain_Plant_density_plant_hill")
  testthat::expect_equal(unique(fb_sub$SUBSAMPLE),c("1","2"))
  
})