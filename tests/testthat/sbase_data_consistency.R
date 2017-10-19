
# SweetPotato Base values -------------------------------------------------
context("Values of sweetpotatobase data")


test_that("Experimental design", {
  
  library(brapi)
  white_list <- brapi::ba_db()
  #establish connection
  sp_base_credentials <- white_list$sweetpotatobase
  trial_table <- brapi::ba_trials(con = sp_base_credentials)
  out <- list(sp_base_credentials  = sp_base_credentials , trial_table = trial_table)
  
  sbase_fb <- out$trial_table
  credentials <- out$sp_base_credentials
  
  aselProgram <- "Ghana"
  atrialName  <- "2010"
  astudyName  <- "2010ASPGH_MEGAclone10-POKUASE"
  
  col_fb_sbase <- sbase_fb %>% dplyr::filter(programName== aselProgram, trialName == atrialName, studyName == astudyName)
  dt <-  ba_studies_table(credentials , studyDbId = as.character(col_fb_sbase$studyDbId))
  
  design <- unique(dt$studyDesign)
  testthat::expect_equal(design, "CRD")
  
  
})

test_that("Study (field book) without design", {
  
  library(brapi)
  white_list <- brapi::ba_db()
  #establish connection
  sp_base_credentials <- white_list$sweetpotatobase
  trial_table <- brapi::ba_trials(con = sp_base_credentials)
  out <- list(sp_base_credentials  = sp_base_credentials , trial_table = trial_table)
  
  sbase_fb <- out$trial_table
  credentials <- out$sp_base_credentials
  
  aselProgram <- "Uganda"
  atrialName  <- "NKB FLOWERING TRIAL"
  astudyName  <- "NKB FLOWERING TRIAL_AbiZARDI"
  
  col_fb_sbase <- sbase_fb %>% dplyr::filter(programName== aselProgram, trialName == atrialName, studyName == astudyName)
  dt <-  ba_studies_table(credentials , studyDbId = as.character(col_fb_sbase$studyDbId))
  
  design <- unique(dt$studyDesign)
  testthat::expect_equal(length(design), 0)
  print("Study (field book) without design")
})

test_that("Study (field book) without data", {
 
  library(brapi)
  white_list <- brapi::ba_db()
  #establish connection
  sp_base_credentials <- white_list$sweetpotatobase
  trial_table <- brapi::ba_trials(con = sp_base_credentials)
  out <- list(sp_base_credentials  = sp_base_credentials , trial_table = trial_table)
  
  sbase_fb <- out$trial_table
  credentials <- out$sp_base_credentials
  
  aselProgram <- "Uganda"
  atrialName  <- "NKB FLOWERING TRIAL"
  astudyName  <- "NKB FLOWERING TRIAL_AbiZARDI"
  
  col_fb_sbase <- sbase_fb %>% dplyr::filter(programName== aselProgram, trialName == atrialName, studyName == astudyName)
  dt <-  ba_studies_table(credentials , studyDbId = as.character(col_fb_sbase$studyDbId))
  
  testthat::expect_equal(nrow(nrowdt), 0)
  print("Study (field book) without data")
})




