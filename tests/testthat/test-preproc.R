context("Test for field book pre-proccessing functions")

test_that("Check traits in field book", {
  
  fb_path <- rprojroot::find_testthat_root_file("dataset/test_crd_2.rds") 
  fb <- readRDS(fb_path)
  out <- get_fbtrait(trait_names = names(fb))
  testthat::expect_equal(length(out), 2)

})

test_that("Check traits in field book with Trends", {
  
  fb_path <- rprojroot::find_testthat_root_file("dataset/test_crd_2.rds") 
  fb <- readRDS(fb_path)
  out <- get_fbtrait(trait_names = names(fb),trend = TRUE)
  testthat::expect_equal(length(out), 1)
  
})


test_that("Check reshape form for trend analysis",{
  
  fb_path <- rprojroot::find_testthat_root_file("dataset/test_crd_2.rds") 
  fb <- readRDS(fb_path)
  out <- get_fbsubsample(fb,trait = "Rice_Grain_Plant_density_plant_hill",trend = TRUE)
  testthat::expect_equal(nrow(out), 16)
})


test_that("Check incorporation of dates in trend analysis",{
  
  fb_path <- rprojroot::find_testthat_root_file("dataset/CRD_PURI1567089918.xlsx") 
  fb <- read_xlsx(fb_path, sheet = "Crop_measurements") %>% as.data.frame(stringsAsFactors=FALSE)
  fb <- clean_fb(fb)
  
  out <- get_fbsubsample(fb,trait = "Rice_Grain_Plant_density_plant_hill",trend = TRUE)
  testthat::expect_equal(nrow(out), 16)
  
  
})
          
          