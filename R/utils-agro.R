#' Get file direcction of agrofim's files.
#' 
#' @param path file path
#' @param file_name file name. The file name is provided by AGROFIMS after downloading the fb file. Ex. PURI1567089918
#' @description 
#' @export
#' 
get_agrofims_file <- function(path = "/home/obenites/AGROFIMS/kdsmart/", file_name="PURI1567089918"){
  
  #file_name <- gsub(file_name,pattern = ".zip", "")
  out_file <- list.files(path = path,pattern = paste0(file_name,"-"), full.names = TRUE)
  out_file <- out_file[str_detect(string = out_file,pattern = ".xlsx")][1]
  # kdx2agrofims(
  #   zip_name="/home/obenites/AGROFIMS/hagrofims/inst/hidap_agrofims/kdx2agro/PURI1567089918,celine_aubert,2019-09-04_102712[1] - Copy.zip", 
  #   excel_name= "/home/obenites/AGROFIMS/hagrofims/inst/hidap_agrofims/kdx2agro/CRD_rice.xlsx")
  # 
}