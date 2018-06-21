#' Server for Single Environment analysis for HIDAP-AgroFIMS
#'
#' @param input shinyserver input
#' @param output shinyserver output
#' @param session shinyserver session
#' @param values reactive values
#' @importFrom shiny reactive tabPanel renderUI selectInput icon h2 uiOutput radioButtons actionButton br column fluidRow
#' @importFrom shinydashboard infoBox tabBox infoBoxOutput renderInfoBox
#' @importFrom shinyFiles parseFilePaths
#' @import pepa
#' @import st4gi
#' @author Omar Benites
#' @export

single_hdagrofims_server <- function(input, output, session, values){
 
  
  hot_path_agrofims <- reactive ({
    
    #validate(
    #  need(input$file != "", label = "Please enter an XLSX file. XLS files are forbidden")
    #)
    
    if(is.null(input$file_single)==0){return(NULL)}
    if(!is.null(input$file_single_agrofims)>0){
      hot_file <- input$file_single_agrofims
    }
   
  })
  
  hot_fb_agrofims <- reactive({
   
    inFile <- hot_path_agrofims()
    if(is.null(inFile)) return(NULL)
    file.rename(inFile$datapath, paste(inFile$datapath, ".xlsx", sep=""))
    out<- readxl::read_excel(paste(inFile$datapath, ".xlsx", sep=""),sheet = "Fieldbook") 
    names(out)
    out
  })
  
  output$genotypes_single_agrofims  <- renderUI({
    selectInput('genotypes_single_agrofims', 'Select Genotypes', c(Choose='', names(hot_fb_agrofims()) ), 
                selectize=TRUE)
  })
  
  output$rep_single_agrofims  <- renderUI({
    selectInput('rep_single_agrofims', 'Select Replications', c(Choose='', names(hot_fb_agrofims())),
                selectize=TRUE)
  })
  
  output$trait_single_agrofims <- renderUI({
    selectInput('trait_single_agrofims', 'Select Trait(s)', c(Choose='', names(hot_fb_agrofims())),
                selectize=TRUE, multiple = TRUE)
  })
  
  output$factor_single_agrofims  <- renderUI({
    selectInput('factor_single_agrofims', 'Select Factor', c(Choose='', names(hot_fb_agrofims()) ),
                selectize=TRUE)
  })
  
  output$block_single_agrofims  <- renderUI({
    selectInput('block_single_agrofims', 'Select Block', c(Choose='', names(hot_fb_agrofims()) ),
                selectize=TRUE)
  })
  
  output$k_single_agrofims  <- renderUI({
    shiny::numericInput('k_single_agrofims', 'Select Block Size',   value =2, min=2, max = 100)
  })    
  
  output$file_message_single_agrofims <- renderInfoBox({
    
    hot_file <- hot_path_agrofims()
    p1 <<- hot_path_agrofims()
    #print(hot_file)
    if(is.null(hot_file)){
      shinydashboard::infoBox(title="Select fieldbook file", subtitle=
                paste("Choose your fieldbook file"), icon = icon("upload", lib = "glyphicon"),
              color = "blue",fill = TRUE, width = NULL)
    } else {
      hot_file <- hot_file$name
      #hot_file <- paste(hot_file, collapse = ", ")
      shinydashboard::infoBox(title="GREAT!", subtitle = paste(" Fieldbook selected: ", hot_file),  
                              icon = icon("ok", lib = "glyphicon"),
                              color = "green",fill = TRUE, width = NULL)
    }
    
  })
  
  
  output$downloadagrofims_single_report <- downloadHandler(
    filename =  paste0("report", "_", as.character(Sys.time(), "%Y%m%d%H%M%S"), '.docx'),
    content = function(con) {
      #content = function(file) {  
      
      shiny::withProgress(message = "Opening single Report...",value= 0,{
        
        shiny::incProgress(1/5, detail = paste("Downloading Analysis..."))  
        
        fieldbook <- hot_fb_agrofims()
        fieldbook <- as.data.frame(fieldbook)
        
        #treatments
        trait <- input$trait_single_agrofims
        rep <- input$rep_single_agrofims
        genotypes <- input$genotypes_single_agrofims
        block <- input$block_single_agrofims
        k <- input$k_single_agrofims
        factor_single <- input$factor_single_agrofims
        
        #type of document
        format <- "word" #paste(input$format_single_agrofims, sep="")
        design <- input$design_single_agrofims
        
        
        incProgress(2/5, detail = paste("Downloading Analysis..."))
        
        if(design == "Randomized Complete Block Design (RCBD)"){
          
          
          servName =   "rcbd"
          serverFileDir <-"https://research.cip.cgiar.org/gtdms/hidap/hidap_sbase_reports/files/"
          serverService <-"https://research.cip.cgiar.org/gtdms/hidap/hidap_sbase_reports/getFileUpload.php"
          
          uploadDate  <- as.character(Sys.time(), "%Y%m%d%H%M%S")
          ranStr <-  stringi::stri_rand_strings(1, 15,  '[a-zA-Z0-9]')
          servName <- paste(uploadDate, ranStr, servName , sep= "-") #nombre sin extensions!!!!
          
          #dirfiles <- system.file(package = "pepa")
          
          dirName <- fbglobal::get_base_dir()
          print(dirName)
          path <- paste0(dirName, servName, ".docx")
          
          #print(path)
          
          try(pepa::repo.rcbd(traits = trait, geno = genotypes, rep = rep, format = format, data = fieldbook, server =TRUE, server_dir_name = dirName, 
                              server_file_name = servName))
          #path<- "~/R/x86_64-pc-linux-gnu-library/3.4/pepa/rmd/rcbd.docx" #rsconnect cip
          
          params <- list(
            dataRequest = "uploadFile",
            fileServerName = paste0(servName, ".docx"),
            filedata= httr::upload_file(path, "text/csv")
          )
          
          var <- httr::POST(serverService, body=params)
          code <- httr::content(var, "text")
          
          
          
          if (code == "200")
            print("uploaded")
          else
            print("Not uploaded")
          
          
        }
        
        
        
        
        if(design == "Completely Randomized Design (CRD)"){
          
          
          #servName =   "crd.docx"
          servName =   "crd"
          serverFileDir <-"https://research.cip.cgiar.org/gtdms/hidap/hidap_sbase_reports/files/"
          serverService <-"https://research.cip.cgiar.org/gtdms/hidap/hidap_sbase_reports/getFileUpload.php"
          
          uploadDate  <- as.character(Sys.time(), "%Y%m%d%H%M%S")
          ranStr <-  stringi::stri_rand_strings(1, 15,  '[a-zA-Z0-9]')
          servName <- paste(uploadDate, ranStr, servName , sep= "-") #nombre sin extensions!!!!
          
          #dirfiles <- system.file(package = "pepa")
          
          dirName <- fbglobal::get_base_dir()
          path <- paste0(dirName, servName, ".docx")
          
          print(path)
          
          try(pepa::repo.crd(traits = trait, geno = genotypes, format = format, data = fieldbook, server =TRUE, server_dir_name = dirName, 
                             server_file_name = servName
          ))
          
          params <- list(
            dataRequest = "uploadFile",
            fileServerName = paste0(servName, ".docx"),
            filedata= httr::upload_file(path, "text/csv")
          )
          
          var <- httr::POST(serverService, body=params)
          code <- httr::content(var, "text")
          
          
          
          if (code == "200")
            print("uploaded")
          else
            print("Not uploaded")
          
        }
        
        
        if(design == "Augmented Block Design (ABD)"){
          
          
          servName =   "abd"
          
          serverFileDir <-"https://research.cip.cgiar.org/gtdms/hidap/hidap_sbase_reports/files/"
          serverService <-"https://research.cip.cgiar.org/gtdms/hidap/hidap_sbase_reports/getFileUpload.php"
          
          
          uploadDate  <- as.character(Sys.time(), "%Y%m%d%H%M%S")
          ranStr <-  stringi::stri_rand_strings(1, 15,  '[a-zA-Z0-9]')
          servName <- paste(uploadDate, ranStr, servName , sep= "-") #nombre sin extensions!!!!
          
          #dirfiles <- system.file(package = "pepa")
          
          dirName <- fbglobal::get_base_dir()
          path <- paste0(dirName, servName, ".docx")
          
          print(path)
          
          
          
          try(pepa::repo.abd(traits = trait, geno = genotypes, rep = rep, format = format, data = fieldbook, server =TRUE, server_dir_name = dirName, 
                             server_file_name = servName))
          
          
          params <- list(
            dataRequest = "uploadFile",
            fileServerName = paste0(servName, ".docx"),
            filedata= httr::upload_file(path, "text/csv")
          )
          
          var <- httr::POST(serverService, body=params)
          code <- httr::content(var, "text")
          
          
          
          if (code == "200")
            print("uploaded")
          else
            print("Not uploaded")
          
          
          #try(pepa::repo.abd(traits = trait, geno = genotypes, rep = rep, format = format, data = fieldbook))
          #path <- "/usr/local/lib/R/site-library/pepa/rmd/abd.docx" # # former shiny server CIP-RIU
          #path<- "~/R/x86_64-pc-linux-gnu-library/3.4/pepa/rmd/abd.docx" #rsconnect cip
          #tempReport <- file.path(tempdir(), "abd.docx") 
          #file.copy("/usr/local/lib/R/site-library/pepa/rmd/abd.docx", con)
          
          
          
        }
        # 
        # 
        if(design == "Alpha design"){
          
          
          servName =   "a01d"
          serverFileDir <-"https://research.cip.cgiar.org/gtdms/hidap/hidap_sbase_reports/files/"
          serverService <-"https://research.cip.cgiar.org/gtdms/hidap/hidap_sbase_reports/getFileUpload.php"
          
          
          uploadDate  <- as.character(Sys.time(), "%Y%m%d%H%M%S")
          ranStr <-  stringi::stri_rand_strings(1, 15,  '[a-zA-Z0-9]')
          servName <- paste(uploadDate, ranStr, servName , sep= "-") #nombre sin extensions!!!!
          
          #dirfiles <- system.file(package = "pepa")
          
          dirName <- fbglobal::get_base_dir()
          path <- paste0(dirName, servName, ".docx")
          
          print(path)
          
          
          
          
          #try(pepa::repo.abd(traits = trait, geno = genotypes, format = format, data = fieldbook))
          try(pepa::repo.a01d(traits = trait, geno = genotypes, rep = rep, block = block, k = k, data = fieldbook, format = format, server =TRUE, server_dir_name = dirName, 
                              server_file_name = servName))
          
          #path<- "~/R/x86_64-pc-linux-gnu-library/3.4/pepa/rmd/a01d.docx"
          #tempReport <- file.path(tempdir(), "a01d.docx")
          #path <- "/usr/local/lib/R/site-library/pepa/rmd/a01d.docx"
          #file.copy("/usr/local/lib/R/site-library/pepa/rmd/a01d.docx", con)
          
          params <- list(
            dataRequest = "uploadFile",
            fileServerName = paste0(servName, ".docx"),
            filedata= httr::upload_file(path, "text/csv")
          )
          
          var <- httr::POST(serverService, body=params)
          code <- httr::content(var, "text")
          
          
          
          if (code == "200")
            print("uploaded")
          else
            print("Not uploaded")
          
          
        }
        # 
        # 
        # if(design == "Split Plot with Plots in CRD (SPCRD)"){
        #   title <- paste("Automatic report for ", design, sep= "")
        #   try(pepa::repo.2f(traits = trait, A = genotypes, B = factor_single, rep = rep, design = "crd",  title= title, data = fieldbook, format = format))
        #   path <- "/usr/local/lib/R/site-library/pepa/rmd/2fcrd.docx"
        #   #file.copy("/usr/local/lib/R/site-library/pepa/rmd/2fcrd.docx", con)
        # }
        # 
        # 
        # if(design == "Factorial Two-Way Design in CRD (F2CRD)"){
        #   title <- paste("Automatic report for ", design, sep= "")
        #   try(pepa::repo.2f(traits = trait, A = genotypes, B = factor_single, rep = rep, design = "crd",  title= title, data = fieldbook, format = format))
        #   path <- "/usr/local/lib/R/site-library/pepa/rmd/2fcrd.docx"
        #   #file.copy("/usr/local/lib/R/site-library/pepa/rmd/2fcrd.docx", con)
        # }
        # 
        # 
        # if(design == "Split Plot with Plots in RCBD (SPRCBD)"){
        #   title <- paste("Automatic report for ", design, sep= "")
        #   try(pepa::repo.2f(traits = trait, A = genotypes, B = factor_single, rep = rep, design = "rcbd", title= title, data = fieldbook, format = format))
        #   path <- "/usr/local/lib/R/site-library/pepa/rmd/2frcbd.docx"
        #   #file.copy("/usr/local/lib/R/site-library/pepa/rmd/2frcbd.docx", con)
        # }
        # 
        # 
        # if(design == "Factorial Two-Way Design in RCBD (F2RCBD)"){
        #   
        #   title <- paste("Automatic report for ", design, sep= "")
        #   try(pepa::repo.2f(traits = trait, A = genotypes, B = factor_single, rep = rep, design = "rcbd", title= title, data = fieldbook, format = format))
        #   path <- "/usr/local/lib/R/site-library/pepa/rmd/2frcbd.docx"
        #   #file.copy("/usr/local/lib/R/site-library/pepa/rmd/2frcbd.docx", con)
        # }
        # 
        
        
        Sys.chmod(path, mode = "0777", use_umask = TRUE)
        
        #print(paste0(serverFileDir, servName, ".docx"))
        print(servName)
        # file.copy(paste0(serverFileDir, servName) , con, overwrite = TRUE)
        download.file(paste0(serverFileDir, servName, ".docx"), con, method = "curl")
        
        incProgress(4/5, detail = paste("Formattting in ", "MS Word",sep= ""))
        incProgress(5/5, detail = paste("Downloading Analysis..."))
        
      })
      
    }
  )
  
  
  
  
  
  
  
  
}

