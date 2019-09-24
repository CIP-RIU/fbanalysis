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
    
    print("paso 0")
    print(inFile$datapath)
    print(inFile$name)
    print("paso 1")
    try({file.copy(from = inFile$datapath, to = file.path("/home/obenites/AGROFIMS/hagrofims/inst/hidap_agrofims/kdx2agro/"))})
    #Import Marie's library
    reticulate::source_python("/home/obenites/agrofims_modules/kdsmart_integration/kdxtoagro.py")
    print("paso 2")
    kdx2agrofims(zip_name="/home/obenites/AGROFIMS/hagrofims/inst/hidap_agrofims/kdx2agro/PURI1567089918,celine_aubert,2019-09-04_102712[1] - Copy.zip", 
                 excel_name= "/home/obenites/AGROFIMS/hagrofims/inst/hidap_agrofims/kdx2agro/CRD_rice.xlsx")
    print("paso 3")
    out <- readxl::read_excel("/home/obenites/AGROFIMS/hagrofims/inst/hidap_agrofims/kdx2agro/CRD_rice.xlsx",
                              sheet = "Crop_measurements") 
    print("paso 4")
    print(out)
    
    ## STABLE CODE
    #file.rename(inFile$datapath, paste(inFile$datapath, ".xlsx", sep=""))
    #out<- readxl::read_excel(paste(inFile$datapath, ".xlsx", sep=""),sheet = "Crop_measurements") 
    names(out)
    out
  })
  
  hot_metadata <- reactive({
    
      inFile <- hot_path_agrofims()
      if(is.null(inFile)) return(NULL)
      file.rename(inFile$datapath, paste(inFile$datapath, ".xlsx", sep=""))
      out<- readxl::read_excel(paste(inFile$datapath, ".xlsx", sep=""),sheet = "Metadata") 
      out
    
  })
  
  ###Update Design (select Input)  ------------------------------------------------------------------
  
  # observe({
  #   
  #   # <- hot_fb_sbase()
  #   design <- try(hot_metadata()$stat_design) #the statistical design from the SweetPotatoBase
  #   nrow_fb <- try(nrow(hot_fb_sbase()$fb))
  #   
  #   if(str_detect(design,"error")){
  #     design <- NULL
  #   }
  #   x <- input$design_single_sbase #the statistical design from the single env UI
  #   
  #   # Can use character(0) to remove all choices
  #   #if (is.null(x)|| length(design)==0){
  #   if (is.null(x) || length(design)==0 ){
  #     
  #     x <- "Completely Randomized Design (CRD)"
  #     shinysky::showshinyalert(session, "alert_single_sbase_done", paste("This study does not have data."), styleclass = "danger")
  #     
  #   } else {
  #     
  #     if(design == "CRD")  {x <- "Completely Randomized Design (CRD)"}
  #     if(design == "RCBD") {x <- "Randomized Complete Block Design (RCBD)"}
  #     if(design == "ABD")  {x <-"Augmented Block Design (ABD)"}
  #     if(design == "Alpha")   {x <-"Alpha design"}
  #     #if(design == "CRD")  {choice_design<-"Factorial Two-Way Design in CRD (F2CRD)"}
  #     #if(design == "CRD")  {choice_design<-"Factorial Two-Way Design in RCBD (F2RCBD)"}
  #     
  #   }
  #   x <- x
  #   
  #   # Can also set the label and select items
  #   updateSelectInput(session, "design_single_sbase",
  #                     label = 'Select statistical design of your experiment',
  #                     choices = x,
  #                     #"Split Plot with Plots in CRD (SPCRD)",
  #                     #"Split Plot with Plots in RCBD (SPRCBD)"),
  #                     selected = x
  #   )
  # })
  
  ## end update design select  ------------------------------------------------------------------
  
  
  output$trt_single_agrofims  <- renderUI({ #genotypes
    selectInput('trt_single_agrofims', 'Select Treatments', c(Choose='', names(hot_fb_agrofims()) ), 
                selectize=TRUE)
  })
  
  output$rep_single_agrofims  <- renderUI({ #repetition
    selectInput('rep_single_agrofims', 'Select Blocks/Replications', c(Choose='', names(hot_fb_agrofims())),
                selectize=TRUE) 
  })
  
  output$trait_single_agrofims <- renderUI({ #trait
    selectInput('trait_single_agrofims', 'Select Trait(s)', c(Choose='', names(hot_fb_agrofims())),
                selectize=TRUE, multiple = TRUE)
  })
  
  output$factor_single_agrofims  <- renderUI({ #factor 1
    selectInput('factor_single_agrofims', 'Select Factors', c(Choose='', names(hot_fb_agrofims()) ),
                selectize=TRUE,multiple = TRUE)
  })
  
  output$block_single_agrofims  <- renderUI({ #block
    selectInput('block_single_agrofims', 'Select Block', c(Choose='', names(hot_fb_agrofims()) ),
                selectize=TRUE)
  })
  
  
  ### SPLIT PLOT ########################################################################################3
  
  output$mainplot_single_agrofims  <- renderUI({ #block
    selectInput('mainplot_factor_agrofims', 'Select Main Plot Factor', c(Choose='', names(hot_fb_agrofims()) ),
                selectize=TRUE)
  })
  
  output$subplot_single_agrofims  <- renderUI({ #block
    selectInput('subplot_factor_agrofims', 'Select Sub Plot Factor', c(Choose='', names(hot_fb_agrofims()) ),
                selectize=TRUE)
  })
  
  output$subsubplot_single_agrofims  <- renderUI({ #block
    selectInput('subsubplot_factor_agrofims', 'Select Sub Sub PLot Factor', c(Choose='', names(hot_fb_agrofims()) ),
                selectize=TRUE)
  })
  ####### END SUBPLOT ##############################################################################33
  
  
  output$k_single_agrofims  <- renderUI({ #block size
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
      
      shiny::withProgress(message = "Opening single Report...",value= 0,{
        
        shiny::incProgress(1/5, detail = paste("Downloading Analysis..."))  
        
        fieldbook <- hot_fb_agrofims()
        fieldbook <- as.data.frame(fieldbook)
        names(fieldbook) <-  stringr::str_replace_all(string= names(fieldbook) , pattern =  "[[:space:]]", replacement = "_")
        
        
        if(is.null(input$trait_single_agrofims)){
          shinyalert("Oops!", "Select trait(s) to perform analysis", type = "error")
        } else{
        
        trait <-  stringr::str_replace_all(string= input$trait_single_agrofims, pattern =  "[[:space:]]", replacement = "_")
        pos<- match(input$trait_single_agrofims,names(fieldbook))
        names(fieldbook)[pos]<- trait #str_replace_all(input$trait_single_agrofims, string = " ", "_")
        
        rep <- input$rep_single_agrofims
        print("repe")
        print(rep)
        
        trt <- input$trt_single_agrofims
        #block <- input$block_single_agrofims
        k <- input$k_single_agrofims #
        #factors <- input$factor_single_agrofims # factor 1
        factors<- stringr::str_replace_all(string= input$factor_single_agrofims, pattern =  "[[:space:]]", replacement = "_")
        
        #factor2_single <- input$factor_single_agrofims2 #factor 2
        
        ###Split plot inputs ####
        
        
        mplot<- input$main_plot_factor
        
        subplot<- input$sub_plot_factor
        subsubplot<- input$subsub_plot_factor
        
        
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
          
          dirName <- fbglobal::get_base_dir()
          print(dirName)
          path <- paste0(dirName, servName, ".docx")
          
          if(is.null(rep)){
            shinyalert("Oops!", "Select blocks/repetitions to perform analysis", type = "error")
            
          } else{
          
          try(pepa::repo.rcbd(traits = trait, trt = trt, rep = rep, format = format, dfr = fieldbook, server =TRUE, server_dir_name = dirName, 
                              server_file_name = servName))
          
          params <- list(
            dataRequest = "uploadFile",
            fileServerName = paste0(servName, ".docx"),
            filedata= httr::upload_file(path, "text/csv")
          )
          
          
          var <- httr::POST(serverService, body=params)
          code <- httr::content(var, "text")
          
          
          }
          
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
          
          try(pepa::repo.crd(traits = trait, trt = trt, trt.lab = "treatment", format = format, dfr = fieldbook, 
                             server =TRUE, server_dir_name = dirName, server_file_name = servName
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
        # 
        if(design == "Factorial with CRD"){
          
          
          if(is.null(factors)){
            shinyalert("Oops!", "Select factors to perform analysis", type = "error")
            
          } else {
          
            if(length(factors)==2){
              servName =  "2fcrd"  
            }else{
              servName =  "factorial"
            }
            
            serverFileDir <-"https://research.cip.cgiar.org/gtdms/hidap/hidap_sbase_reports/files/"
            serverService <-"https://research.cip.cgiar.org/gtdms/hidap/hidap_sbase_reports/getFileUpload.php"
            
            uploadDate  <- as.character(Sys.time(), "%Y%m%d%H%M%S")
            ranStr <-  stringi::stri_rand_strings(1, 15,  '[a-zA-Z0-9]')
            servName <- paste(uploadDate, ranStr, servName , sep= "-") #nombre sin extensions!!!!
            
            dirName <- fbglobal::get_base_dir()
            print(dirName)
            path <- paste0(dirName, servName, ".docx")
            
            pepa::repo.f(dfr = fieldbook, traits = trait, rep= NULL,
                         factors= factors , format = format, 
                         server =TRUE, server_dir_name = dirName, server_file_name = servName )
            
            
            params <- list(
              dataRequest = "uploadFile",
              fileServerName = paste0(servName, ".docx"),
              filedata= httr::upload_file(path, "text/csv")
            )
            
            var <- httr::POST(serverService, body=params)
            code <- httr::content(var, "text")
              
          }  
          
          
          
          if (code == "200")
            print("uploaded")
          else
            print("Not uploaded")
          
        }
        
        if(design == "Factorial with RCBD"){
          
          
          if(is.null(factors)){
            shinyalert("Oops!", "Select factors to perform analysis", type = "error")
            
          } else {
          
            #servName =   "2fcrd"
            if(length(factors)==2){
              servName =  "2frcbd"  
            }else{
              servName =  "factorial"
            }
            serverFileDir <-"https://research.cip.cgiar.org/gtdms/hidap/hidap_sbase_reports/files/"
            serverService <-"https://research.cip.cgiar.org/gtdms/hidap/hidap_sbase_reports/getFileUpload.php"
            
            uploadDate  <- as.character(Sys.time(), "%Y%m%d%H%M%S")
            ranStr <-  stringi::stri_rand_strings(1, 15,  '[a-zA-Z0-9]')
            servName <- paste(uploadDate, ranStr, servName , sep= "-") #nombre sin extensions!!!!
            
            dirName <- fbglobal::get_base_dir()
            print(dirName)
            path <- paste0(dirName, servName, ".docx")
            
            pepa::repo.f(dfr = fieldbook, traits = trait, factors= factors,
                         rep = rep, format = format, 
                         server =TRUE, server_dir_name = dirName, server_file_name = servName )
            
            
            params <- list(
              dataRequest = "uploadFile",
              fileServerName = paste0(servName, ".docx"),
              filedata= httr::upload_file(path, "text/csv")
            )
            
            var <- httr::POST(serverService, body=params)
            code <- httr::content(var, "text")
            
              
          }
          
          if (code == "200")
            print("uploaded")
          else
            print("Not uploaded")
          
        }
     
        if(design == "Split plot Design"){

          #servName =   "2fcrd"
          ##
          servName =  "spld"  
          #}
          #TODO: identificar cuando seleccionan split split plot
          
          
          serverFileDir <-"https://research.cip.cgiar.org/gtdms/hidap/hidap_sbase_reports/files/"
          serverService <-"https://research.cip.cgiar.org/gtdms/hidap/hidap_sbase_reports/getFileUpload.php"
          
          uploadDate  <- as.character(Sys.time(), "%Y%m%d%H%M%S")
          ranStr <-  stringi::stri_rand_strings(1, 15,  '[a-zA-Z0-9]')
          servName <- paste(uploadDate, ranStr, servName , sep= "-") #nombre sin extensions!!!!
          
          dirName <- fbglobal::get_base_dir()
          print(dirName)
          path <- paste0(dirName, servName, ".docx")
          
          mplot<- input$main_plot_factor
          subplot<- input$sub_plot_factor
          subsubplot<- input$subsub_plot_factor
          
          pepa::repo.spld(dfr = fieldbook, traits = trait, 
                        mpf= mplot,spf=subplot ,sspf=subsubplot,
                        rep = rep, format = format, 
                        server =TRUE, server_dir_name = dirName, server_file_name = servName )
          
          
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
        

        }
        
        
        Sys.chmod(path, mode = "0777", use_umask = TRUE)
        
        print(servName)
        download.file(paste0(serverFileDir, servName, ".docx"), con, method = "curl")
        
        incProgress(4/5, detail = paste("Formattting in ", "MS Word",sep= ""))
        incProgress(5/5, detail = paste("Downloading Analysis..."))
        
      })
      
    }
  )
  

}

