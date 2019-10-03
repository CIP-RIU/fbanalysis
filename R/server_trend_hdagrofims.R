#' Server for Trend Environment analysis for HIDAP-AgroFIMS
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

trend_hdagrofims_server <- function(input, output, session, values){
  
  
  hot_path_agrofims <- reactive ({
    
    #validate(
    #  need(input$file != "", label = "Please enter an XLSX file. XLS files are forbidden")
    #)
    
    if(is.null(input$file_trend)==0){return(NULL)}
    if(!is.null(input$file_trend_agrofims)>0){
      hot_file <- input$file_trend_agrofims
    }
    
  })
  
  hot_fb_agrofims <- reactive({
    
    inFile <- hot_path_agrofims()
    if(is.null(inFile)) return(NULL)
    print(inFile$datapath)
    print(inFile$name)
    try({
      file.copy(from = inFile$datapath, 
                   to = file.path("/home/obenites/AGROFIMS/hagrofims/inst/hidap_agrofims/kdx2agro/import-kdxfiles/kdxfiles/"),
                   overwrite = TRUE)
      
      file.rename(from = paste0("/home/obenites/AGROFIMS/hagrofims/inst/hidap_agrofims/kdx2agro/import-kdxfiles/kdxfiles/0.zip"),
                  to = paste0("/home/obenites/AGROFIMS/hagrofims/inst/hidap_agrofims/kdx2agro/import-kdxfiles/kdxfiles/",inFile$name))
                   
    })
    #Import Marie's library
    #reticulate::source_python("/home/obenites/agrofims_modules/kdsmart_integration/kdxtoagro.py")
    reticulate::source_python("/home/obenites/AGROFIMS/hagrofims/inst/hidap_agrofims/kdx2agro/import-kdxfiles/kdxfiles/kdxtoagro.py")
    #kdx2agrofims(zip_name="/home/obenites/AGROFIMS/hagrofims/inst/hidap_agrofims/kdx2agro/PURI1567089918,celine_aubert,2019-09-04_102712[1] - Copy.zip", 
    kdx2agrofims(zip_name=paste0("/home/obenites/AGROFIMS/hagrofims/inst/hidap_agrofims/kdx2agro/import-kdxfiles/kdxfiles/",inFile$name), 
                 excel_name = get_agrofims_file( "/home/obenites/AGROFIMS/kdsmart/", file_name = strsplit(inFile$name,split = ",")[[1]][1])
                 #excel_name= "/home/obenites/AGROFIMS/hagrofims/inst/hidap_agrofims/kdx2agro/CRD_rice.xlsx")
                 #excel_name= "/home/obenites/AGROFIMS/kdsmart/"
                 )
              
    
    print("paso 3")
    
    #### FOR DEPLOYMENT #########
    #PURI1567089918,celine_aubert,2019-09-04_102712[1] - Copy.zip"
    fname <- strsplit(inFile$name,split = ",")[[1]][1]
    #fb <- readxl::read_excel("/home/obenites/AGROFIMS/hagrofims/inst/hidap_agrofims/PURI1567089918.xlsx",
    fb <- readxl::read_excel(paste0("/home/obenites/AGROFIMS/hagrofims/inst/hidap_agrofims/",fname,".xlsx"),
                             sheet = "Crop_measurements")
    ### END FOR DEPLOYRMENT #####
    fb <- clean_fb(fb,trend = TRUE)
    
    #### FOR TES #####
    #fb <- readxl::read_excel("/home/obenites/HIDAP_SB_1.0.0/fbanalysis/inst/app_trend/PURI1567089918.xlsx",
    #                           sheet = "Crop_measurements")
    #Detect and remove empty columns
    # isFilled <-purrr::map(.x = fb, function(x) (all(!is.na(x)==TRUE)) )
    # fb <- fb[,unlist(isFilled)]
    # #Remove TIMESTAMP columns
    # fb<- fb[,!stringr::str_detect(names(fb),"TIMESTAMP_")]
    # #fb <- fb %>% as.data.frame(stringsAsFactors=FALSE)
    # #Change whitespaces with "_"
    # names(fb) <-  stringr::str_replace_all(string= names(fb) , pattern =  "[[:space:]]", replacement = "_")
    # #Change // for "_" to readable traits
    # names(fb) <- gsub("([//])","_", names(fb))
    # ### END FOR TEST
    # print("paso 4")
    # fb<- fb %>% as.data.frame(stringsAsFactors=FALSE)
    
  })
  
  # hot_fbsample_agrofims <- reactive({
  #   
  #   #Field book
  #   fb <- hot_fb_agrofims()
  #   #Metadata
  #   design <- input$design_trend_agrofims
  #   
  #   if(design=="Completely Randomized Design (CRD)"){
  #     factors <- c("PLOT","ROW", "COL", "TREATMENT" )  
  #   }
  #   
  #   selected_variables <- input$trait_trend_agrofims
  #   print(selected_variables)
  #   
  #   ##BEGIN TEST
  #   isFilled <-purrr::map(.x = fb, function(x) (all(!is.na(x)==TRUE)) )
  #   fb <- fb[,unlist(isFilled)]
  #   fb <- fb[,!stringr::str_detect(names(fb),"TIMESTAMP_")]
  #   
  #   clean_variables <- stringr::str_replace_all(selected_variables,".__[:digit:]+","")
  #   clean_variables <-  gsub(".*:", "", clean_variables) %>% unique()
  #   
  #   
  #   if(length(clean_variables)>1){
  #     
  #     fb <- data.frame()
  #     
  #     #TODO: SHOW ERROR IF USER SELECT OTHER VARIABLE THAT IS NOT PART OF THE TRAIT
  #     
  #   } 
  #   else {
  #     
  #     fieldbook <- fb  
  #     seasons <- unique(gsub(":.*", "", selected_variables))
  #     fieldbook <- fieldbook[,c(factors,selected_variables)]
  #     fb_list <- list()
  #     for(i in seq.int(seasons)){
  #       fb_list[[i]] <- fieldbook[,c(factors,selected_variables[stringr::str_detect(string = selected_variables, paste0(seasons[i],":"))])]
  #       fb_list[[i]] <- fb_list[[i]] %>% tidyr::gather_("SUBSAMPLE", paste0("season_",i,"_",clean_variables) , 
  #                                                       selected_variables[stringr::str_detect(string = selected_variables, paste0(seasons[i],":"))] )
  #       fb_list[[i]] <- fb_list[[i]] %>% dplyr::mutate(SUBSAMPLE=gsub(".*__","",fb_list[[i]]$SUBSAMPLE)) 
  #     }
  #     
  #     out <- fb_list[[1]]
  #     if(length(fb_list)==1){
  #       out <- out
  #     } else {
  #       for(i in 2:length(fb_list)){
  #         out <- dplyr::left_join(out,fb_list[[i]])
  #       }
  #     }
  #     names(out) <- gsub("([//])","_", names(out))
  #     out[,6] <- as.numeric(out[,6])
  #   }
  #   out
  # })
  
  col_trend_dates <- reactive({
    fb <- hot_fb_agrofims()
    lookup <- paste0("TIMESTAMP_[:digit:]+:",input$trait_trend_agrofims)
    var_dates <- names(fb)[stringr::str_detect(string = names(fb), pattern =  lookup)] 
    fb_dates <- fb[, var_dates] %>% tidyr::gather("VARIABLE","DATE", var_dates)
    out <- as.Date(fb_dates$DATE)
  })
  
  hot_fbsample_agrofims <- reactive({
    
    design <- input$design_trend_agrofims
    trait <- input$trait_trend_agrofims
    fb<- hot_fb_agrofims()
    
    fb <- get_fbsubsample(fb=fb, design=design, trait=trait,trend = TRUE, trend_col= col_trend_dates())

  })
  
  hot_traits <- reactive({
    traits <- names(hot_fb_agrofims()[,stringr::str_detect(names(hot_fb_agrofims()),"__")])
    traits <- stringr::str_replace_all(traits,pattern = "__[:digit:]+","") %>% unique()
    traits
  })
  
  hot_treatment <- reactive({
    treatment <- names(hot_fb_agrofims()[,!stringr::str_detect(names(hot_fb_agrofims()),"__")])
    treatment
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
  #   x <- input$design_trend_sbase #the statistical design from the trend env UI
  #   
  #   # Can use character(0) to remove all choices
  #   #if (is.null(x)|| length(design)==0){
  #   if (is.null(x) || length(design)==0 ){
  #     
  #     x <- "Completely Randomized Design (CRD)"
  #     shinysky::showshinyalert(session, "alert_trend_sbase_done", paste("This study does not have data."), styleclass = "danger")
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
  #   updateSelectInput(session, "design_trend_sbase",
  #                     label = 'Select statistical design of your experiment',
  #                     choices = x,
  #                     #"Split Plot with Plots in CRD (SPCRD)",
  #                     #"Split Plot with Plots in RCBD (SPRCBD)"),
  #                     selected = x
  #   )
  # })
  
  ## end update design select  ------------------------------------------------------------------
  
  
  output$trt_trend_agrofims  <- renderUI({ #genotypes
    #selectInput('trt_trend_agrofims', 'Select Treatments', c(Choose='', names(hot_fb_agrofims()) ), 
    selectInput('trt_trend_agrofims', 'Select Treatments', c(Choose='', hot_treatment()), 
                selectize=TRUE)
  })
  
  output$rep_trend_agrofims  <- renderUI({ #repetition
    selectInput('rep_trend_agrofims', 'Select Blocks/Replications', c(Choose='', names(hot_fb_agrofims())),
                selectize=TRUE) 
  })
  
  output$eu_trend_agrofims  <- renderUI({ #repetition
    selectInput('eu_trend_agrofims', 'Select experimental units', c(Choose='', hot_treatment()),
                selectize=TRUE) 
  })
  
  # output$stat_trend_agrofims  <- renderUI({ #repetition
  #   shinyWidgets::awesomeCheckboxGroup( inputId = "stat_trend_agrofims",label = "Statistical options", choices = c("Standad error", "Mean"),
  #     inline = TRUE, status = "primary"
  #   )
  # })
  
  output$trait_trend_agrofims <- renderUI({ #trait
    #selectInput('trait_trend_agrofims', 'Select Trait(s)', c(Choose='', names(hot_fb_agrofims())),
    selectInput('trait_trend_agrofims', 'Select Trait(s)', c(Choose='', get_fbtrait(names(hot_fb_agrofims()),trend = TRUE)),
                selectize=TRUE, multiple = FALSE)
  })
  
  output$factor_trend_agrofims  <- renderUI({ #factor 1
    selectInput('factor_trend_agrofims', 'Select Factors', c(Choose='', names(hot_fb_agrofims())),
                selectize=TRUE,multiple = TRUE)
  })
  
  output$block_trend_agrofims  <- renderUI({ #block
    selectInput('block_trend_agrofims', 'Select Block', c(Choose='', names(hot_fb_agrofims()) ),
                selectize=TRUE)
  })
  
  
  ### SPLIT PLOT ########################################################################################3
  
  output$mainplot_trend_agrofims  <- renderUI({ #block
    selectInput('mainplot_factor_agrofims', 'Select Main Plot Factor', c(Choose='', names(hot_fb_agrofims()) ),
                selectize=TRUE)
  })
  
  output$subplot_trend_agrofims  <- renderUI({ #block
    selectInput('subplot_factor_agrofims', 'Select Sub Plot Factor', c(Choose='', names(hot_fb_agrofims()) ),
                selectize=TRUE)
  })
  
  output$subsubplot_trend_agrofims  <- renderUI({ #block
    selectInput('subsubplot_factor_agrofims', 'Select Sub Sub PLot Factor', c(Choose='', names(hot_fb_agrofims()) ),
                selectize=TRUE)
  })
  ####### END SUBPLOT ##############################################################################33
  
  
  output$k_trend_agrofims  <- renderUI({ #block size
    shiny::numericInput('k_trend_agrofims', 'Select Block Size',   value =2, min=2, max = 100)
  })    
  
  output$file_message_trend_agrofims <- renderInfoBox({
    
    hot_file <- hot_path_agrofims()
    #p1 <<- hot_path_agrofims()
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
  
  
  output$downloadagrofims_trend_report <- downloadHandler(
    filename =  paste0("report", "_", as.character(Sys.time(), "%Y%m%d%H%M%S"), '.docx'),
    content = function(con) {
      
      shiny::withProgress(message = "Opening trend Report...",value= 0,{
        
        shiny::incProgress(1/5, detail = paste("Downloading Analysis..."))  
       
        #aa1 <<- hot_fb_agrofims()
        #aa1 <<- input$stat_trend_agrofims 
        #bb1 <<- hot_fbsample_agrofims()
        
        
        if(is.null(input$trait_trend_agrofims)){
          shinyalert("Oops!", "Select trait(s) to perform analysis", type = "error")
          
        } else {
    
          
          #Field book data
          fb <- hot_fbsample_agrofims()
          #bb1<<- fb
          
          fb <- as.data.frame(fb,stringsAsFactors=FALSE)
          #Factor inputs
          trait <- input$trait_trend_agrofims
          rep <- input$rep_trend_agrofims
          trt <- input$trt_trend_agrofims
          eu <- input$eu_trend_agrofims
          #stat options
          staop <- input$stat_trend_agrofims 
          average <- ifelse("mean" %in% staop, TRUE, FALSE)
          se <- ifelse("sd" %in% staop,  TRUE, FALSE)
        
          #block <- input$block_trend_agrofims
          k <- input$k_trend_agrofims #
          #factors <- input$factor_trend_agrofims # factor 1
          factors<- stringr::str_replace_all(string= input$factor_trend_agrofims, pattern =  "[[:space:]]", replacement = "_")
          #factor2_trend <- input$factor_trend_agrofims2 #factor 2
          mplot<- input$main_plot_factor
          subplot<- input$sub_plot_factor
          subsubplot<- input$subsub_plot_factor
          #type of document
          format <- "word" #paste(input$format_trend_agrofims, sep="")
          design <- input$design_trend_agrofims
          
          
          incProgress(2/5, detail = paste("Downloading Analysis..."))
          
          if(design == "Randomized Complete Block Design (RCBD)"){
            
            servName =   "trend_graph"
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
              
              try(
                
                
                try(pepa::repo.trend(response = trait, treat = trt, rep = rep, eu = eu, dap = "DATE", dfr = fb, 
                                     average = average,se = se,format = format,
                                     server =TRUE, server_dir_name = dirName, server_file_name = servName 
                ))
                # 
                # pepa::repo.rcbd(traits = trait, trt = trt, rep = rep, format = format, dfr = fb, server =TRUE, server_dir_name = dirName, 
                #                   server_file_name = servName)
                  
                  )
              
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
         
            servName =   "trend_graph"
            
            serverFileDir <-"https://research.cip.cgiar.org/gtdms/hidap/hidap_sbase_reports/files/"
            serverService <-"https://research.cip.cgiar.org/gtdms/hidap/hidap_sbase_reports/getFileUpload.php"
            
            uploadDate  <- as.character(Sys.time(), "%Y%m%d%H%M%S")
            ranStr <-  stringi::stri_rand_strings(1, 15,  '[a-zA-Z0-9]')
            servName <- paste(uploadDate, ranStr, servName , sep= "-") #nombre sin extensions!!!!
             
            dirName <- fbglobal::get_base_dir()
            path <- paste0(dirName, servName, ".docx")
            
            
            try(pepa::repo.trend(response = trait, treat = trt, rep = rep, eu = eu, dap = "DATE", dfr = fb, 
                                 average = average,se = se,format = format,
                                 server =TRUE, server_dir_name = dirName, server_file_name = servName 
                                 ))

            params <- list(
              dataRequest = "uploadFile",
              fileServerName = paste0(servName, ".docx"),
              filedata= httr::upload_file(path, "text/csv")
              
            )
            
            var <- httr::POST(serverService, body=params, timeout(1000))
            code <- httr::content(var, "text")
            
            print("paso 2")
            
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
              
              pepa::repo.f(dfr = fb, traits = trait, rep= NULL,
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
              
              pepa::repo.f(dfr = fb, traits = trait, factors= factors,
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
            
            pepa::repo.spld(dfr = fb, traits = trait, 
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
        getOption('timeout')
        options(timeout=1000000000000)
        
        download.file(paste0(serverFileDir, servName, ".docx"), con, method = "curl")
        
        incProgress(4/5, detail = paste("Formattting in ", "MS Word",sep= ""))
        incProgress(5/5, detail = paste("Downloading Analysis..."))
        
      })
      
    }
  )
  
  
}

