#' Server for Single Environment analysis
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

single_server_base <- function(input, output, session, values){


  
  values <- reactiveValues(fileInput = NULL)
  
  # Limpia marcadores del mapa "1a" y asigna NULL a ciertos parametros
  observe({
    #input$clearMap1a
    shiny::withProgress(message = "Connecting to SweetPotatoBase",value= 0,{
      
      #NOTE: To use pepa report package we need R 3.3.0 or more.
      #NOTE Finally, we always need pandoc installer.
      #ToDo: In case of poor conection print a message and do not show anything
      
      incProgress(1/5, detail = paste("Connecting to SweetPotatoBase via brapiR..."))
      white_list <- brapi::ba_db()
      #establish connection
      incProgress(3/5, detail = paste("Ready for connection..."))
      sp_base_credentials <- white_list$sweetpotatobase
      trial_table <- brapi::ba_trials(con = sp_base_credentials)

      out <- list(sp_base_credentials  = sp_base_credentials , trial_table = trial_table)
      incProgress(5/5, detail = paste("Ready for connection..."))
   
    
    values$hot_bdata <- out
    })
    
  })
  
  
  #hot_bdata <- reactive ({

  # hot_bdata <-  eventReactive(input$connect_single_sbase,{
  # 
  #   validate(
  #    need(input$connect_single_sbase != "", label = "Please connect to SweetPotato Base")
  #   )
  # 
  #   #if(is.null(sel_fb_temp) || sel_fb_temp == ""){  return()  }
  #   #if(length(input$connect_single_sbase)>0){
  # 
  #    #fb_temp <- readRDS(sel_fb_temp)
  #     white_list <- brapi::ba_db()
  #     #establish connection
  #     sp_base_credentials <- white_list$sweetpotatobase
  #     trial_table <- brapi::ba_trials(con = sp_base_credentials)
  # 
  #     out <- list(sp_base_credentials  = sp_base_credentials , trial_table = trial_table)
  # 
  #  #trial_table
  # 
  # })



  # program name for single trial in sbase
  output$programName_single_sbase  <- renderUI({

    #req(input$connect_single_sbase)

    sbase_data <- values$hot_bdata
    sbase_data <- sbase_data$trial_table
    program_name <- sbase_data  %>% select(programName)
    program_name <- program_name %>% unique()

    selectInput('single_selProgram_sbase', 'Select program', c(Choose='', program_name), selectize=TRUE)

  })


  output$trialName_single_sbase  <- renderUI({

    #req(input$connect_single_sbase)
    req(input$single_selProgram_sbase)

    sel_programName <- input$single_selProgram_sbase

    #sbase_data <- hot_bdata() #using button for connecting to SBASE
    sbase_data <- values$hot_bdata
    sbase_data <- sbase_data$trial_table

    sbase_data <- sbase_data %>% filter(programName == sel_programName)

    trial_name <- sbase_data %>% select(trialName)
    trial_name <- trial_name[[1]] %>% unique()

    selectInput('single_sbase_trialName', 'Select trial', c(Choose='', trial_name), selectize=TRUE)

  })



  output$studyName_single_sbase  <- renderUI({

    # req(input$connect_single_sbase)
    # req(input$single_selProgram_sbase)
    req(input$single_sbase_trialName)
    sel_trialName <- input$single_sbase_trialName

    #sbase_data <- hot_bdata() #using button for connecting to SBASE
    sbase_data <- values$hot_bdata #reactive data
    sbase_data <- sbase_data$trial_table

    sbase_data <- sbase_data %>% filter(trialName == sel_trialName)

    study_name <- sbase_data %>% select(studyName)
    study_name <- study_name[[1]] %>% unique()

    selectInput('single_sbase_studyName', 'Select study', c(Choose='', study_name), selectize=TRUE)

  })


  output$show_single_sbase_params <- reactive({
    #return(!is.null(gmtl_data()))
    p <- input$single_sbase_studyName
    #print(p)
    # p <- p ==""
    if(is.null(p)){ 
      val_logic <- FALSE 
    } else if(p==""){
      val_logic <- FALSE
    } else{
      val_logic <- TRUE
    }

    return(val_logic)
    
  })
  
  #set options for show_mtable
  outputOptions(output, 'show_single_sbase_params', suspendWhenHidden=FALSE)
  
  hot_fb_sbase <- reactive({

    req(input$single_sbase_studyName)

    #sbase_data <- hot_bdata() #extracting informatin from sbase (credentials and fieldbook) #using button for connecting to SBASE
    sbase_data <- values$hot_bdata
    
    sbase_fb <- sbase_data$trial_table
    credentials <- sbase_data$sp_base_credentials

    col_fb_sbase <- sbase_fb %>% dplyr::filter(programName== input$single_selProgram_sbase, trialName == input$single_sbase_trialName, studyName == input$single_sbase_studyName)
    dt <-  ba_studies_table(credentials , studyDbId = as.character(col_fb_sbase$studyDbId))

  })

  
  output$genotypes_single_sbase  <- renderUI({

    #req(input$connect_single_sbase)
    req(input$single_selProgram_sbase)
    req(input$single_sbase_trialName)

    selectInput('genotypes_single_sbase', 'Select Genotypes', c(Choose='', names(hot_fb_sbase())),
                selectize=TRUE)
  })


  output$rep_single_sbase  <- renderUI({
    selectInput('rep_single_sbase', 'Select Replications', c(Choose='', names(hot_fb_sbase())),
                selectize=TRUE)
  })


  output$trait_single_sbase <- renderUI({
    selectInput('trait_single_sbase', 'Select Trait(s)', c(Choose='', names(hot_fb_sbase())),
                selectize=TRUE, multiple = TRUE)
  })


  output$factor_single_sbase  <- renderUI({
    selectInput('factor_single_sbase', 'Select Factor', c(Choose='', names(hot_fb_sbase())),
                selectize=TRUE)
  })


  output$block_single  <- renderUI({
    selectInput('block_single_sbase', 'Select Block', c(Choose='', names(hot_fb_sbase())),
                selectize=TRUE)
  })


  output$k_single_sbase  <- renderUI({
    shiny::numericInput('k_single_sbase', 'Select Block Size',   value =2, min=2, max = 100)
  })


  output$file_message_single_sbase <- renderInfoBox({

    #sbase_data <-  hot_bdata()["trial_table"] #using button for connecting to SBASE

    sbase_data <- values$hot_bdata
    sbase_data <- sbase_data["trial_table"]
    
    if(is.null(sbase_data)){
      infoBox(title="Select fieldbook file", subtitle=
                paste("Connect to SweetPotato Base"), icon = icon("plug"),
              color = "blue",fill = TRUE, width = NULL)
      #      }
      #       else if(all(is.na(germoplasm))) {
      #         infoBox(title="ERROR", subtitle=
      #                   paste("Your material list", "is empty. Please check it"), icon = icon("warning-sign", lib = "glyphicon"),
      #                 color = "red",fill = TRUE, width = NULL)
      #         #shell.exec(hot_path())
      #
    } else {
      #       material <- paste(germoplasm, collapse = ",")
      #       message <-  paste("Material list imported: ", material)
      hot_file <- sbase_data
      hot_file <- paste(hot_file, collapse = ", ")
      infoBox(title="GREAT!", subtitle =
                paste("Now you are connected"),  icon = icon("ok", lib = "glyphicon"),
              color = "green",fill = TRUE, width = NULL)
    }
  })

  shiny::observeEvent(input$single_button_sbase, {
    shiny::withProgress(message = "Opening single Report...",value= 0,{

      #NOTE: To use pepa report package we need R 3.3.0 or more.
      #NOTE Finally, we always need pandoc installer.

      incProgress(3/5, detail = paste("Analyzing..."))
      design <- input$design_single_sbase

      #fieldbook <- as.data.frame(hot_bdata()$trial_table)
      fieldbook <-  as.data.frame(hot_fb_sbase())
      #saveRDS(fieldbook,"res.rds")
      trait <- input$trait_single_sbase
      rep <- input$rep_single_sbase
      genotypes <- input$genotypes_single_sbase
      block <- input$block_single_sbase
      k <- input$k_single_sbase
      factor_single <- input$factor_single_sbase

      #format <- paste(input$format_single,"_document",sep="")
      format <- paste(input$format_single_sbase)

      if(design == "Randomized Complete Block Design (RCBD)"){
        try(pepa::repo.rcbd(traits = trait, geno = genotypes, rep = rep, format = format, data = fieldbook))
      }

      if(design == "Completely Randomized Design (CRD)"){
        try(pepa::repo.crd(traits = trait, geno = genotypes, format = format, data = fieldbook))
        #try(pepa::repo.crd(traits = trait, geno = genotypes, rep = rep, format = format, data = fieldbook))
      }

      if(design == "Augmented Block Design (ABD)"){
        #try(pepa::repo.abd(traits = trait, geno = genotypes, format = format, data = fieldbook))
        try(pepa::repo.abd(traits = trait, geno = genotypes, rep = rep, format = format, data = fieldbook))
      }

      if(design == "Alpha Design(0,1) (AD)"){
        #try(pepa::repo.abd(traits = trait, geno = genotypes, format = format, data = fieldbook))
        try(pepa::repo.a01d(traits = trait, geno = genotypes, rep = rep, block = block, k = k, data = fieldbook, format = format))
      }

      if(design == "Split Plot with Plots in CRD (SPCRD)"){

        title <- paste("Automatic report for ", design, sep= "")

        try(pepa::repo.2f(traits = trait, A = genotypes, B = factor_single, rep = rep, design = "crd",  title= title, data = fieldbook, format = format))
      }

      if(design == "Factorial Two-Way Design in CRD (F2CRD)"){

        title <- paste("Automatic report for ", design, sep= "")
        try(pepa::repo.2f(traits = trait, A = genotypes, B = factor_single, rep = rep, design = "crd",  title= title, data = fieldbook, format = format))
      }

      if(design == "Split Plot with Plots in RCBD (SPRCBD)"){

        title <- paste("Automatic report for ", design, sep= "")
        try(pepa::repo.2f(traits = trait, A = genotypes, B = factor_single, rep = rep, design = "rcbd", title= title, data = fieldbook, format = format))
      }

      if(design == "Factorial Two-Way Design in RCBD (F2RCBD)"){

        title <- paste("Automatic report for ", design, sep= "")
        try(pepa::repo.2f(traits = trait, A = genotypes, B = factor_single, rep = rep, design = "rcbd", title= title, data = fieldbook, format = format))
      }

      incProgress(5/5, detail = paste("Analyzing..."))
      
      
    })
  })




}

