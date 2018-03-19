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
  
  observe({
    
    shiny::withProgress(message = "Connecting to SweetPotatoBase",value= 0,{
      
      #NOTE: To use pepa report package we need R 3.3.0 or more.
      #NOTE Finally, we always need pandoc installer.
      #ToDo: In case of poor conection print a message and do not show anything
      
      incProgress(1/5, detail = paste("Connecting to SweetPotatoBase via brapiR..."))
      
      # validate(
      #  need(input$connect_single_sbase != "", label = "Please connect to SweetPotato Base")
      # )
      
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
  
  output$show_single_sbase <- reactive({
    return(!is.null(values$hot_bdata))
  })
  
  output$show_single_sbase_len <- reactive({
    fb <- hot_fb_sbase()$fb
    design<-  hot_fb_sbase()$stat_design
    #fb <- fb$fb 
    
    
    return(!is.null(hot_fb_sbase()))
    #return(!is.null(fb) & length(design)>0)
  })
  
  
  outputOptions(output, 'show_single_sbase', suspendWhenHidden=FALSE)
  
  outputOptions(output, 'show_single_sbase_len', suspendWhenHidden=FALSE)
  
  # program name for single trial in sbase
  output$programName_single_sbase  <- renderUI({
    
    #req(input$connect_single_sbase)
    
    sbase_data <- values$hot_bdata
    sbase_data <- sbase_data$trial_table
    program_name <- sbase_data  %>% select(programName)
    program_name <- program_name %>% unique()
    
    selectInput('single_selProgram_sbase', 'Select program', c(Choose='', program_name), selectize=TRUE)
    
  })
  
  #select trial name
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
  
  
  # select study name
  output$studyName_single_sbase  <- renderUI({
    
    # req(input$connect_single_sbase)
    req(input$single_selProgram_sbase)
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
  
  
  output$show_studyName_status <- reactive({
    return(length(input$single_sbase_studyName)>0)
  })
  
  
  #get information from sbase 
  hot_fb_sbase <- reactive({
    
    req(input$single_sbase_trialName)
    req(input$single_sbase_studyName)
    
    
    #sbase_data <- hot_bdata() #extracting informatin from sbase (credentials and fieldbook) #using button for connecting to SBASE
    sbase_data <- values$hot_bdata
    
    sbase_fb <- sbase_data$trial_table
    credentials <- sbase_data$sp_base_credentials
    
    col_fb_sbase <- sbase_fb %>% dplyr::filter(programName== input$single_selProgram_sbase, trialName == input$single_sbase_trialName, studyName == input$single_sbase_studyName)
    fb <<-  try(ba_studies_table(credentials , studyDbId = as.character(col_fb_sbase$studyDbId)))
    #print(fb)
    design <- try(unique(fb$studyDesign))
    
    if(str_detect(design,"error")){
      design <- NULL
    }
    
    out <- list(fb = fb , stat_design = design)
    
  })
  
  
  observe({
    
    # <- hot_fb_sbase()
    design <- try(hot_fb_sbase()$stat_design) #the statistical design from the SweetPotatoBase
    nrow_fb <- try(nrow(hot_fb_sbase()$fb))
    
    if(str_detect(design,"error")){
      design <- NULL
    }
    x <- input$design_single_sbase #the statistical design from the single env UI
    
    # Can use character(0) to remove all choices
    #if (is.null(x)|| length(design)==0){
    if (is.null(x) || length(design)==0 ){
      
      x <- "Completely Randomized Design (CRD)"
      shinysky::showshinyalert(session, "alert_single_sbase_done", paste("This study does not have data."), styleclass = "danger")
      
    } else {
      
      if(design == "CRD")  {x <- "Completely Randomized Design (CRD)"}
      if(design == "RCBD") {x <- "Randomized Complete Block Design (RCBD)"}
      if(design == "ABD")  {x <-"Augmented Block Design (ABD)"}
      if(design == "Alpha")   {x <-"Alpha design"}
      #if(design == "CRD")  {choice_design<-"Factorial Two-Way Design in CRD (F2CRD)"}
      #if(design == "CRD")  {choice_design<-"Factorial Two-Way Design in RCBD (F2RCBD)"}
      
    }
    x <- x
    
    # Can also set the label and select items
    updateSelectInput(session, "design_single_sbase",
                      label = 'Select statistical design of your experiment',
                      choices = x,
                      #"Split Plot with Plots in CRD (SPCRD)",
                      #"Split Plot with Plots in RCBD (SPRCBD)"),
                      selected = x
    )
  })
  
  
  #select genotypes
  
  output$genotypes_single_sbase  <- renderUI({
    
    #req(input$connect_single_sbase)
    req(input$single_selProgram_sbase)
    req(input$single_sbase_trialName)
    
    selectInput('genotypes_single_sbase', 'Select Genotypes', c(Choose='', names(hot_fb_sbase()$fb)),
                selectize=TRUE)
    
  })
  
  #select repetition or blocks
  output$rep_single_sbase  <- renderUI({
    
    fb <- hot_fb_sbase()
    fb <- fb$fb 
    # selectInput('rep_single_sbase', 'Select Replications', c(Choose='', names(hot_fb_sbase()$fb)),
    #             selectize=TRUE)
    selectInput('rep_single_sbase', 'Select Replications or Blocks', c(Choose='', names(fb)),
                selectize=TRUE)
  })
  
  #select triats
  output$trait_single_sbase <- renderUI({
    # selectInput('trait_single_sbase', 'Select Trait(s)', c(Choose='', names(hot_fb_sbase()$fb)),
    #             selectize=TRUE, multiple = TRUE)
    fb <- hot_fb_sbase()
    fb <- fb$fb 
    selectInput('trait_single_sbase', 'Select Trait(s)', c(Choose='', names(fb)),
                selectize=TRUE, multiple = TRUE)
    
  })
  
  #select factors
  output$factor_single_sbase  <- renderUI({
    selectInput('factor_single_sbase', 'Select Factor', c(Choose='', names(hot_fb_sbase()$fb)),
                selectize=TRUE)
  })
  
  #select block
  output$block_single  <- renderUI({
    selectInput('block_single_sbase', 'Select Block', c(Choose='', names(hot_fb_sbase()$fb)),
                selectize=TRUE)
  })
  
  #block size
  output$k_single_sbase  <- renderUI({
    shiny::numericInput('k_single_sbase', 'Select Block Size',   value =2, min=2, max = 100)
  })
  
  
  output$file_message_single_sbase <- renderInfoBox({
    
    sbase_data <- values$hot_bdata
    sbase_data <- sbase_data$fb
    
    sbase_data <- sbase_data["trial_table"]
    print(sbase_data)
    #shiny::withProgress(message = "Checking connection to SweetPotatoBase",value= 0,{

    #  incProgress(2/5, detail = paste("Validating..."))
    
    if(!is.null(sbase_data)){
      infoBox(title="Select fieldbook file", subtitle=
                paste("Now you are NOT connected to SweetPotatoBase. Check whether the database is under maintenance"), icon = icon("plug"),
              color = "yellow", fill = TRUE, width = NULL)
      #
    } else {
      hot_file <- sbase_data
      #hot_file <- paste(hot_file, collapse = ", ")
      infoBox(title="GREAT!", subtitle =
                paste("Now you are connected to SweetPotatoBase using BrAPI"),  icon = icon("ok", lib = "glyphicon"),
              color = "green",fill = TRUE, width = NULL)
    }
      #incProgress(5/5, detail = paste("Validating"))
    
    #})
    
  })
  
  
  output$downloadSbase_single_report <- downloadHandler(
    filename = function() {
      paste("report", 'docx', sep='.')
    },
    content = function(con) {
      
      shiny::withProgress(message = "Opening single Report...",value= 0,{
        
        incProgress(1/5, detail = paste("Downloading Analysis..."))  
        
        #hot_fb_sbase <- hot_fb_sbase()$fb
        
        #fieldbook <- as.data.frame(hot_fb_sbase())
        fieldbook <- hot_fb_sbase()
        fieldbook <- as.data.frame(fieldbook$fb)
        
        
        trait <- input$trait_single_sbase
        rep <- input$rep_single_sbase
        genotypes <- input$genotypes_single_sbase
        block <- input$block_single_sbase
        k <- input$k_single_sbase
        factor_single <- input$factor_single_sbase
        
        
        format <- paste(input$format_single_sbase, sep="")
        design <- input$design_single_sbase
        
        
        incProgress(2/5, detail = paste("Downloading Analysis..."))
        
        if(design == "Randomized Complete Block Design (RCBD)"){
          try(pepa::repo.rcbd(traits = trait, geno = genotypes, rep = rep, format = format, data = fieldbook))
          path <- "/usr/local/lib/R/site-library/pepa/rmd/rcbd.docx" #shiny server CIP-RIU
          #path <- "/home/hidap/R/x86_64-pc-linux-gnu-library/3.4/pepa/rmd/rcbd.docx" #shiny server BTI-SweetPotatoBase
          
          #file.copy("/usr/local/lib/R/site-library/pepa/rmd/crd.docx", con)
        }
        
        if(design == "Completely Randomized Design (CRD)"){
          try(pepa::repo.crd(traits = trait, geno = genotypes, format = format, data = fieldbook))
          path <- "/usr/local/lib/R/site-library/pepa/rmd/crd.docx" #shiny server CIP-RIU
          #path <- "/home/hidap/R/x86_64-pc-linux-gnu-library/3.4/pepa/rmd/crd.docx" #shiny server BTI-SweetPotatoBase
          #file.copy("/usr/local/lib/R/site-library/pepa/rmd/rcbd.docx", con)
        }
        
        
        if(design == "Augmented Block Design (ABD)"){
          #try(pepa::repo.abd(traits = trait, geno = genotypes, format = format, data = fieldbook))
          try(pepa::repo.abd(traits = trait, geno = genotypes, rep = rep, format = format, data = fieldbook))
          path <- "/usr/local/lib/R/site-library/pepa/rmd/abd.docx"
          #file.copy("/usr/local/lib/R/site-library/pepa/rmd/abd.docx", con)
        }
        # 
        # 
        if(design == "Alpha design"){
          #try(pepa::repo.abd(traits = trait, geno = genotypes, format = format, data = fieldbook))
          try(pepa::repo.a01d(traits = trait, geno = genotypes, rep = rep, block = block, k = k, data = fieldbook, format = format))
          path <- "/usr/local/lib/R/site-library/pepa/rmd/a01d.docx"
          #file.copy("/usr/local/lib/R/site-library/pepa/rmd/a01d.docx", con)
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
        
        print(trait)
        print(design)
        
        file.copy(path , con, overwrite = TRUE)
        
        incProgress(4/5, detail = paste("Formattting in ", "MS Word",sep= ""))
        incProgress(5/5, detail = paste("Downloading Analysis..."))
        
      })
      
    }
  )
  
  
  shiny::observeEvent(input$single_button_sbase, {
    shiny::withProgress(message = "Opening single Report...",value= 0,{ #begin progress bar
      
      #NOTE: To use pepa report package we need R 3.3.0 or more.
      #NOTE Finally, we always need pandoc installer.
      incProgress(3/5, detail = paste("Analyzing..."))
      
      design <- input$design_single_sbase
      
      incProgress(2/5, detail = paste("Processing..."))
      #fieldbook <- as.data.frame(hot_bdata()$trial_table)
      
      #fieldbook <-  as.data.frame(hot_fb_sbase())
      
      fieldbook <- hot_fb_sbase()
      fieldbook <- as.data.frame(fieldbook$fb)
      
      #saveRDS(fieldbook,"res.rds")
      trait <- input$trait_single_sbase
      rep <- input$rep_single_sbase
      genotypes <- input$genotypes_single_sbase
      block <- input$block_single_sbase
      k <- input$k_single_sbase
      factor_single <- input$factor_single_sbase
      
      incProgress(3/5, detail = paste("Passing parameters..."))
      
      #format <- paste(input$format_single,"_document",sep="")
      format <- paste(input$format_single_sbase)
      
      print(trait)
      print(design)
      
      
      
      incProgress(4/5, detail = paste("Formatting in ", format, sep=""))
      
      if(design == "Randomized Complete Block Design (RCBD)"){
        try(pepa::repo.rcbd(traits = trait, geno = genotypes, rep = rep, format = format, data = fieldbook))
      }
      
      if(design == "Completely Randomized Design (CRD)"){
        try(pepa::repo.crd(traits = trait, geno = genotypes, format = format, data = fieldbook))
        #try(pepa::repo.crd(traits = trait, geno = genotypes, rep = rep, format = format, data = fieldbook))
      }
      
      # if(design == "Augmented Block Design (ABD)"){
      #   #try(pepa::repo.abd(traits = trait, geno = genotypes, format = format, data = fieldbook))
      #   try(pepa::repo.abd(traits = trait, geno = genotypes, rep = rep, format = format, data = fieldbook))
      # }
      # 
      # if(design == "Alpha Design(0,1) (AD)"){
      #   #try(pepa::repo.abd(traits = trait, geno = genotypes, format = format, data = fieldbook))
      #   try(pepa::repo.a01d(traits = trait, geno = genotypes, rep = rep, block = block, k = k, data = fieldbook, format = format))
      # }
      # 
      # if(design == "Split Plot with Plots in CRD (SPCRD)"){
      #   
      #   title <- paste("Automatic report for ", design, sep= "")
      #   try(pepa::repo.2f(traits = trait, A = genotypes, B = factor_single, rep = rep, design = "crd",  title= title, data = fieldbook, format = format))
      # }
      # 
      # if(design == "Factorial Two-Way Design in CRD (F2CRD)"){
      #   
      #   title <- paste("Automatic report for ", design, sep= "")
      #   try(pepa::repo.2f(traits = trait, A = genotypes, B = factor_single, rep = rep, design = "crd",  title= title, data = fieldbook, format = format))
      # }
      # 
      # if(design == "Split Plot with Plots in RCBD (SPRCBD)"){
      #   
      #   title <- paste("Automatic report for ", design, sep= "")
      #   try(pepa::repo.2f(traits = trait, A = genotypes, B = factor_single, rep = rep, design = "rcbd", title= title, data = fieldbook, format = format))
      # }
      # 
      # if(design == "Factorial Two-Way Design in RCBD (F2RCBD)"){
      #   title <- paste("Automatic report for ", design, sep= "")
      #   try(pepa::repo.2f(traits = trait, A = genotypes, B = factor_single, rep = rep, design = "rcbd", title= title, data = fieldbook, format = format))
      # }
      
      incProgress(5/5, detail = paste("Formatting in ", format, sep="")) #end progress bar
      
      
    })
  })
  
  
  
  
}

