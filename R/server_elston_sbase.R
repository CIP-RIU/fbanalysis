#' Server Elston Analysis
#' 
#' @param input elston shiny server input 
#' @param output elston shiny server output
#' @param session elston shiny server session
#' @param values elston reactive values
#' @importFrom shiny reactive tabPanel renderUI selectInput icon h2 uiOutput radioButtons actionButton br column fluidRow 
#' @importFrom shinydashboard infoBox tabBox infoBoxOutput renderInfoBox
#' @importFrom shinyFiles parseFilePaths
#' @import pepa
#' @import st4gi
#' @export
#' 

elston_server_sbase <- function(input, output, session, values){
  

  
  values <- reactiveValues(fileInput = NULL)
  
  observe({
    
    #shiny::withProgress(message = "Connecting to SweetPotatoBase",value= 0,{
     
      #incProgress(1/5, detail = paste("Connecting to SweetPotatoBase via brapiR..."))
      
      white_list <- brapi::ba_db()
      #establish connection
     # incProgress(3/5, detail = paste("Ready for connection..."))
      sp_base_credentials <- white_list$sweetpotatobase
      trial_table <- brapi::ba_trials(con = sp_base_credentials)
      
      out <- list(sp_base_credentials  = sp_base_credentials , trial_table = trial_table)
     #  incProgress(5/5, detail = paste("Ready for connection..."))
      
      values$hot_bdata <- out
    #})
    
  })
 
  
  elston_bdata <- reactive({
    
    req(input$elston_sbase_studyName)
    
    sbase_data <- values$hot_bdata #extracting information from sbase (credentials and fieldbook)
    sbase_trial_table <- sbase_data$trial_table
    credentials <- sbase_data$sp_base_credentials
    
    #get inputs
    program <- input$elston_selProgram_sbase
    trial   <- input$elston_sbase_trialName
    study   <- input$elston_sbase_studyName
    
    #Vector with all the studies selected by users
    sel_multi_study <-  sbase_trial_table %>%  
                        filter(programName %in% program) %>% 
                        filter(trialName %in% trial) %>% 
                        filter(studyName %in% study)
    
    #id of selected studies
    id_study <- sel_multi_study$studyDbId
    
    #number of studies
    n <- length(id_study)
    #Inizialitation of empty list. It storages of all datasets selected by users 
    combine <- vector(mode="list", length = n)
    
    if(length(id_study)==0){return (NULL)}
    
    # if(length(id_study)>=1 && length(id_study)<=2 ) {
    #   flag <- FALSE
    #   shinysky::showshinyalert(session, "alert_met_sbase_done", paste("Select at least 3 studies (fieldbooks)"), styleclass = "warning")
    #   return (NULL)
    # }
    
    if(length(id_study)>=1){
      
      #Inizialitation of environment vector.
      ENVIRONMENT <- vector(mode = "character", length = n )
      
      for(i in 1:n){
        
        combine[[i]] <-  brapi::ba_studies_table(credentials , studyDbId = as.character(id_study[i])) #get fieldbook and then storage
        ENVIRONMENT <- paste("ENV", unique(combine[[i]][["locationName"]]), i, sep="_")#create a differente environment ID despite of having the same location.
        #put environment columns aside to each fieldbook.
        combine[[i]] <- cbind(ENVIRONMENT, combine[[i]])
      }
      
      #join books. The fieldbook books were previously combined.
      join_books <- data.table::rbindlist(combine,fill = TRUE)
      join_books <- as.data.frame(join_books)
      shinysky::showshinyalert(session, "alert_elston_sbase_done", paste("Great!. Perform your Elston analysis"), styleclass = "success")
      elston_bdata <- join_books
    }
  })
  
  #select program name
  output$programName_elston_sbase  <- renderUI({
    
    ##req(input$connect_met_sbase)
    
    #sbase_data <- hot_bdata()
    #sbase_data <- sbase_data$trial_table
    sbase_data <- values$hot_bdata
    sbase_data <- sbase_data$trial_table
    
    program_name <- sbase_data  %>% select(programName)
    program_name <- program_name %>% unique()
    
    selectInput('elston_selProgram_sbase', 'Select program', multiple=TRUE, c(Choose='', program_name), selectize=TRUE)
    
  })
  
  #select trial name
  output$trialName_elston_sbase  <- renderUI({
    
    #req(input$connect_met_sbase)
    req(input$elston_selProgram_sbase)
    
    sel_programName <- input$elston_selProgram_sbase
    
    # sbase_data <- hot_bdata()
    # sbase_data <- sbase_data$trial_table
    
    sbase_data <- values$hot_bdata
    sbase_data <- sbase_data$trial_table
    
    sbase_data <- sbase_data %>% filter(programName == sel_programName)
    
    trial_name <- sbase_data %>% select(trialName)
    trial_name <- trial_name[[1]] %>% unique()
    
    selectInput('elston_sbase_trialName', 'Select trial',  multiple=TRUE, c(Choose='', trial_name), selectize=TRUE)
    
  })
  
  # select study name
  output$studyName_elston_sbase  <- renderUI({
    
    # req(input$connect_single_sbase)
    req(input$elston_selProgram_sbase)
    req(input$elston_sbase_trialName)
    sel_trialName <- input$elston_sbase_trialName

    
    sbase_data <- values$hot_bdata
    sbase_data <- sbase_data$trial_table
    
    
    sbase_data <- sbase_data %>% filter(trialName == sel_trialName)
    
    study_name <- sbase_data %>% select(studyName)
    study_name <- study_name[[1]] %>% unique()
    
    selectInput('elston_sbase_studyName', 'Select study',  multiple=TRUE, c(Choose='', study_name), selectize=TRUE)
    
  })
  
  
  output$genotypes_elston_sbase  <- renderUI({
    selectInput('genotypes_sbase_elston', 'Select Genotypes', c(Choose='', select_options(elston_bdata())), 
                selectize=TRUE)
  })
  
  output$env_elston_sbase  <- renderUI({
    selectInput('env_sbase_elston', 'Select Environments', c(Choose='', select_options(elston_bdata())),
                selectize=TRUE)
  })
  
  output$rep_elston_sbase  <- renderUI({
    selectInput('rep_sbase_elston', 'Select Replications', c(Choose='', select_options(elston_bdata())),
                selectize=TRUE)
  })
  
  output$trait_posElston_sbase <- renderUI({
    selectInput('trait_pos_sbase_elston', 'Select Positive Trait(s)', c(Choose='', select_options(elston_bdata())),
                selectize=TRUE, multiple = TRUE)
  })
  
  output$trait_negElston_sbase <- renderUI({
    selectInput('trait_neg_sbase_elston', 'Select Negative Trait(s)', c(Choose='', select_options(elston_bdata())),
                selectize=TRUE, multiple = TRUE)
  })
  
  
  output$file_message_elston_sbase <- renderInfoBox({
    
    sbase_data <- values$hot_bdata
    sbase_data <- sbase_data["trial_table"]
    
    if(is.null(sbase_data)){
      infoBox(title="GREAT", subtitle=
                paste("Now conected to SweetPotatoBase"), icon = icon("upload", lib = "glyphicon"),
              color = "blue",fill = TRUE, width = NULL)
    } else {
      
      hot_file <- sbase_data
      hot_file <- paste("hot_file", collapse = ", ")
      infoBox(title="GREAT!", subtitle =
                paste("Now conected to SweetPotatoBase"),  icon = icon("ok", lib = "glyphicon"),
              color = "green",fill = TRUE, width = NULL)
    }
  })
  
  
  shiny::observeEvent(input$elston_sbase_button, {
    shiny::withProgress(message = "Opening Elston  Index Report...",value= 0,{
      
      fieldbook <- as.data.frame(elston_bdata())
      print(fieldbook)
      trait_pos <- input$trait_pos_sbase_elston
      print(trait_pos)
      trait_neg <- input$trait_neg_sbase_elston
      print(trait_neg)
      trait <- c(trait_pos,trait_neg)
      trait <- trait[trait!=""]
      #trait <- input$trait_elston
      env <- input$env_sbase_elston
      rep <- input$rep_sbase_elston
      
      genotypes <- input$genotypes_sbase_elston
      #model <- gsub(pattern = "[[:space:]]\\(.*", replacement = "", input$model_elston)
      
      means <- input$means_sbase_elston

      #       
      if(length(trait_neg)>0){
        fieldbook[,trait_neg] <- -fieldbook[,trait_neg]
      }
      
      
      #format <- paste(input$format_elston,"_document",sep="")
      format <- paste(input$format_sbase_elston, sep="")
      
      print(format)
      
      try(pepa::pty.elston(traits = trait, geno = genotypes, env= env, rep = rep, means = means,
                           data = fieldbook, format = format))
      
      if(env!="" && means=="fitted"){
        #env <- NULL
        print(env)
        print(1)
        try(pepa::pty.elston(traits = trait, geno = genotypes, env = env,  means = means, data = fieldbook, format = format))
      }
      
      if(env=="" && means=="single"){
        # If means = "single" and env is not specified, then single arithmetic means are computed over all 
        # the observations for each genotype.
        print(env)
        print(2)
        #env <- NULL
        try(pepa::pty.elston(traits = trait, geno = genotypes, env = NULL,  means = means, data = fieldbook, format = format))
      }
      
      if(env!="" && means=="single"){
        
        # If means = "single" and env is specified, then single arithmetic means are computed 
        # over the replications for each genotype at each environment and then for each genotype over environments.
        print(env)
        print(3)
        try(pepa::pty.elston(traits = trait, geno = genotypes, env = env, means = means, data = fieldbook, format = format))
      } 
      
      #       if(env=="" && model=="g+e" && means=="single"){
      #         try(pepa::pty.elston(traits = trait, geno = genotypes, model = model, means = means, data = fieldbook))
      #       } 
      
      
      
      
    })
  })
  
} 


