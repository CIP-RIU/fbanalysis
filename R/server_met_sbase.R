#' Server Multi-Environment Analysis (MET)
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

met_server_sbase <- function(input, output, session, values){
  
  
  # hot_bdata <-  reactive({
  #  
  #    validate(
  #      need(input$connect_met_sbase != "", label = "Please connect to SweetPotato Base")
  #    )
  #    
  #    #if(is.null(sel_fb_temp) || sel_fb_temp == ""){  return()  }
  #    #if(length(input$connect_single_sbase)>0){
  #    
  #    #fb_temp <- readRDS(sel_fb_temp)
  #    white_list <- brapi::ba_db()
  #    #establish connection
  #    sp_base_credentials <- white_list$sweetpotatobase
  #    trial_table <- brapi::ba_trials(con = sp_base_credentials)
  #    
  #    out <- list(sp_base_credentials  = sp_base_credentials , trial_table = trial_table)
  #    
  #    hot_bdata <- out
  #    
  #    
  #    
  #  })
  
  values <- reactiveValues(fileInput = NULL)
  
  observe({
    
    #shiny::withProgress(message = "Connecting to SweetPotatoBase",value= 0,{
      
      #NOTE: To use pepa report package we need R 3.3.0 or more.
      #NOTE Finally, we always need pandoc installer.
      #ToDo: In case of poor conection print a message and do not show anything
      
      #incProgress( 1/5, detail = paste("Connecting to SweetPotatoBase via brapiR..."))
      
      # validate(
      #  need(input$connect_single_sbase != "", label = "Please connect to SweetPotato Base")
      # )
      
      white_list <- brapi::ba_db()
      #establish connection
     # incProgress(3/5, detail = paste("Ready for connection..."))
      sp_base_credentials <- white_list$sweetpotatobase
      trial_table <- brapi::ba_trials(con = sp_base_credentials)
      
      out <- list(sp_base_credentials  = sp_base_credentials , trial_table = trial_table)
      #incProgress(5/5, detail = paste("Ready for connection..."))
      
      
      values$hot_bdata <- out
   # })
    
  })
  
  #select program name
  output$programName_met_sbase  <- renderUI({
    
    ##req(input$connect_met_sbase)
    
    #sbase_data <- hot_bdata()
    #sbase_data <- sbase_data$trial_table
    sbase_data <- values$hot_bdata
    sbase_data <- sbase_data$trial_table
    
    program_name <- sbase_data  %>% select(programName)
    program_name <- program_name %>% unique()
    
    selectInput('met_selProgram_sbase', 'Select program', multiple=TRUE, c(Choose='', program_name), selectize=TRUE)
    
  })
  
  #select trial name
  output$trialName_met_sbase  <- renderUI({
    
    #req(input$connect_met_sbase)
    req(input$met_selProgram_sbase)
    
    sel_programName <- input$met_selProgram_sbase
    
    # sbase_data <- hot_bdata()
    # sbase_data <- sbase_data$trial_table
    
    sbase_data <- values$hot_bdata
    sbase_data <- sbase_data$trial_table
    
    sbase_data <- sbase_data %>% filter(programName == sel_programName)
    
    trial_name <- sbase_data %>% select(trialName)
    trial_name <- trial_name[[1]] %>% unique()
    
    selectInput('met_sbase_trialName', 'Select trial',  multiple=TRUE, c(Choose='', trial_name), selectize=TRUE)
    
  })
  
  # select study name
  output$studyName_met_sbase  <- renderUI({
    
    # req(input$connect_single_sbase)
    req(input$met_selProgram_sbase)
    req(input$met_sbase_trialName)
    sel_trialName <- input$met_sbase_trialName
    
    #sbase_data <- hot_bdata()
    #sbase_data <- sbase_data$trial_table
    
    sbase_data <- values$hot_bdata
    sbase_data <- sbase_data$trial_table
    
    
    sbase_data <- sbase_data %>% filter(trialName == sel_trialName)
    
    study_name <- sbase_data %>% select(studyName)
    study_name <- study_name[[1]] %>% unique()
    
    selectInput('met_sbase_studyName', 'Select study',  multiple=TRUE, c(Choose='', study_name), selectize=TRUE)
    
  })
  
  
  #conditional value for diplaying MET inputs
  output$show_met_sbase_params <- reactive({
    return(!is.null(values$hot_bdata))
  })
  
  output$show_met_sbase_params <- reactive({
    #return(!is.null(gmtl_data()))
    p <- input$met_sbase_studyName
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
  
  output$show_met_sbase_len <- reactive({
    return(!is.null(hot_fb_sbase()))
  })
  
  outputOptions(output, 'show_met_sbase_params', suspendWhenHidden=FALSE)
  
  outputOptions(output, 'show_met_sbase_len', suspendWhenHidden=FALSE)
  
  
  # Statistical design Inputs -----------------------------------------------
  
  #met combined data
  hot_fb_sbase <- reactive({
    
    req(input$met_sbase_studyName)
    
    sbase_data <- values$hot_bdata #extracting information from sbase (credentials and fieldbook)
    sbase_trial_table <- sbase_data$trial_table
    credentials <- sbase_data$sp_base_credentials
    
    #get inputs
    program <- input$met_selProgram_sbase
    trial <- input$met_sbase_trialName
    study <- input$met_sbase_studyName
    
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
    
    if(length(id_study)>=1 && length(id_study)<=2 ) {
      flag <- FALSE
      shinysky::showshinyalert(session, "alert_met_sbase_done", paste("Select at least 3 studies (fieldbooks)"), styleclass = "warning")
      return (NULL)
    }
    
    if(length(id_study)>2){
      
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
      #write.csv(join_books,"join_books.csv")
      shinysky::showshinyalert(session, "alert_met_sbase_done", paste("Great!. Perform your MET analysis"), styleclass = "success")
      #met_bdata <- readxl::read_excel(path=hot_file , sheet = "Fieldbook")
      #write.csv(join_books,"metdata.csv")
      met_bdata <- join_books
    }
  })
  
  #### Inputs for met analysis #####
  
  #select genotype column
  output$genotypes_met_sbase  <- renderUI({
    
  
    req(input$met_selProgram_sbase)
    req(input$met_sbase_trialName)
    
    selectInput('genotypes_met_sbase', 'Select genotypes', c(Choose='', select_options(hot_fb_sbase())),
                selectize=TRUE)
  })
  
  #select genotype column
  output$env_met_sbase  <- renderUI({
    
    selectInput('env_met_sbase', 'Select environments', c(Choose='', select_options(hot_fb_sbase())),
                selectize=TRUE)
  })
  
  #select repetition column
  output$rep_met_sbase  <- renderUI({
    
    selectInput('rep_met_sbase', 'Select replications', c(Choose='', select_options(hot_fb_sbase())),
                selectize=TRUE)
  })
  
  #select traits column
  output$trait_met_sbase <- renderUI({
    
    selectInput('trait_met_sbase', 'Select trait(s)', c(Choose='', select_options(hot_fb_sbase())),
                selectize=TRUE, multiple = TRUE)
    
  })
  
  ####  
  #message of connection
  #ToDo: It should be doing by default
  output$file_message_met_sbase <- renderInfoBox({
    
    sbase_data <- values$hot_bdata
    sbase_data <- sbase_data["trial_table"]
    
    if(is.null(sbase_data)){
      infoBox(title="Select Fieldbook File", subtitle=
                paste("Choose at least 3 fieldbook files for MET"), icon = icon("upload", lib = "glyphicon"),
              color = "blue",fill = TRUE, width = NULL)
    } else {
      
      hot_file <- sbase_data
      hot_file <- paste("hot_file", collapse = ", ")
      infoBox(title="GREAT!", subtitle =
                paste("Fieldbooks selected: ", hot_file),  icon = icon("ok", lib = "glyphicon"),
              color = "green",fill = TRUE, width = NULL)
    }
  })
  
  
  output$downloadSbase_met_report <- downloadHandler(
    filename = function() {
      paste("report", '.docx', sep='.')
    },
    content = function(con) {
      
      shiny::withProgress(message = "Opening MET Report...",value= 0,{ #begin progress bar
        incProgress(1/5, detail = paste("Downloading met report..."))
        
        #getting parameters and fieldbook  
        #print(hot_fb_sbase())
        fieldbook <- as.data.frame(hot_fb_sbase())
        trait <- input$trait_met_sbase
        env <- input$env_met_sbase
        #print(trait)
        rep <- input$rep_met_sbase
        genotypes <- input$genotypes_met_sbase
        
        incProgress(2/5, detail = paste("Passing parameters..."))
        
        #Format of the file
        format <- paste(input$format_met_sbase,sep="")
        incProgress(3/5, detail = paste("Formatting on word..."))
        
        #Formatting on word
        try({pepa::repo.met(traits = trait, geno = genotypes, env = env, rep = rep, data = fieldbook, format=format)})
        file.copy("/usr/local/lib/R/site-library/pepa/rmd/met.docx", con, overwrite =  TRUE)
        incProgress(5/5, detail = paste("Formatting on word..."))
        
      }) #end progress bar
      
    }
  )
  
  
  #run analysis
  shiny::observeEvent(input$met_sbase_button, {
    shiny::withProgress(message = "Opening MET Enviroment Report...",value= 0,{
      
      #NOTE: To use pepa report package we need R 3.3.0 or more.
      #NOTE Finally, we always need pandoc installer.
      
        incProgress(1/5, detail = paste("Downloading met report..."))
        
        req(input$met_sbase_trialName)
        
        incProgress(2/5, detail = paste("Downloading met report..."))
        fieldbook <- as.data.frame(hot_fb_sbase())
        print(hot_fb_sbase())
        genotypes <- input$genotypes_met_sbase
        trait <- input$trait_met_sbase
        env <- input$env_met_sbase
        rep <- input$rep_met_sbase
        
        format <- paste(input$format_met_sbase, sep="")
        
        incProgress(4/5, detail = paste("Downloading met report..."))
        try({
          
          pepa::repo.met(traits = trait, geno = genotypes, env = env, rep = rep, data = fieldbook, format=format)
        })
        
        incProgress(5/5, detail = paste("Downloading met report..."))
        
      }) #end progress bar
      
      
    })
  
  
} 


