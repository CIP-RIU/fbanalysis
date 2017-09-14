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
  
  
  volumes <- shinyFiles::getVolumes()
  shinyFiles::shinyFileChoose(input, 'file_met_sbase', roots=volumes, session=session,
                              restrictions=system.file(package='base'),filetypes=c('xlsx'))
  
  validate(
    need(input$file != "", label = "Please select a file from sweetpotato base")
  )
  
  # hot_path <- reactive ({
  #   
  #   #validate(
  #   #  need(input$file != "", label = "Please enter an XLSX file. XLS files are forbidden")
  #   #)
  #   
  #   if(length(input$file_met_sbase)==0){return (NULL)}
  #   if(length(input$file_met_sbase)>0){
  #     hot_file <- as.character(parseFilePaths(volumes, input$file_met_sbase)$datapath)
  #   }
  # })
  # 
  
  
  hot_path <- reactive ({
    
    #validate(
    #  need(input$file != "", label = "Please enter an XLSX file. XLS files are forbidden")
    #)
    
    #typeImport <- input$typeImport_single_sbase
    
    #if(typeImport=="sbase"){
      
      fb_temp <- input$sel_single_list_sbase
      
      
      if(is.null(fb_temp)){return()}
      if(!is.null(fb_temp)){
        
        #ToDo: Establish a conection with SBASE
        # (1) Find the location of the files in SBASE
        # (2) List Files from SBASE
        # (3) Read the fselected files
        
        # file.copy(fb_temp$datapath,paste(fb_temp$datapath, ".xlsx", sep=""))
        # fb_temp <- readxl::read_excel(paste(fb_temp$datapath, ".xlsx", sep=""), sheet = "FieldBook")
        fb_temp <- readRDS(sel_fb_temp)
        
        
      }
      
      fb_temp
    #}
    # 
    # if(typeImport=="local"){
    #   
    #   sel_fb_temp <- input$fileInput_single_sbase
    #   
    #   if(is.null(sel_fb_temp) || sel_fb_temp == ""){  return()  }
    #   if(length(sel_fb_temp)>0){
    #     
    #     #ToDo: Establish a conection with SBASE
    #     # (1) Find the location of the files in SBASE
    #     # (2) List Files from SBASE
    #     # (3) Read the fselected files
    #     fb_temp <- readRDS(sel_fb_temp)
    #     
    #   }
    #   
    # }
    # 
    #fb_temp
    
  })
  
  
  
  met_bdata <- reactive({
    hot_file <- hot_path()
    if(length(hot_file)==0){return (NULL)}
    if(length(hot_file)>0){
      
      cropfiles_list <- hot_file 
      
      n <- length(hot_file)
      combine <- list() 
      
      ENVIRONMENT <- vector(mode = "character", length = n )
      
      for(i in 1:n){  
        
        combine[[i]] <- readxl::read_excel(cropfiles_list[i], sheet = "Fieldbook") 
        
        Minimal <- readxl::read_excel(cropfiles_list[i], sheet = "Minimal") 
        
        #BOOK <- traittools::get_fb_param(Minimal,"Short_name")
        BOOK <- traittools::get_fb_param(Minimal,"Trial_name")
        DATE <- traittools::get_fb_param(Minimal,"Begin_date")
        #MONTH <- traittools::get_fb_param()
        ENVIRONMENT <- paste(traittools::get_fb_param(Minimal,"Site_short_name"), "_env_", i, sep = "")
        #BOOK <- getfilename_book(ammiafiles_list[i])
        #YEAR <- getdate_file(BOOK)$year
        #MONTH <- getdate_file(BOOK)$month
        #LOCATION <- getlocation_file(BOOK)
        
        #combine[[i]] <- cbind(BOOK,YEAR,MONTH,LOCATION,combine[[i]])
        combine[[i]] <- cbind(BOOK, DATE, ENVIRONMENT, combine[[i]])
      } 
      
      join_books <- data.table::rbindlist(combine,fill = TRUE)
      join_books <- as.data.frame(join_books)
      #write.csv(join_books,"join_books.csv")
      # join_books    
      
      #met_bdata <- readxl::read_excel(path=hot_file , sheet = "Fieldbook")
      met_bdata <- join_books
    }
  })
  
  output$genotypes_met_sbase  <- renderUI({
    
    # met_headers <- names(met_bdata())
    # 
    # gen_selection <- base::setdiff(met_headers, c("BOOK","DATE","PLOT")) 
    # 
    # selectInput('genotypes_met', 'Select genotypes', c(Choose='', gen_selection), 
    #             selectize=TRUE)
    
    selectInput('genotypes_met_sbase', 'Select genotypes', c(Choose='', select_options(met_bdata())),
                selectize=TRUE)
  })
  
  output$env_met_sbase  <- renderUI({
    
    # env_selection <- setdiff(names(met_bdata()), c("BOOK","DATE","PLOT"))
    # 
    # selectInput('env_met', 'Select environments', c(Choose='', env_selection),
    #             selectize=TRUE)
    
    selectInput('env_met_sbase', 'Select environments', c(Choose='', select_options(met_bdata())),
                selectize=TRUE)
  })
  
  output$rep_met_sbase  <- renderUI({
    
    # rep_selection <- setdiff(names(met_bdata()), c("BOOK","DATE","PLOT"))
    # 
    # selectInput('rep_met', 'Select replications', c(Choose='', rep_selection),
    #             selectize=TRUE)
    
    selectInput('rep_met_sbase', 'Select replications', c(Choose='', select_options(met_bdata())),
                selectize=TRUE)
  })
  
  output$trait_met_sbase <- renderUI({
    
    # trait_selection <- setdiff(names(met_bdata()), c("BOOK","DATE","PLOT"))
    # 
    # selectInput('trait_met', 'Select trait(s)', c(Choose='', trait_selection),
    #             selectize=TRUE, multiple = TRUE)
    
    selectInput('trait_met_sbase', 'Select trait(s)', c(Choose='', select_options(met_bdata())),
                selectize=TRUE, multiple = TRUE)
    
  })
  
  output$file_message_met_sbase <- renderInfoBox({
    
    #germoplasm <-material_table()$Institutional_number
    #germoplasm <-germoplasm_list()$institutional_number
    #print( germoplasm)
    
    hot_file <- hot_path()
    #print("omar")
    #print(hot_file)
    #print("omar2")
    if(is.null(hot_file)){
      infoBox(title="Select Fieldbook File", subtitle=
                paste("Choose at least 3 fieldbook files for MET"), icon = icon("upload", lib = "glyphicon"),
              color = "blue",fill = TRUE, width = NULL)
    } else {
      
      hot_file <- basename(hot_file)
      hot_file <- paste(hot_file, collapse = ", ")
      infoBox(title="GREAT!", subtitle =
                paste("Fieldbooks selected: ", hot_file),  icon = icon("ok", lib = "glyphicon"),
              color = "green",fill = TRUE, width = NULL)
    }
  })
  #     
  #     
  #     output$run_met <- renderUI({
  #       
  #       trait <- input$met_fb_trait
  #       genotypes <- input$met_fb_genotypes
  #       rep <- input$met_fb_rep 
  #     
  #       if(length(trait)==0 || length(genotypes)==0 || length(rep)==0 || is.null(hot_bdata)) return()
  #       actionButton(inputId = "met_button", label= "Analyze", icon = icon("play-circle"),
  #                  width = NULL,height = NULL) 
  #     })    
  
  shiny::observeEvent(input$met_sbase_button, {
    shiny::withProgress(message = "Opening Multi Enviroment Report...",value= 0,{
      
      #NOTE: To use pepa report package we need R 3.3.0 or more.
      #NOTE Finally, we always need pandoc installer.
      
      fieldbook <- as.data.frame(met_bdata())
      trait <- input$trait_met_sbase
      env <- input$env_met_sbase
      #print(trait)
      rep <- input$rep_met_sbase
      genotypes <- input$genotypes_met_sbase
      #format <- paste(input$format_met,"_document",sep="") deprecated version.
      format <- paste(input$format_met_sbase,sep="")
      
      try(pepa::repo.met(traits = trait, geno = genotypes, env = env, rep = rep, data = fieldbook, format=format))
      
    })
  })
  
} 


