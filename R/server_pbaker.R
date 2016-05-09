#' Server pbaker Analysis
#' 
#' @param input shinyserver input 
#' @param output shinyserver output
#' @param session shinyserver session
#' @param values reactive values
#' @importFrom shiny reactive tabPanel renderUI selectInput icon h2 uiOutput radioButtons actionButton br column fluidRow 
#' @importFrom shinydashboard infoBox tabBox infoBoxOutput renderInfoBox
#' @importFrom shinyFiles parseFilePaths

#' @author Omar Benites
#' @export

pbaker_server <- function(input, output, session, values){
  
  
  volumes <- shinyFiles::getVolumes()
  shinyFiles::shinyFileChoose(input, 'file_pbaker', roots=volumes, session=session,
                              restrictions=system.file(package='base'),filetypes=c('xlsx'))
  
  
  hot_path <- reactive ({
    
    if(length(input$file_pbaker)==0){return (NULL)}
    if(length(input$file_pbaker)>0){
      hot_file <- as.character(parseFilePaths(volumes, input$file_pbaker)$datapath)
    }
  })
  
  pbaker_bdata <- reactive({
    hot_file <- hot_path()
    if(length(hot_file)==0){return (NULL)}
    if(length(hot_file)>0){
      
      cropfiles_list <- hot_file 
      
      n <- length(hot_file)
      combine <- list() 
      
      for(i in 1:n){  
        combine[[i]] <- readxl::read_excel(cropfiles_list[i], sheet = "Fieldbook") 
        
        Minimal <- readxl::read_excel(cropfiles_list[i], sheet = "Minimal") 
        
        BOOK <- traittools::get_fb_param(Minimal,"Short_name")
        DATE <- traittools::get_fb_param(Minimal,"Begin_date")
        #MONTH <- traittools::get_fb_param()
        ENVIRONMENT <- traittools::get_fb_param(Minimal,"Site_short_name")
        #BOOK <- getfilename_book(ammiafiles_list[i])
        #YEAR <- getdate_file(BOOK)$year
        #MONTH <- getdate_file(BOOK)$month
        #LOCATION <- getlocation_file(BOOK)
        
        #combine[[i]] <- cbind(BOOK,YEAR,MONTH,LOCATION,combine[[i]])
        combine[[i]] <- cbind(BOOK, DATE, ENVIRONMENT, combine[[i]])
      } 
      
      join_books <- data.table::rbindlist(combine,fill = TRUE)
      join_books <- as.data.frame(join_books)
      write.csv(join_books,"join_books.csv")
      # join_books    
      
      #pbaker_bdata <- readxl::read_excel(path=hot_file , sheet = "Fieldbook")
      pbaker_bdata <- join_books
    }
  })
  
  output$genotypes_pbaker  <- renderUI({
    selectInput('genotypes_pbaker', 'Select Genotypes', c(Choose='', select_options(pbaker_bdata())), 
                selectize=TRUE)
  })
  
  output$env_pbaker  <- renderUI({
    selectInput('env_pbaker', 'Select Environments', c(Choose='', select_options(pbaker_bdata())),
                selectize=TRUE)
  })
  
  output$rep_pbaker  <- renderUI({
    selectInput('rep_pbaker', 'Select Repetitions', c(Choose='', select_options(pbaker_bdata())),
                selectize=TRUE)
  })
  
  output$trait_pbaker <- renderUI({
    selectInput('trait_pbaker', 'Select at least 2 Trait(s)', c(Choose='', select_options(pbaker_bdata())),
                selectize=TRUE, multiple = TRUE)
  })
  
  output$model_pbaker <- renderUI({
    selectInput('model_pbaker', 'Select Model', c('gxe (interaction)'='gxe', 
                                                  "g+e (no-interaction)"='g+e'),
                selectize=TRUE, multiple = FALSE)
  })
  
  output$file_message_pbaker <- renderInfoBox({
    
    hot_file <- hot_path()
   
    print(hot_file)
    if(is.null(hot_file)){
      infoBox(title="Select File", subtitle=
                paste("Choose at least 3 Fieldbook Files"), icon = icon("upload", lib = "glyphicon"),
              color = "blue",fill = TRUE, width = NULL)
    } else {
      hot_file <- basename(hot_file)
      infoBox(title="GREAT!", subtitle =
                paste("Your fieldbook has been Selected: ", hot_file),  icon = icon("ok", lib = "glyphicon"),
              color = "green",fill = TRUE, width = NULL)
    }
  })
  
  shiny::observeEvent(input$pbaker_button, {
    shiny::withProgress(message = "Opening pbaker Index Report...",value= 0,{
      
      fieldbook <- as.data.frame(pbaker_bdata())
      trait <- input$trait_pbaker
      print(trait)
      env <- input$env_pbaker
      print(env)
      rep <- input$rep_pbaker
      print(rep)
      genotypes <- input$genotypes_pbaker
      print(genotypes)
      model <- gsub(pattern = "[[:space:]]\\(.*", replacement = "", input$model_pbaker)
      print(model)
      
      format <- paste(input$format_pbaker,"_document",sep="")
      
      #try(pepa::pty.pesekbaker(traits = trait, geno = genotypes, env = env , model = model, data = fieldbook))
      try(pepa::pty.pesekbaker(traits = trait, geno = genotypes, env = env, rep = rep, data = fieldbook, format= format))
      
    })
  })
  
} 


