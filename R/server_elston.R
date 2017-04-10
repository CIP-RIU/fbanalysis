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

elston_server <- function(input, output, session, values){
  
  
  volumes <- shinyFiles::getVolumes()
  shinyFiles::shinyFileChoose(input, 'file_elston', roots=volumes, session=session,
                              restrictions=system.file(package='base'),filetypes=c('xlsx'))
   
  
  hot_path <- reactive ({

    if(length(input$file_elston)==0){return (NULL)}
    if(length(input$file_elston)>0){
      hot_file <- as.character(parseFilePaths(volumes, input$file_elston)$datapath)
    }
  })
  
  elston_bdata <- reactive({
    hot_file <- hot_path()
    if(length(hot_file)==0){return (NULL)}
    if(length(hot_file)>0){
      
      cropfiles_list <- hot_file 
      
      n <- length(hot_file)
      combine <- list() 
      
      for(i in 1:n){  
        combine[[i]] <- readxl::read_excel(cropfiles_list[i], sheet = "Fieldbook") 
        
        Minimal <- readxl::read_excel(cropfiles_list[i], sheet = "Minimal") 
        
        #BOOK <- traittools::get_fb_param(Minimal,"Short_name")
        BOOK <- traittools::get_fb_param(Minimal,"Trial_name")
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
      #write.csv(join_books,"join_books.csv")
      # join_books    
      
      #elston_bdata <- readxl::read_excel(path=hot_file , sheet = "Fieldbook")
      elston_bdata <- join_books
    }
  })
  
  output$genotypes_elston  <- renderUI({
    selectInput('genotypes_elston', 'Select Genotypes', c(Choose='', select_options(elston_bdata())), 
                selectize=TRUE)
  })
  
  output$env_elston  <- renderUI({
    selectInput('env_elston', 'Select Environments', c(Choose='', select_options(elston_bdata())),
                selectize=TRUE)
  })
  
  output$rep_elston  <- renderUI({
    selectInput('rep_elston', 'Select Replications', c(Choose='', select_options(elston_bdata())),
                selectize=TRUE)
  })
  
  output$trait_posElston <- renderUI({
    selectInput('trait_pos_elston', 'Select Positive Trait(s)', c(Choose='', select_options(elston_bdata())),
                selectize=TRUE, multiple = TRUE)
  })
  
  
  output$trait_negElston <- renderUI({
    selectInput('trait_neg_elston', 'Select Negative Trait(s)', c(Choose='', select_options(elston_bdata())),
                selectize=TRUE, multiple = TRUE)
  })
  

  output$model_elston <- renderUI({
    selectInput('model_elston', 'Select Model', c('gxe (interaction)'='gxe', 
                                                           "g+e (no-interaction)"='g+e'),
                selectize=TRUE, multiple = FALSE)
  })
  

  output$file_message_elston <- renderInfoBox({

    hot_file <- hot_path()
    print(hot_file)
    if(is.null(hot_file)){
      infoBox(title="Select File", subtitle=
                paste("Choose your Fieldbook File"), icon = icon("upload", lib = "glyphicon"),
              color = "blue",fill = TRUE, width = NULL)
     } else {
      hot_file <- basename(hot_file)
      infoBox(title="GREAT!", subtitle =
                paste(" Fieldbook Selected: ", hot_file),  icon = icon("ok", lib = "glyphicon"),
              color = "green",fill = TRUE, width = NULL)
    }
  })
 
  shiny::observeEvent(input$elston_button, {
    shiny::withProgress(message = "Opening Elston  Index Report...",value= 0,{
  
      fieldbook <- as.data.frame(elston_bdata())
      
      trait_pos <- input$trait_pos_elston
      #print(trait_pos)
      trait_neg <- input$trait_neg_elston
      #print(trait_neg)
      trait <- c(trait_pos,trait_neg)
      trait <- trait[trait!=""]
      #trait <- input$trait_elston
      env <- input$env_elston
      #print(input$env_elston)
      rep <- input$rep_elston
      genotypes <- input$genotypes_elston
      model <- gsub(pattern = "[[:space:]]\\(.*", replacement = "", input$model_elston)
      
      means <- input$means_elston
      model <- input$model_elston
#       print(input$means_elston)
#       print(input$model_elston)
#       
      if(length(trait_neg)>0){
        fieldbook[,trait_neg] <- -fieldbook[,trait_neg]
      }
      
      
      #format <- paste(input$format_elston,"_document",sep="")
      format <- paste(input$format_elston,sep="")
      
      try(pepa::pty.elston(traits = trait, geno = genotypes, model = model, env= env, rep = rep, means = means,
                           data = fieldbook, format = format))
      
      if(env=="" && model=="g+e" && means=="single"){
        try(pepa::pty.elston(traits = trait, geno = genotypes, model = model, means = means, data = fieldbook, format = format))
      }
      
      if(env=="" && model=="gxe" && means=="single"){
        try(pepa::pty.elston(traits = trait, geno = genotypes, model = model, means = means, data = fieldbook, format = format))
      } 
      
#       if(env=="" && model=="g+e" && means=="single"){
#         try(pepa::pty.elston(traits = trait, geno = genotypes, model = model, means = means, data = fieldbook))
#       } 
      
      
      
      
    })
  })
  
} 


