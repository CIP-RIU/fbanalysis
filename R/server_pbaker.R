#' Server pbaker Analysis
#' 
#' @param input shinyserver input 
#' @param output shinyserver output
#' @param session shinyserver session
#' @param values reactive values
#' @importFrom shiny reactive tabPanel renderUI selectInput icon h2 uiOutput radioButtons actionButton br column fluidRow numericInput 
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
  
  output$model_pbaker <- renderUI({
    selectInput('model_pbaker', 'Select Model', c('gxe (interaction)'='gxe', 
                                                  "g+e (no-interaction)"='g+e'),
                selectize=TRUE, multiple = FALSE)
  })
  
  output$trait_pbaker <- renderUI({
    selectInput('trait_pbaker', 'Select at least 2 Trait(s)', c(Choose='', select_options(pbaker_bdata())),
                selectize=TRUE, multiple = TRUE)

  })

  output$weight_pbaker <- renderUI({
   
     
     trait <- as.character(input$trait_pbaker)
    if(length(trait)>0){
      
      
    lapply(1:length(trait), function(i) {
      print(i)
      
      numericInput(paste0("n_input_wpb_", trait[i]), label = paste0("Desired genetic gain for ", trait[i]), value = 1)
    })

    }
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
      print(fieldbook)
      trait <- input$trait_pbaker
      trait <- trait[trait!=""]
      print(trait)
      env <- input$env_pbaker
      print(env)
      rep <- input$rep_pbaker
      print(rep)
      genotypes <- input$genotypes_pbaker
      print(genotypes)
      model <- gsub(pattern = "[[:space:]]\\(.*", replacement = "", input$model_pbaker)
      print(model)
      
      units <- input$units_pbaker
      
      means<- input$means_pbaker
      
      #table of weights using pbaker index
      w_pbaker_table <- data.frame(lapply(1:length(trait), function(i) {
        input[[paste0("n_input_wpb_", trait[i])]]
      }))
      
#       res <<- w_pbaker_table
#       #print(w_pbaker_table)
#       a1 <<- trait
#       names(res) <- trait
      
      names(w_pbaker_table) <- trait
      
      if(length(trait)>=2)
      for(j in trait){
        fieldbook[,j] <- w_pbaker_table[,j]*fieldbook[,j]
      }
#       
#     fps <<- fieldbook
#     print(fps)
#       
      #format <- paste(input$format_pbaker,"_document",sep="")
      #format <- paste(input$format_pbaker,sep="")
      
      #TODO: Tener en cuenta cuando la matriz es singular, y no la puedes invertir.
      
      #try(pepa::pty.pesekbaker(traits = trait, geno = genotypes, env = env , model = model, data = fieldbook))
      try(pepa::pty.pesekbaker(traits = trait, geno = genotypes, env = env, rep = rep,
                               means = means,model = model, units = units,  data = fieldbook))
      
    })
  })
  
} 


