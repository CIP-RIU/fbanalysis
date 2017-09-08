#' Server for Genetic Analysis
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

genetic_server <- function(input, output, session, values){
  
  
  volumes <- shinyFiles::getVolumes()
  shinyFiles::shinyFileChoose(input, 'file_genetic', roots=volumes, session=session,
                              restrictions=system.file(package='base'),filetypes=c('xlsx'))
  
  
  hot_path <- reactive ({
    
    #validate(
    #  need(input$file != "", label = "Please enter an XLSX file. XLS files are forbidden")
    #)
    
    if(length(input$file_genetic)==0){return (NULL)}
    if(length(input$file_genetic)>0){
      hot_file <- as.character(parseFilePaths(volumes, input$file_genetic)$datapath)
    }
  })
  
  hot_bdata <- reactive({
    hot_file <- hot_path()
    if(length(hot_file)==0){return (NULL)}
    if(length(hot_file)>0){
      hot_bdata <- readxl::read_excel(path=hot_file , sheet = "Fieldbook")
    }
  })
  
  #Parameter for North Caroline I and II
  
  output$male_des_nc_genetic  <- renderUI({
    selectInput('male_nc_gen', 'Select Male', c(Choose='', select_options(hot_bdata())), 
                selectize=TRUE)
  })
  
  output$female_des_nc_genetic  <- renderUI({
    selectInput('female_nc_gen', 'Select Female', c(Choose='', select_options(hot_bdata())),
                selectize=TRUE)
  })
  
  output$set_des_nc_genetic <- renderUI({
    selectInput('set_nc_gen', 'Select Set', c(Choose='', select_options(hot_bdata())),
                selectize=TRUE, multiple = TRUE)
  })
  
  #Parameter for North Caroline I
  
  output$progeny_des_nc_genetic <- renderUI({
    selectInput('progeny_nc_gen', 'Select Progeny', c(Choose='', select_options(hot_bdata())),
                selectize=TRUE, multiple = TRUE)
  })
  
  
  
# Parameters for Line by tester ----------------------------------------------------------

  output$line_des_lxt_genetic <- renderUI({
    selectInput('line_lxt_gen', 'Select Line', c(Choose='', select_options(hot_bdata())),
                selectize=TRUE)
  })
  
  
  output$tester_des_lxt_genetic <- renderUI({
    selectInput('tester_lxt_gen', 'Select Tester', c(Choose='', select_options(hot_bdata())),
                selectize=TRUE)
  })
  
##  
 
  output$trait_des_genetic <- renderUI({
    selectInput('trait_gen', 'Select Trait(s)', c(Choose='', select_options(hot_bdata())),
                selectize=TRUE, multiple =TRUE)
  })
  
  
  output$rep_des_genetic <- renderUI({
    selectInput('rep_gen', 'Select Replications', c(Choose='', select_options(hot_bdata())),
                selectize=TRUE)
  })
  
  output$sch_des_lxt_genetic<- renderUI({
    #selectInput('sch_lxt_gen', 'Select scheme', c(Choose='', 1:2),
    selectInput('sch_lxt_gen', 'Select scheme', c(Choose='', list("progenitors and progenie"= 1, "progenie" =2)),
                selectize=TRUE)
  })
  
  
  
  output$file_message_genetic <- renderInfoBox({
    

    hot_file <- hot_path()
    print(hot_file)
    if(is.null(hot_file)){
      infoBox(title="Select fieldbook file", subtitle=
                paste("Choose your fieldbook file"), icon = icon("upload", lib = "glyphicon"),
              color = "blue",fill = TRUE, width = NULL)
       
    } else {
 
      hot_file <- basename(hot_file)
      hot_file <- paste(hot_file, collapse = ", ")
      infoBox(title="GREAT!", subtitle =
                paste(" Fieldbook selected: ", hot_file),  icon = icon("ok", lib = "glyphicon"),
              color = "green",fill = TRUE, width = NULL)
    }
  })

  shiny::observeEvent(input$genetic_button, {
    shiny::withProgress(message = "Opening Genetic Analysis Report...",value= 0,{
      
      #NOTE: To use pepa report package we need R 3.3.0 or more.
      #NOTE Finally, we always need pandoc installer.
      
      design <- input$design_genetic
      
      fieldbook <- as.data.frame(hot_bdata())
      
      #north carolina
      male <- input$male_nc_gen
      female <- input$female_nc_gen
      progeny <- input$progeny_nc_gen #Just for NCI
      set <- input$set_nc_gen 
      
      #line by tester
      line <- input$line_lxt_gen
      tester <- input$tester_lxt_gen
      
      #parameters for both designs
      trait <- input$trait_gen
      rep <- input$rep_gen
      
      
      scheme <- input$sch_lxt_gen
      
      print(scheme)
      
      format <- paste(input$format_genetic)
      
      if(design == "North Carolina Design I (NCI)"){
        #NCI consider progeny as entry parameter for data analysis
        try(pepa::repo.nc(traits = trait, set = set, male = male, female = female, progeny = progeny, rep = rep, model = 1, data = fieldbook, format = format))
        
      }
      
      if(design == "North Carolina Design II (NCII)"){
        #NCII consider progeny as entry parameter for data analysis
        try(pepa::repo.nc(traits = trait, set = set, male = male, female = female, rep = rep, model = 2, data = fieldbook, format = format))
        
      }
      
      if(design == "Line by Tester (LxT)"){
        #try(pepa::repo.abd(traits = trait, geno = genotypes, rep = rep, format = format, data = fieldbook))
        try(pepa::repo.lxt(traits = trait, line = line, tester = tester,rep = rep, scheme = scheme ,data = fieldbook, format = format))
      }
      
    })
  })

  
  
  
}

