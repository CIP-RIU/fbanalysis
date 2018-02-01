#' Server for Drought Index
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

droindex_server <- function(input, output, session, values){
  
  
  volumes <- shinyFiles::getVolumes()
  shinyFiles::shinyFileChoose(input, 'file_droindex', roots=volumes, session=session,
                              restrictions=system.file(package='base'),filetypes=c('xlsx'))
  
  
  hot_path <- reactive ({

    
    if(length(input$file_droindex)==0){return (NULL)}
    if(length(input$file_droindex)>0){
      hot_file <- as.character(parseFilePaths(volumes, input$file_droindex)$datapath)
    }
  })
  
  hot_bdata <- reactive({
    hot_file <- hot_path()
    if(length(hot_file)==0){return (NULL)}
    if(length(hot_file)>0){
      hot_bdata <- readxl::read_excel(path=hot_file , sheet = "Fieldbook")
    }
  })
  
  output$genotypes_droindex  <- renderUI({
    selectInput('genotypes_droindex', 'Select Genotypes', c(Choose='', select_options(hot_bdata())), 
                selectize=TRUE)
  })
  
  # output$rep_droindex  <- renderUI({
  #   selectInput('rep_droindex', 'Select Replications', c(Choose='', select_options(hot_bdata())),
  #               selectize=TRUE)
  # })
  # 
  output$trait_droindex <- renderUI({
    selectInput('trait_droindex', 'Select Trait(s)', c(Choose='', select_options(hot_bdata())),
                selectize=TRUE, multiple = FALSE)
  })
  
  output$factor_droindex  <- renderUI({
    selectInput('factor_droindex', 'Select Factor', c(Choose='', select_options(hot_bdata())),
                selectize=TRUE)
  })
  
  output$block_droindex  <- renderUI({
    selectInput('block_droindex', 'Select Block', c(Choose='', select_options(hot_bdata())),
                selectize=TRUE)
  })
  
  # output$k_droindex  <- renderUI({
  #   shiny::numericInput('k_droindex', 'Select Block Size',   value =2, min=2, max = 100)
  # })    
  # 
  output$lvl_stress_droindex  <- renderUI({
    #shiny::numericInput('k_droindex', 'Select Block Size',   value =2, min=2, max = 100)
    #fb <- hot_bdata()
    req(input$factor_droindex)
    col_factor <- input$factor_droindex
    lvls <- unique( hot_bdata()[ , col_factor])
    selectInput('sel_lvlstress_droindex', 'Select stress level', c(Choose='', lvls), 
                selectize=TRUE)
    
  })    
  
  output$lvl_control_droindex  <- renderUI({
    #shiny::numericInput('k_droindex', 'Select Block Size',   value =2, min=2, max = 100)
    req(input$factor_droindex)
    col_factor <- input$factor_droindex
    lvls <- unique(hot_bdata()[ , col_factor])
    selectInput('sel_lvlcontrol_droindex', 'Select non-stressed level', c(Choose='',  lvls), 
                selectize=TRUE)
  })    
  
  
  output$file_message_droindex <- renderInfoBox({
    
    hot_file <- hot_path()
    print(hot_file)
    if(is.null(hot_file)){
      infoBox(title="Select fieldbook file", subtitle=
                paste("Choose your fieldbook file"), icon = icon("upload", lib = "glyphicon"),
              color = "blue",fill = TRUE, width = NULL)
    
    } else {
      #       material <- paste(germoplasm, collapse = ",")
      #       message <-  paste("Material list imported: ", material)
      hot_file <- basename(hot_file)
      hot_file <- paste(hot_file, collapse = ", ")
      infoBox(title="GREAT!", subtitle =
                paste(" Fieldbook selected: ", hot_file),  icon = icon("ok", lib = "glyphicon"),
              color = "green",fill = TRUE, width = NULL)
    }
  })
  
  
  di_data <- reactive({ 
  
      design <- input$design_droindex
  
      fieldbook <- as.data.frame(hot_bdata())
      trait <- input$trait_droindex
      rep <- input$rep_droindex
      genotypes <- input$genotypes_droindex
      #block <- input$block_droindex
      #k <- input$k_droindex
      factor_droindex <- input$factor_droindex
      
      stress_lvl <- input$sel_lvlstress_droindex
      control_lvl <- input$sel_lvlcontrol_droindex
      #format <- paste(input$format_droindex)
      #print(out)
      out <- sbformula::DSI.means(trait = trait, geno= genotypes, dfactor = factor_droindex, clabel = control_lvl, slabel = stress_lvl, fb=fieldbook)
      
  })   
      
  output$tbl <-  DT::renderDataTable({
    req(input$file_droindex)
    req(input$genotypes_droindex)
    req(input$factor_droindex)
    req(input$sel_lvlstress_droindex)
    req(input$sel_lvlcontrol_droindex)
    
    out <- di_data()  
    out
    
  })
  
  shiny::observeEvent(input$droindex_button,{
    
    #For single Fieldbooks
    withProgress(message = "Add Drought indeces worksheet into fieldbook file...",value= 0,
                 {
                   incProgress(2/15,message = "detection of sheets..")
                   DF <- di_data()
                   print("detection of sheets")
                   hot_file <- hot_path() 
                   try(wb <- openxlsx::loadWorkbook(hot_file))
                   sheets <- readxl::excel_sheets(path = hot_file)
                   
                   print("after loadworkbook")
                   
                   if(is.element("Drought_indeces",sheets)){    
                     try( openxlsx::removeWorksheet(wb, "Drought_indeces"))
                   }
                   incProgress(7/15, message = "Adding worksheet...")
                   try(openxlsx::addWorksheet(wb = wb,sheetName = "Drought_indeces",gridLines = TRUE))
                   try(openxlsx::writeDataTable(wb,sheet = "Drought_indeces", x = DF,colNames = TRUE, withFilter = FALSE))
                   try(openxlsx::saveWorkbook(wb = wb, file = hot_file, overwrite = TRUE) )
                   print("ejecucion")
                   try(shell.exec(hot_file))
                   incProgress(15/15, message = "Exporting fieldbook file...")
                 })
  }) 
  
}

