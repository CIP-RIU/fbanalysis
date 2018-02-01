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
  
  output$rep_droindex  <- renderUI({
    selectInput('rep_droindex', 'Select Replications', c(Choose='', select_options(hot_bdata())),
                selectize=TRUE)
  })
  
  output$trait_droindex <- renderUI({
    selectInput('trait_droindex', 'Select Trait(s)', c(Choose='', select_options(hot_bdata())),
                selectize=TRUE, multiple = TRUE)
  })
  
  output$factor_droindex  <- renderUI({
    selectInput('factor_droindex', 'Select Factor', c(Choose='', select_options(hot_bdata())),
                selectize=TRUE)
  })
  
  output$block_droindex  <- renderUI({
    selectInput('block_droindex', 'Select Block', c(Choose='', select_options(hot_bdata())),
                selectize=TRUE)
  })
  
  output$k_droindex  <- renderUI({
    shiny::numericInput('k_droindex', 'Select Block Size',   value =2, min=2, max = 100)
  })    
  
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
    selectInput('sel_lvlcontrol_droindex', 'Select control level', c(Choose='',  lvls), 
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
  
  
  shiny::observeEvent(input$droindex_button, {
    shiny::withProgress(message = "Opening single Report...",value= 0,{
      
      #NOTE: To use pepa report package we need R 3.3.0 or more.
      #NOTE Finally, we always need pandoc installer.
      
      design <- input$design_droindex
      
      fieldbook <- as.data.frame(hot_bdata())
      #saveRDS(fieldbook,"res.rds")
      trait <- input$trait_droindex
      rep <- input$rep_droindex
      genotypes <- input$genotypes_droindex
      block <- input$block_droindex
      k <- input$k_droindex
      factor_droindex <- input$factor_droindex
      
      #format <- paste(input$format_droindex,"_document",sep="")
      format <- paste(input$format_droindex)
      
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
        
        try(pepa::repo.2f(traits = trait, A = genotypes, B = factor_droindex, rep = rep, design = "crd",  title= title, data = fieldbook, format = format))
      }
      
      if(design == "Factorial Two-Way Design in CRD (F2CRD)"){
        
        title <- paste("Automatic report for ", design, sep= "")
        try(pepa::repo.2f(traits = trait, A = genotypes, B = factor_droindex, rep = rep, design = "crd",  title= title, data = fieldbook, format = format))
      }
      
      if(design == "Split Plot with Plots in RCBD (SPRCBD)"){
        
        title <- paste("Automatic report for ", design, sep= "")
        try(pepa::repo.2f(traits = trait, A = genotypes, B = factor_droindex, rep = rep, design = "rcbd", title= title, data = fieldbook, format = format))
      }
      
      if(design == "Factorial Two-Way Design in RCBD (F2RCBD)"){
        
        title <- paste("Automatic report for ", design, sep= "")
        try(pepa::repo.2f(traits = trait, A = genotypes, B = factor_droindex, rep = rep, design = "rcbd", title= title, data = fieldbook, format = format))
      }
      
    })
  })
  
}

