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
  
  
  # volumes <- shinyFiles::getVolumes()
  # shinyFiles::shinyFileChoose(input, 'file_single_sbase', roots=volumes, session=session,
  #                             restrictions=system.file(package='base'),filetypes=c('xlsx'))
  
  
  hot_bdata <- reactive ({
    
    validate(
     need(input$sel_single_list_sbase != "", label = "Please enter an XLSX file. XLS files are forbidden")
    )
    sel_fb_temp <- input$sel_single_list_sbase
    
    if(is.null(sel_fb_temp) || sel_fb_temp == ""){  return()  }
    if(length(sel_fb_temp)>0){
      
      #ToDo: Establish a conection with SBASE
      # (1) Find the location of the files in SBASE
      # (2) List Files from SBASE
      # (3) Read the fselected files
      fb_temp <- readRDS(sel_fb_temp)
      
    }
    
    # }
    
    fb_temp
    
    
   #typeImport <- input$typeImport_single_sbase
   # if(typeImport=="sbase"){
   #    
   #    fb_temp <- input$fileInput_single_sbase
   #    
   #    if(is.null(fb_temp)){return()}
   #    if(!is.null(fb_temp)){
   #      
   #      file.copy(fb_temp$datapath,paste(fb_temp$datapath, ".xlsx", sep=""))
   #      fb_temp <- readxl::read_excel(paste(fb_temp$datapath, ".xlsx", sep=""), sheet = "FieldBook")
   #      
   #      fb_temp <- as.list(fb_temp) #mtl in list format
   #    }
   #    
   #    
   #  }
   #  fb_temp
    
  })
  
  # hot_bdata <- reactive({
  #   hot_file <- hot_path()
  #   if(length(hot_file)==0){return (NULL)}
  #   if(length(hot_file)>0){
  #     hot_bdata <- readxl::read_excel(path=hot_file , sheet = "Fieldbook")
  #   }
  # })
  
  
  output$genotypes_single_sbase  <- renderUI({
    selectInput('genotypes_single_sbase', 'Select Genotypes', c(Choose='', select_options(hot_bdata())), 
                selectize=TRUE)
  })
  
  
  output$rep_single_sbase  <- renderUI({
    selectInput('rep_single_sbase', 'Select Replications', c(Choose='', select_options(hot_bdata())),
                selectize=TRUE)
  })
  
  
  output$trait_single_sbase <- renderUI({
    selectInput('trait_single_sbase', 'Select Trait(s)', c(Choose='', select_options(hot_bdata())),
                selectize=TRUE, multiple = TRUE)
  })
  
  
  output$factor_single_sbase  <- renderUI({
    selectInput('factor_single_sbase', 'Select Factor', c(Choose='', select_options(hot_bdata())),
                selectize=TRUE)
  })
  
  
  output$block_single  <- renderUI({
    selectInput('block_single_sbase', 'Select Block', c(Choose='', select_options(hot_bdata())),
                selectize=TRUE)
  })
  
  
  output$k_single_sbase  <- renderUI({
    shiny::numericInput('k_single_sbase', 'Select Block Size',   value =2, min=2, max = 100)
  })    
  
  
  output$file_message_single_sbase <- renderInfoBox({
    
    #germoplasm <-material_table()$Institutional_number
    #germoplasm <-germoplasm_list()$institutional_number
    #print( germoplasm)
    
    #hot_file <- hot_path()
    hot_file <- input$sel_single_list_sbase
    print(hot_file)
    if(is.null(hot_file)){
      infoBox(title="Select fieldbook file", subtitle=
                paste("Choose your fieldbook file"), icon = icon("upload", lib = "glyphicon"),
              color = "blue",fill = TRUE, width = NULL)
      #      }
      #       else if(all(is.na(germoplasm))) {
      #         infoBox(title="ERROR", subtitle=
      #                   paste("Your material list", "is empty. Please check it"), icon = icon("warning-sign", lib = "glyphicon"),
      #                 color = "red",fill = TRUE, width = NULL)
      #         #shell.exec(hot_path())
      #         
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
  #     
  #     
  #     output$run_single <- renderUI({
  #       
  #       trait <- input$single_fb_trait
  #       genotypes <- input$single_fb_genotypes
  #       rep <- input$single_fb_rep 
  #     
  #       if(length(trait)==0 || length(genotypes)==0 || length(rep)==0 || is.null(hot_bdata)) return()
  #       actionButton(inputId = "single_button", label= "Analyze", icon = icon("play-circle"),
  #                  width = NULL,height = NULL) 
  #     })    
  
  shiny::observeEvent(input$single_button_sbase, {
    shiny::withProgress(message = "Opening single Report...",value= 0,{
      
      #NOTE: To use pepa report package we need R 3.3.0 or more.
      #NOTE Finally, we always need pandoc installer.
      
      design <- input$design_single_sbase
      
      fieldbook <- as.data.frame(hot_bdata())
      #saveRDS(fieldbook,"res.rds")
      trait <- input$trait_single_sbase
      rep <- input$rep_single_sbase
      genotypes <- input$genotypes_single_sbase
      block <- input$block_single_sbase
      k <- input$k_single_sbase
      factor_single <- input$factor_single_sbase
      
      #format <- paste(input$format_single,"_document",sep="")
      format <- paste(input$format_single_sbase)
      
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
        
        try(pepa::repo.2f(traits = trait, A = genotypes, B = factor_single, rep = rep, design = "crd",  title= title, data = fieldbook, format = format))
      }
      
      if(design == "Factorial Two-Way Design in CRD (F2CRD)"){
        
        title <- paste("Automatic report for ", design, sep= "")
        try(pepa::repo.2f(traits = trait, A = genotypes, B = factor_single, rep = rep, design = "crd",  title= title, data = fieldbook, format = format))
      }
      
      if(design == "Split Plot with Plots in RCBD (SPRCBD)"){
        
        title <- paste("Automatic report for ", design, sep= "")
        try(pepa::repo.2f(traits = trait, A = genotypes, B = factor_single, rep = rep, design = "rcbd", title= title, data = fieldbook, format = format))
      }
      
      if(design == "Factorial Two-Way Design in RCBD (F2RCBD)"){
        
        title <- paste("Automatic report for ", design, sep= "")
        try(pepa::repo.2f(traits = trait, A = genotypes, B = factor_single, rep = rep, design = "rcbd", title= title, data = fieldbook, format = format))
      }
      
    })
  })
  

  
  
}

