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

single_server <- function(input, output, session, values){
  

  volumes <- shinyFiles::getVolumes()
  shinyFiles::shinyFileChoose(input, 'file_single', roots=volumes, session=session,
                  restrictions=system.file(package='base'),filetypes=c('xlsx'))
  
  
  hot_path <- reactive ({
    
    #validate(
    #  need(input$file != "", label = "Please enter an XLSX file. XLS files are forbidden")
    #)
    
    if(length(input$file_single)==0){return (NULL)}
    if(length(input$file_single)>0){
      hot_file <- as.character(parseFilePaths(volumes, input$file_single)$datapath)
    }
  })
  
  hot_bdata <- reactive({
    hot_file <- hot_path()
    if(length(hot_file)==0){return (NULL)}
    if(length(hot_file)>0){
      hot_bdata <- readxl::read_excel(path=hot_file , sheet = "Fieldbook")
    }
  })
  
    output$genotypes_single  <- renderUI({
      selectInput('genotypes_single', 'Select Genotypes', c(Choose='', select_options(hot_bdata())), 
                  selectize=TRUE)
    })
    
    output$rep_single  <- renderUI({
      selectInput('rep_single', 'Select Repetitions', c(Choose='', select_options(hot_bdata())),
                  selectize=TRUE)
    })
    
    output$trait_single <- renderUI({
      selectInput('trait_single', 'Select Trait(s)', c(Choose='', select_options(hot_bdata())),
                  selectize=TRUE, multiple = TRUE)
    })
    
    output$factor_single  <- renderUI({
      selectInput('factor_single', 'Select Factor', c(Choose='', select_options(hot_bdata())),
                  selectize=TRUE)
    })
    
    
    output$block_single  <- renderUI({
      selectInput('block_single', 'Select Block', c(Choose='', select_options(hot_bdata())),
                  selectize=TRUE)
    })
    
    output$k_single  <- renderUI({
      shiny::numericInput('k_single', 'Select Block Size',   value =2, min=2, max = 100)
    })    
    
    output$file_message_single <- renderInfoBox({
      
      #germoplasm <-material_table()$Institutional_number
      #germoplasm <-germoplasm_list()$institutional_number
      #print( germoplasm)
      
      hot_file <- hot_path()
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
      
  shiny::observeEvent(input$single_button, {
    shiny::withProgress(message = "Opening single Report...",value= 0,{

      #NOTE: To use pepa report package we need R 3.3.0 or more.
      #NOTE Finally, we always need pandoc installer.
      
      design <- input$design_single
     
      fieldbook <- as.data.frame(hot_bdata())
      #saveRDS(fieldbook,"res.rds")
      
      trait <- input$trait_single
      
      rep <- input$rep_single
      
      genotypes <- input$genotypes_single
      
      block <- input$block_single
      k <- input$k_single
      
      factor_single <- input$factor_single
     
      #format <- paste(input$format_single,"_document",sep="")
      format <- paste(input$format_single)
      
      
      
      
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
  
  
  hot_check_single_fb <- reactive({
    
    req(input$trait_single)
    req(input$rep_single)
    req(input$genotypes_single)
    
    trait <- input$trait_single
    rep <- input$rep_single
    genotypes <- input$genotypes_single
    design <- input$design_single
    factorb <- input$factor_single
    
    n <- length(trait)
    fb <- as.data.frame(hot_bdata())
    
    if(!is.null(trait) && !is.null(rep) && !is.null(genotypes)) {
    
    
      if(design == 'Completely Randomized Design (CRD)' || design  == 'Randomized Complete Block Design (RCBD)') {
        
        temp_colum <- lapply(X = 1:n, function(x) (single_error(mve.rcbd(trait = trait[x], treat = genotypes, rep = rep, data = fb), trait[x])))
        
      }
      
      if(design == "Factorial Two-Way Design in CRD (F2CRD)"){

        temp_colum <- lapply(X = 1:n, function(x) (single_error(mve.2f(trait = trait, A = genotypes, B = factorb, rep = rep, design = "crd",data = fb))))
      }

      if(design  == "Factorial Two-Way Design in RCBD (F2RCBD)"){

        temp_colum <- lapply(X = 1:n, function(x) (single_error(mve.2f(trait = trait, A = genotypes, B = factorb, rep = rep, design = "rcbd",data = fb))))

      } 
      
      e_trait <- temp_colum %>% map_chr("trait")
      e_error <- temp_colum %>% map_chr("error")
      
      #out <- paste("Trait Status of ", e_trait, ": ", e_error, sep="")
      out <- list( e_trait =  e_trait, e_error = e_error)
      #out <- paste(out, sep= "\n")
      #out <-   paste("hello", "world", sep="\n")
      
    } else {
      
      out <- NULL
    }
    
    out
    
  })
  
  
  output$single_anova_fail_message = renderRHandsontable({
    
    if(!is.null(hot_check_single_fb())) {
       msg <-  hot_check_single_fb() 
      # #HTML(paste(msg, sep = '<br/>'))
      # out <- paste("Trait Status of ", msg$e_trait, ": ", msg$e_error, sep="")
      # #msg <- paste(msg)
      # msg <- HTML(paste(out , sep = '<br/>'))
      df <- data.frame(trait = msg$e_trait, status = msg$e_error)
      rhandsontable(df)
       #msg <- paste(msg)
    } else {
      df <- data.frame()
      rhandsontable(df)
    }
    
  })
  
  
  
  
  #output$single_anova_fail_message <- shiny::renderText({
  
#   output$single_anova_fail_message <-renderUI({
#     if(!is.null(hot_check_single_fb())) {
#       
#         msg <-  hot_check_single_fb() 
#         #HTML(paste(msg, sep = '<br/>'))
#         out <- paste("Trait Status of ", msg$e_trait, ": ", msg$e_error, sep="")
#         #msg <- paste(msg)
#         msg <- HTML(paste(out , sep = '<br/>'))
#         
#         #msg <- paste(msg)  
#     } else {
#         msg <- paste("")
#     }
#     msg
#   })
#   
# } 



}

