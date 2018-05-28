#' Server for Linear Discriminant Analysis in HiDAP
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

lda_server <- function(input, output, session, values){
  
  
  volumes <- shinyFiles::getVolumes()
  shinyFiles::shinyFileChoose(input, 'file_lda', roots=volumes, session=session,
                              restrictions=system.file(package='base'),filetypes=c('xlsx'))
  
  
  hot_path <- reactive ({
    
    #validate(
    #  need(input$file != "", label = "Please enter an XLSX file. XLS files are forbidden")
    #)
    
    if(length(input$file_lda)==0){return (NULL)}
    if(length(input$file_lda)>0){
      hot_file <- as.character(parseFilePaths(volumes, input$file_lda)$datapath)
    }
  })
  
  hot_bdata <- reactive({
    hot_file <- hot_path()
    if(length(hot_file)==0){return (NULL)}
    if(length(hot_file)>0){
      hot_bdata <- readxl::read_excel(path=hot_file , sheet = "Fieldbook")
    }
  })
  
  output$uigenotypes_lda  <- renderUI({
    selectInput('genotypes_lda', 'Select Genotypes', c(Choose='', select_options(hot_bdata())), 
                selectize=TRUE)
  })
  
  # output$uirep_lda  <- renderUI({
  #   selectInput('rep_lda', 'Select Replications', c(Choose='', select_options(hot_bdata())),
  #               selectize=TRUE)
  # })
  
  output$uitrait_lda <- renderUI({
    selectInput('trait_lda', 'Select Trait(s)', c(Choose='', select_options(hot_bdata())),
                selectize=TRUE, multiple = TRUE)
  })
  
  output$uifactor_lda  <- renderUI({
    selectInput('factor_lda', 'Select Factor', c(Choose='', select_options(hot_bdata())),
                selectize=TRUE)
  })
  
  # output$lvl_factor_lda  <- renderUI({
  #   
  #   req(input$sel_factor_lda)
  #   factor_lbl <- input$sel_factor_lda
  #   factor_levels <- hot_bdata() %>% dplyr::select_(factor_lbl) %>% dplyr::pull()
  #   #selectInput('lvlFactor_lda', 'Select Factor', c(Choose='', select_options(hot_bdata())), selectize=TRUE)
  #   selectInput('sel_lvlFactor_lda', 'Select level(s) of the factor', c(Choose='', factor_levels), selectize=TRUE)
  #   
  # })
  
  output$x12 = DT::renderDataTable({
    
    req(input$factor_lda)
    req(input$trait_lda)
    
    fb <- as.data.frame(hot_bdata())
    treatment <- c(input$factor_lda, input$trait_lda)
    factor_lda <- input$factor_lda

    fb <- fb[treatment]
    fb <- na.omit(fb) #remove cases with NA values
    fb[,factor_lda] <- ordered(fb[, factor_lda]) #order the levels of the factor. It must be do it.
    
    # 4.3. Generate a Train and Test data set
    set.seed(1)
    intrain <- sample(nrow(fb), round(0.50*nrow(fb)))
    train <- fb[intrain, ]
    test <<- fb[-intrain, ]
    
    # 4.4. Calculating the variable importance.
    formula <- as.formula(paste(factor_lda,  "~.", sep=" ")  )
  
    model <- randomForest(formula, data = train)
    pred <- predict(model, newdata = test)
    # ct <- table(fb[,factor], pred)
    # sum(diag(prop.table(ct)))
    gini <- data.frame(model$importance)
    gini$trait <- rownames(gini)
    gini[sort.int(gini$MeanDecreaseGini, decreasing = T, index.return = T)$ix, ]
    
    gini_table <- gini[sort.int(gini$MeanDecreaseGini, decreasing = T, index.return = T)$ix, ]
    df <- gini_table
        
  })
  
  
  
  output$uiblock_lda  <- renderUI({
    selectInput('block_lda', 'Select Block', c(Choose='', select_options(hot_bdata())),
                selectize=TRUE)
  })
  
  output$uik_lda  <- renderUI({
    shiny::numericInput('k_lda', 'Select Block Size',   value =2, min=2, max = 100)
  })    
  
  output$file_message_lda <- renderInfoBox({
    
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
      hot_file <- paste(hot_file, collapse = ", ")
      infoBox(title="GREAT!", subtitle =
                paste(" Fieldbook selected: ", hot_file),  icon = icon("ok", lib = "glyphicon"),
              color = "green",fill = TRUE, width = NULL)
    }
  })
  
  #     
  #     
  #     output$run_lda <- renderUI({
  #       
  #       trait <- input$lda_fb_trait
  #       genotypes <- input$lda_fb_genotypes
  #       rep <- input$lda_fb_rep 
  #     
  #       if(length(trait)==0 || length(genotypes)==0 || length(rep)==0 || is.null(hot_bdata)) return()
  #       actionButton(inputId = "lda_button", label= "Analyze", icon = icon("play-circle"),
  #                  width = NULL,height = NULL) 
  #     })    
  
  shiny::observeEvent(input$lda_button, {
    shiny::withProgress(message = "Opening lda Report...",value= 0,{
      
      #NOTE: To use pepa report package we need R 3.3.0 or more.
      #NOTE Finally, we always need pandoc installer.
      
      #design <- input$design_lda
      
      fieldbook <- as.data.frame(hot_bdata(), stringsAsFactors=FALSE)
      #saveRDS(fieldbook,"res.rds")
      trait <- input$trait_lda
      rep <- input$rep_lda
      genotypes <- input$genotypes_lda
      factor_lda <- input$factor_lda
      block <- input$block_lda
      k <- input$k_lda
      
      
      #format <- paste(input$format_lda,"_document",sep="")
      format <- paste(input$format_lda)
      
      try(fbanalysis::repo_lda(traits = trait, geno = genotypes, factor = factor_lda, data = fieldbook, format = format))
      
      
      # if(design == "Randomized Complete Block Design (RCBD)"){
      #   try(pepa::repo.rcbd(traits = trait, geno = genotypes, rep = rep, format = format, data = fieldbook))
      # }
      # 
      # if(design == "Completely Randomized Design (CRD)"){
      #   try(pepa::repo.crd(traits = trait, geno = genotypes, format = format, data = fieldbook))
      #   #try(pepa::repo.crd(traits = trait, geno = genotypes, rep = rep, format = format, data = fieldbook))
      # }
      # 
      # if(design == "Augmented Block Design (ABD)"){
      #   #try(pepa::repo.abd(traits = trait, geno = genotypes, format = format, data = fieldbook))
      #   try(pepa::repo.abd(traits = trait, geno = genotypes, rep = rep, format = format, data = fieldbook))
      # }
      # 
      # if(design == "Alpha Design(0,1) (AD)"){
      #   #try(pepa::repo.abd(traits = trait, geno = genotypes, format = format, data = fieldbook))
      #   try(pepa::repo.a01d(traits = trait, geno = genotypes, rep = rep, block = block, k = k, data = fieldbook, format = format))
      # }
      # 
      # if(design == "Split Plot with Plots in CRD (SPCRD)"){
      #   
      #   title <- paste("Automatic report for ", design, sep= "")
      #   
      #   try(pepa::repo.2f(traits = trait, A = genotypes, B = factor_lda, rep = rep, design = "crd",  title= title, data = fieldbook, format = format))
      # }
      # 
      # if(design == "Factorial Two-Way Design in CRD (F2CRD)"){
      #   
      #   title <- paste("Automatic report for ", design, sep= "")
      #   try(pepa::repo.2f(traits = trait, A = genotypes, B = factor_lda, rep = rep, design = "crd",  title= title, data = fieldbook, format = format))
      # }
      # 
      # if(design == "Split Plot with Plots in RCBD (SPRCBD)"){
      #   
      #   title <- paste("Automatic report for ", design, sep= "")
      #   try(pepa::repo.2f(traits = trait, A = genotypes, B = factor_lda, rep = rep, design = "rcbd", title= title, data = fieldbook, format = format))
      # }
      # 
      # if(design == "Factorial Two-Way Design in RCBD (F2RCBD)"){
      #   
      #   title <- paste("Automatic report for ", design, sep= "")
      #   try(pepa::repo.2f(traits = trait, A = genotypes, B = factor_lda, rep = rep, design = "rcbd", title= title, data = fieldbook, format = format))
      # }
      
    })
  })
  
  # 
  # hot_check_lda_fb <- reactive({
  #   
  #   req(input$trait_lda)
  #   req(input$rep_lda)
  #   req(input$genotypes_lda)
  #   
  #   trait <- input$trait_lda
  #   rep <- input$rep_lda
  #   genotypes <- input$genotypes_lda
  #   design <- input$design_lda
  #   factorb <- input$factor_lda
  #   block <- input$block_lda
  #   
  #   n <- length(trait)
  #   fb <- as.data.frame(hot_bdata())
  #   
  #   if(!is.null(trait) && !is.null(rep) && !is.null(genotypes)) {
  #   
  #   
  #     if(design == 'Completely Randomized Design (CRD)' || design  == 'Randomized Complete Block Design (RCBD)') {
  #       
  #       temp_colum <- lapply(X = 1:n, function(x) (lda_error(mve.rcbd(trait = trait[x], treat = genotypes, rep = rep, data = fb), trait[x])))
  #       e_trait <- temp_colum %>% map_chr("trait")
  #       e_error <- temp_colum %>% map_chr("error")
  #       out <- list( e_trait =  e_trait, e_error = e_error)
  #       
  #     }
  #     
  #     if(design == "Factorial Two-Way Design in CRD (F2CRD)"){
  # 
  #       temp_colum <- lapply(X = 1:n, function(x) (lda_error(mve.2f(trait = trait[x], A = genotypes, B = factorb, rep = rep, design = "crd", data = fb),trait[x] )))
  #       e_trait <- temp_colum %>% map_chr("trait")
  #       e_error <- temp_colum %>% map_chr("error")
  #       out <- list( e_trait =  e_trait, e_error = e_error)
  #     }
  # 
  #     if(design  == "Factorial Two-Way Design in RCBD (F2RCBD)"){
  # 
  #       temp_colum <- lapply(X = 1:n, function(x) (lda_error(mve.2f(trait = trait[x], A = genotypes, B = factorb, rep = rep, design = "rcbd",data = fb), trait[x])))
  #       e_trait <- temp_colum %>% map_chr("trait")
  #       e_error <- temp_colum %>% map_chr("error")
  #       out <- list( e_trait =  e_trait, e_error = e_error)
  #       
  #     } 
  #     
  #     if(design == 'Split Plot with Plots in CRD (SPCRD)'){
  #       
  #       temp_colum <- lapply(X = 1:n, function(x) (lda_error(mve.2f(trait = trait[x], A = genotypes, B = factorb, rep = rep, design = "crd",data = fb), trait[x])))
  #       e_trait <- temp_colum %>% map_chr("trait")
  #       e_error <- temp_colum %>% map_chr("error")
  #       out <- list( e_trait =  e_trait, e_error = e_error)
  #     }
  #     
  #     if(design == 'Split Plot with Plots in RCBD (SPRCBD)'){
  #       
  #       temp_colum <- lapply(X = 1:n, function(x) (lda_error(mve.2f(trait = trait[x], A = genotypes, B = factorb, rep = rep, design = "rcbd",data = fb), trait[x])))
  #       e_trait <- temp_colum %>% map_chr("trait")
  #       e_error <- temp_colum %>% map_chr("error")
  #       out <- list( e_trait =  e_trait, e_error = e_error)
  #     }
  #     
  #     if(design == 'Alpha Design(0,1) (AD)'){
  #       
  #       #temp_colum <- lapply(X = 1:n, function(x) (lda_error(mve.2f(trait = trait, A = genotypes, B = factorb, rep = rep, design = "crd",data = fb))))
  #       # e_trait <- temp_colum %>% map_chr("trait")
  #       # e_error <- temp_colum %>% map_chr("error")
  #       # out <- list( e_trait =  e_trait, e_error = e_error)
  #       out <- NULL
  #     }
  #     
  #     if(design == 'Augmented Block Design (ABD)'){
  #       
  #       #temp_colum <- lapply(X = 1:n, function(x) (lda_error(mve.2f(trait = trait, A = genotypes, B = factorb, rep = rep, design = "crd",data = fb))))
  #       # e_trait <- temp_colum %>% map_chr("trait")
  #       # e_error <- temp_colum %>% map_chr("error")
  #       # out <- list( e_trait =  e_trait, e_error = e_error)
  #       out <- NULL
  #     }
  #     
  #     
  #     # e_trait <- temp_colum %>% map_chr("trait")
  #     # e_error <- temp_colum %>% map_chr("error")
  #     # 
  #     # #out <- paste("Trait Status of ", e_trait, ": ", e_error, sep="")
  #     # out <- list( e_trait =  e_trait, e_error = e_error)
  #     #out <- paste(out, sep= "\n")
  #     #out <-   paste("hello", "world", sep="\n")
  #     
  #   #} else {
  #     
  #     out <- out
  #   #}
  #   }
  #   #out
  #   out
  # })
  # 
  # 
  # output$lda_anova_fail_message = renderRHandsontable({
  #   
  #   if(!is.null(hot_check_lda_fb())) {
  #      msg <-  hot_check_lda_fb() 
  #     # #HTML(paste(msg, sep = '<br/>'))
  #     # out <- paste("Trait Status of ", msg$e_trait, ": ", msg$e_error, sep="")
  #     # #msg <- paste(msg)
  #     # msg <- HTML(paste(out , sep = '<br/>'))
  #     df <- data.frame(trait = msg$e_trait, status = msg$e_error)
  #     rhandsontable(df)
  #      #msg <- paste(msg)
  #   } else {
  #     df <- data.frame()
  #     rhandsontable(df)
  #   }
  #   
  # })
  # 
  # 
  # 
  
  #output$lda_anova_fail_message <- shiny::renderText({
  
  #   output$lda_anova_fail_message <-renderUI({
  #     if(!is.null(hot_check_lda_fb())) {
  #       
  #         msg <-  hot_check_lda_fb() 
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

