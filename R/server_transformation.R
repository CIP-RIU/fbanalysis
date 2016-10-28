dtr_choices <- c(
  "Logarithmic transformation log(y)"="logy",
  "Logarithmic transformation log(y + 1)"="logy1",
  "Square root transformation sqrt(y)"="sqrty",
  "Square root transformation sqrt(y+1)"="sqrty1",
  "Arc-sine transformation arcsin"="arcsin"
)



#' Server for Data Transformation
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

dtr_server <- function(input, output, session, values){
  
  volumes <- shinyFiles::getVolumes()
  shinyFiles::shinyFileChoose(input, 'file_dtr', roots=volumes, session=session,
                              restrictions=system.file(package='base'),filetypes=c('xlsx'))
  
  hot_path <- reactive ({
    
    #validate(
    #  need(input$file != "", label = "Please enter an XLSX file. XLS files are forbidden")
    #)
    
    if(length(input$file_dtr)==0){return (NULL)}
    if(length(input$file_dtr)>0){
      hot_file <- as.character(parseFilePaths(volumes, input$file_dtr)$datapath)
    }
  })
  
  hot_bdata <- reactive({
    hot_file <- hot_path()
    if(length(hot_file)==0){return (NULL)}
    if(length(hot_file)>0){
      hot_bdata <- readxl::read_excel(path=hot_file , sheet = "Fieldbook")
    }
  })
  
  output$file_message_dtr <- renderInfoBox({
    
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

  output$trait_dtr <- renderUI({
    selectInput('trait_dtr', 'Select Trait(s)', c(Choose='', select_options(hot_bdata())),
                selectize=TRUE, multiple = TRUE)
  })    
  
  output$tdr_parameter <- renderUI({
  
    trait <- as.character(input$trait_dtr)
    type <- input$type_dtr
   # res <- vector()
    
    if(length(trait)>0){
      
      lapply(1:length(trait), function(i) {
        print(i)
        
        # if(type =='Logarithmic transformation log(y)' || type == 'Logarithmic transformation log(y + 1)'){
        #   label <- "Base for Logarithmic transformation "
        #   value <- 10
        # }
        # 
        # if(type =='Arc-sine transformation arcsin'){
        #   label <- "Parameter for arc-sine transformation "
        #   value <- 1
        # }
        tagList(
          
        selectInput(paste0('type_dtr_',trait[i]), label = 'Select type of data tranformation', 
                    choices = dtr_choices,
                    selected = "logy",
                    selectize = FALSE),
        
          # 
          # shiny::conditionalPanel(
          #   condition =
          #   "input.type_dtr_ == 'logy'|
          #    input.type_dtr_ == 'logy1'",
          #   numericInput(paste0("n_input_tdr1_", trait[i]), label = paste0("The parameter for ", trait[i]), value = 10)
          #   #uiOutput("trait_single")
          # 
          # ),
          # 
          # shiny::conditionalPanel(
          #   condition =
          #     "input.type_dtr_ == 'arcsin'",
          #   numericInput(paste0("n_input_tdr2_", trait[i]), label = paste0("The parameter for ", trait[i]), value = 10)
          #   #uiOutput("trait_single")
          # 
          # )#,
        
  
        # res <- input[[paste0("type_dtr_", trait[i])]],
        # print("-------------"),
        # print(res),
        # print("-------------"),
        # type <- input$type_dtr
        # 
        # if(res[i] == 'Logarithmic transformation log(y)' || res[i] == 'Logarithmic transformation log(y + 1)'){
        #   label <- "Base for Logarithmic transformation "
        #   value <- 10
        # }
        # 
        # if(res[i] ==  'Arc-sine transformation arcsin'){
        #   label <- "Parameter for arc-sine transformation "
        #   value <- 1
        # }
        
        numericInput(paste0("n_input_tdr_", trait[i]), label = paste0("The parameter for ", trait[i]), value = 10)
        )
      })
      
    }
  })
  
  fbdraft_dtr <- shiny::reactive({    
    
    fieldbook <- as.data.frame(hot_bdata())
    
    trait <- input$trait_dtr
    tdr_table <- data.frame(lapply(1:length(trait), function(i) {
      input[[paste0('type_dtr_',trait[i])]]
    }), stringsAsFactors = FALSE)
    
    tdr_par <- data.frame(lapply(1:length(trait), function(i) {
      input[[paste0('n_input_tdr_',trait[i])]]
    }), stringsAsFactors = FALSE)
    
    names(tdr_par) <- trait
    names(tdr_table) <- trait

    tdr_global <- rbind(tdr_table, tdr_par)
    #fieldbook2 <- data.frame()
    #print(tdr_global)
    
    if(!is.null(trait)){
      for(j in 1:length(trait)) {
      print(j)
      print(tdr_global[1,j])
      
      
      if(tdr_global[1,j]=='logy' || tdr_global[1,j]=='logy1'){
        fieldbook <- st4gi::dtr(trait = trait[j], type = tdr_global[1,j],  base = as.numeric(tdr_global[2,j]), data = fieldbook)
      }
      
      if(tdr_global[1,j]=='sqrty' || tdr_global[1,j]=='sqrty1'){
        fieldbook <- st4gi::dtr(trait = trait[j], type = tdr_global[1,j],  data = fieldbook) #sqrty & sqrty1
      }
      
     if(tdr_global[1,j]=='arcsin') {
        fieldbook <- st4gi::dtr(trait = trait[j], type = tdr_global[1,j] , n = as.numeric(tdr_global[2,j]), data = fieldbook)
      }
      
      fieldbook2 <- fieldbook
   
    }
    }
    if(is.null(trait)){ fieldbook2 <- fieldbook }
    
    
    fieldbook2
    #format <- paste(input$format_dtr)
    
    # if( type_transformation == "logy"){
    #
    #   try(fieldbook <- st4gi::dtr(trait = trait, type = "logy",base = par_logy_tdr, data = fieldbook))
    # }
    #
    # if( type_transformation == "logy1"){
    #   #try(pepa::repo.abd(traits = trait, geno = genotypes, rep = rep, format = format, data = fieldbook))
    #   try(fieldbook <- st4gi::dtr(trait = trait, type = "logy1",base = par_logy_tdr, data = fieldbook))
    # }
    #
    # if( type_transformation == "sqrty"){
    #
    #   try(fieldbook <- st4gi::dtr(trait = trait, type = "sqrty",  data = fieldbook))
    # }
    #
    # if( type_transformation == "sqrty1"){
    #
    #   try(fieldbook <- st4gi::dtr(trait = trait, type = "sqrty1", data = fieldbook))
    # }
    #
    # if( type_transformation == "arcsin"){
    #
    #   try(fieldbook <- st4gi::dtr(trait = trait, type = "arcsin" , n = arcsin_tdr, data = fieldbook))
    # }

   
  })
  
  shiny::observeEvent(input$dtr_button, {
        fb_dtr <-  fbdraft_dtr()
        fbtemp <- fb_dtr
        #print(fb)
        output$fbDesign_table_dtr <- rhandsontable::renderRHandsontable({
          rhandsontable::rhandsontable(fb_dtr, readOnly = T)})
        
      })
      
  shiny::observeEvent(input$exportButton_dtr,{
        
        #For single Fieldbooks
        withProgress(message = "Downloading Fieldbook and Applying Format...",value= 0,
                     {
                        
                       DF <- fbdraft_dtr()
                       print("detection of sheets")
                       hot_file <- hot_path() 
                       try(wb <- openxlsx::loadWorkbook(hot_file))
                       sheets <- readxl::excel_sheets(path = hot_file)
                       
                       print("after loadworkbook")
                       
                       if(is.element("Fieldbook",sheets)){    
                         try( openxlsx::removeWorksheet(wb, "Fieldbook") )
                       }
                       
                       try(openxlsx::addWorksheet(wb = wb,sheetName = "Fieldbook",gridLines = TRUE))
                       try(openxlsx::writeDataTable(wb,sheet = "Fieldbook", x = DF,colNames = TRUE, withFilter = FALSE))
                       try(openxlsx::saveWorkbook(wb = wb, file = hot_file, overwrite = TRUE) )
                       print("ejecucion")
                       try(shell.exec(hot_file))
      
    })
  })
  
}  


