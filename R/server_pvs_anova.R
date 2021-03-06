#' Server for PVS' analysis of variance
#'
#' @param input shinyserver input
#' @param output shinyserver output
#' @param session shinyserver session
#' @param values reactive values
#' @importFrom shiny reactive tabPanel withProgress renderUI HTML selectInput icon h2 uiOutput radioButtons actionButton br column fluidRow 
#' @importFrom shinydashboard infoBox tabBox infoBoxOutput renderInfoBox
#' @importFrom shinyFiles parseFilePaths
#' @import pepa
#' @import st4gi
#' @author Omar Benites
#' @export

pvs_anova_server <- function(input, output, session, values){


  volumes <- shinyFiles::getVolumes()
  shinyFiles::shinyFileChoose(input, 'file_pvs_anova', roots=volumes, session=session,
                              restrictions = system.file(package='base'),filetypes=c('xlsx'))

  hot_path <- reactive ({

    if(length(input$file_pvs_anova)==0){return (NULL)}
    if(length(input$file_pvs_anova)>0){
      hot_file <- as.character(parseFilePaths(volumes, input$file_pvs_anova)$datapath)
    }
  })

  hot_sheet <- reactive ({
    file_path <- hot_path()
    if(length(file_path)==0){return (NULL)}
    if(length(file_path)>0){
      hot_sheet <- readxl::excel_sheets(path = file_path)
    }
  })

  output$sheet_pvs_anova  <- renderUI({

    req(input$file_pvs_anova)
    
    sheets <- hot_sheet()
    pvs_need_sheet <- c( "F4_harvest_mother" , "F5_harvest_baby", "F8_postharvest_dormancy", "summary_global")

    sheets <-  sort(sheets[is.element(sheets, pvs_need_sheet)])

    #pvs_need_sheet <- c("F4_harvest_mother", "F5_harvest_baby", "F8_postharvest_dormancy")

    shiny::selectInput('pvs_anova_sheet', 'Select Sheet', c(Choose='', sheets), selectize = TRUE, multiple = FALSE)
  })

  hot_bdata <- reactive({

    req(input$pvs_anova_sheet)
    hot_file <- hot_path()
    if(length(hot_file)==0){return (NULL)}

    if(length(hot_file)>0){
      
      sheet <- input$pvs_anova_sheet
      # fb_sheets <- readxl::excel_sheets(path = hot_file )
      # sheet_list <- lapply(X=fb_sheets, function(x) openxlsx::read.xlsx(xlsxFile =  hot_file, sheet = x, na.strings = TRUE ))
      # names(sheet_list) <- fb_sheets
      # hot_bdata <- sheet_list
        
      # Read the excel using the selected sheet.  
      hot_bdata <- openxlsx::read.xlsx( xlsxFile =  hot_file, sheet = sheet, na.strings = TRUE )
      if(sheet =="summary_global"){hot_bdata <- select(hot_bdata, matches('INSTN|Mean'))} 
      hot_bdata 
      

    }

    
  })

  output$pvs_anova_genotypes  <- renderUI({
    selectInput('genotypes_pvs_anova', 'Select Genotypes', c(Choose='', select_options(hot_bdata())),
                selectize=TRUE)
  })

  
  output$pvs_anova_rep  <- renderUI({
    selectInput('rep_pvs_anova', 'Select Replications', c(Choose='', select_options(hot_bdata())),
                selectize=TRUE)
  })

  
  output$pvs_anova_trait <- renderUI({
    selectInput('trait_pvs_anova', 'Select Trait(s)', c(Choose='', select_options(hot_bdata())),
                selectize=TRUE, multiple = TRUE)
  })
  
  
  output$pvs_message_anova <- renderInfoBox({

    #germoplasm <-material_table()$Institutional_number
    #germoplasm <-germoplasm_list()$institutional_number
    #print( germoplasm)

    hot_file <- hot_path()
    sheets <- hot_sheet()

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
  
  
  # hot_check_pvs_fb <- reactive({
  #   
  #   fp <- hot_path()
  #   pvs_hot_sheet <- input$sheet_pvs_anova
  #   pvs_need_sheet <- c("F4_harvest_mother", "F5_harvest_baby", "F8_postharvest_dormancy")
  #   pvs_found_sheet <-  pvs_hot_sheet[is.element(pvs_hot_sheet, pvs_need_sheet)]
  #   
  #   fieldbook <- as.data.frame(hot_bdata())
  #   res <- pvs::check_pvs_form(pvs_found_sheet, fieldbook) 
  #   names(res) <- pvs_found_sheet
  #   res
  #   
  # })
  
  
  hot_check_fail_fb <- reactive({

    pvs_hot_sheet <- input$pvs_anova_sheet

    fieldbook <- as.data.frame(hot_bdata())
    
    check_pvs_aov <- pvs::check_pvs_data(fieldbook)
    flag <- check_pvs_aov$flag
    mensaje <- check_pvs_aov$mensaje
    
    if(!flag){
      out <- paste( pvs_hot_sheet, mensaje, sep = ": ")
    } else {
      out <- ""
    }
    
    out
    
  })
  
  
  output$pvs_anova_fail_message <- shiny::renderText({
    #output$pvs_fail_message <- shiny::renderUI({

    if(!is.null(hot_check_fail_fb())) {
      res <- hot_check_fail_fb()
      # print(res)
      out <- res
    } else {
      out <- paste("")
    }

  })
  
  
  shiny::observeEvent(input$button_pvs_anova, {
    shiny::withProgress(message = "Opening PVS anova report...",value= 0,{

      fp <- hot_path()

      pvs_hot_sheet <- input$pvs_anova_sheet
      pvs_need_sheet <- c("F4_harvest_mother", "F5_harvest_baby", "F8_postharvest_dormancy", "summary_global")
      pvs_found_sheet <-  pvs_hot_sheet[is.element(pvs_hot_sheet, pvs_need_sheet)]

      fieldbook <- as.data.frame(hot_bdata())
     
      
      
      genotypes <- input$genotypes_pvs_anova
      rep   <- input$rep_pvs_anova
      trait <- input$trait_pvs_anova
      format <- paste(input$format_pvs_anova)
      
      check_pvs_aov <- pvs::check_pvs_data(fieldbook)
      flag <- check_pvs_aov$flag
      print(flag)
      
      if(flag){

       if(pvs_hot_sheet!="summary_global"){
         try(pepa::repo.rcbd(traits = trait, geno = genotypes, rep = rep, format = format, data = fieldbook))
       }
       else {
          try(pepa::repo.pvssg(traits = trait, data =  fieldbook, format = format))
       }
        
      } 

    })
  })

}


