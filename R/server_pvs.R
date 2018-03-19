#' Server for PVS report for selection criteria and organoleptics
#' Returns user friendly ui for selection criteria and organoleptics
#' @param input shinyserver input
#' @param output shinyserver output
#' @param session shinyserver session
#' @param values reactive values
#' @importFrom shiny reactive tabPanel renderUI selectInput req icon h2 uiOutput radioButtons actionButton br column fluidRow
#' @importFrom shinydashboard infoBox tabBox infoBoxOutput renderInfoBox
#' @importFrom shinyFiles parseFilePaths
#' @import pepa
#' @import st4gi
#' @author Omar Benites
#' @export

pvs_server <- function(input, output, session, values){


  volumes <- shinyFiles::getVolumes()
  shinyFiles::shinyFileChoose(input, 'file_pvs', roots=volumes, session=session,
                              restrictions=system.file(package='base'),filetypes=c('xlsx'))

  hot_path <- reactive ({

    if(length(input$file_pvs)==0){return (NULL)}
    if(length(input$file_pvs)>0){
      hot_file <- as.character(parseFilePaths(volumes, input$file_pvs)$datapath)
    }
  })

  hot_sheet <- reactive ({
    
   
    
    file_path <- hot_path()
    if(length(file_path)==0){return (NULL)}
    if(length(file_path)>0){
      hot_sheet <- readxl::excel_sheets(path = file_path)
    }
  })

  output$sheet_pvs  <- renderUI({
    req(input$file_pvs)
     
    sheets <- hot_sheet()
    pvs_need_sheet <- c("F1_selection_criteria", "F2_select_clones_flowering", "F3_select_clones_harvest",
                        "F9_postharvest_clones_storage" , "summary_organoleptic_mother",
                        "summary_organoleptic_baby")

    sheets <-  sort(sheets[is.element(sheets, pvs_need_sheet)])

    #pvs_need_sheet <- c("F4_harvest_mother", "F5_harvest_baby", "F8_postharvest_dormancy")

    selectInput('pvs_sheet', 'Select Sheet', c(Choose='', sheets), selectize = TRUE, multiple = TRUE)
  })

  hot_bdata <- reactive({

    hot_file <- hot_path()
    if(length(hot_file)==0){return (NULL)}

    if(length(hot_file)>0){
      fb_sheets <- readxl::excel_sheets(hot_file)
      sheet_list <- lapply(X=fb_sheets, function(x) openxlsx::read.xlsx(xlsxFile =  hot_file, sheet = x, na.strings = TRUE ))
      names(sheet_list) <- fb_sheets
      hot_bdata <- sheet_list
    }

  })

  hot_check_pvs_form <- reactive({
    
    fp <- hot_path
    format <- paste(input$format_pvs)
    pvs_need_sheet <- c("F1_selection_criteria", "F2_select_clones_flowering", "F3_select_clones_harvest",
                        "F4_harvest_mother",      "F5_harvest_baby", "F6_organoleptic_mother", "F7_organoleptic_baby",
                        "F8_postharvest_dormancy", "F9_postharvest_clones_storage" , "summary_organoleptic_mother",
                        "summary_organoleptic_baby")
    
    pvs_hot_sheet <- input$pvs_sheet
    pvs_found_sheet <-  pvs_hot_sheet[is.element(pvs_hot_sheet, pvs_need_sheet)]
    hot_pvs_bdata <- hot_bdata()
    
    res <-  lapply(X = pvs_found_sheet, function(x)  pvs::check_pvs_form(x, hot_pvs_bdata[[x]]) )
    names(res) <- pvs_found_sheet
    res
    
  })
  
  hot_check_fail_sheet <- reactive({
    
    res <- hot_check_pvs_form()
    out_fail <- rlist::list.filter(res, out == FALSE)
    out_fail_fnumber <- rlist::list.map(out_fail, mensaje)
    out_fail_fnumber <- unlist(out_fail_fnumber)
     
  })
  
  output$pvs_fail_message <- shiny::renderText({
  #output$pvs_fail_message <- shiny::renderUI({
    
    if(!is.null(hot_check_fail_sheet())) {
    res <- hot_check_fail_sheet()
    #print(res)
    out <- paste(names(res),": ", res, collapse = ";    ")  
    #out <- print(res)
    } else {
      out <- paste("", sep = "\n")
    }
    
  })
  
  output$file_message_pvs <- renderInfoBox({

    #germoplasm <-material_table()$Institutional_number
    #germoplasm <-germoplasm_list()$institutional_number
    #print( germoplasm)

    hot_file <- hot_path()
    sheets <- hot_sheet()
     # print(hot_file)
     # print(sheets)
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

  shiny::observeEvent(input$pvs_button, {
    shiny::withProgress(message = "Opening pvs Report...",value= 0,{

      #NOTE: To use pepa report package we need R 3.3.0 or more.
      #NOTE Finally, we always need pandoc installer.

      fp <- hot_path

      format <- paste(input$format_pvs)

      pvs_need_sheet <- c("F1_selection_criteria", "F2_select_clones_flowering", "F3_select_clones_harvest",
                          "F4_harvest_mother",      "F5_harvest_baby", "F6_organoleptic_mother", "F7_organoleptic_baby",
                          "F8_postharvest_dormancy", "F9_postharvest_clones_storage" , "summary_organoleptic_mother",
                          "summary_organoleptic_baby")

      pvs_hot_sheet <- input$pvs_sheet

      pvs_found_sheet <-  pvs_hot_sheet[is.element(pvs_hot_sheet, pvs_need_sheet)]


      hot_pvs_bdata <- hot_bdata()

      print("f1")
      res <-  lapply(X = pvs_found_sheet, function(x)  pvs::check_pvs_form(x, hot_pvs_bdata[[x]]) )
      names(res) <- pvs_found_sheet

      print("f2")
      out_pass <- rlist::list.filter(res, out == TRUE)
      out_pass_fnumber <- rlist::list.map(out_pass, form_number)
      #out_pass_fnumber <- unlist(out_pass_fnumber, use.names = FALSE)
      out_pass_fnumber <-  unlist(out_pass_fnumber)

      print("out pass")
      #print(out_pass)
      #print(out_pass_fnumber)
      #
      # print(sheets)
      #
      # out_fail <- rlist::list.filter(res, out == FALSE)
      # out_fail_fnumber <- rlist::list.map(out_fail, form_number)
      # out_fail_fnumber <- unlist(out_fail_fnumber)
      #out_fail_fnumber <-  unlist(out_fail_fnumber,use.names = FALSE)

      sheets <- names(out_pass)
      #lapply(X = sheets, function(x) repo.pvs(data = sheets, form = form_number, format = format))
      print("f4")
      # res <- check_pvs_form(i, hot_pvs_bdata[[i]]
      # } #end for loop
      # print(hot_pvs_bdata[[sheets]])
      # print(out_pass_fnumber[[sheets]])

      for(i in sheets) {
        try(pepa::repo.pvs(data =  hot_pvs_bdata[[i]], form = out_pass_fnumber[[i]], format =  format))
      }
      # print("f5")
      # shinysky::showshinyalert(session, "alert_fb_done", paste("WARNING: This fieldbook already exists in HiDAP. Please Select Experiment Number in Crop & Location"),
      #                          styleclass = "warning")
    })
  })

}


