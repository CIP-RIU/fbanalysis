#' UI for PVS report for selection criteria and organoleptics
#' Returns user friendly ui for selection criteria and organoleptics
#' @author Omar Benites
#' @param type type of UI element, deault is a tab in a shinydashboard
#' @param title diaply title name
#' @param name UI TabName
#' @export
#'

ui_pvs <- function(type = "tab", title = "PVS Criteria & Organoleptic", name = "analysis_pvs"){
  ###
  shinydashboard::tabItem(tabName = name,
                         

                          box(
                            title = " ", status = "primary", solidHeader = TRUE,
                            collapsible = TRUE, width = NULL,
                            #tabsetPanel(
                            tabBox(width = 12,
                                   tabPanel("PVS Form Analysis", #begin tabset "CHECK"
                                            h2(title),
                                            fluidRow(
                                              
                                              column(width = 6,
                                                     shinyFiles::shinyFilesButton('file_pvs', 'Select File', 'Please select a file',FALSE),
                                                     infoBoxOutput("file_message_pvs", width = NULL),

                                                     uiOutput("sheet_pvs"),
                                                     radioButtons(inputId = "format_pvs", label="Report format", choices= c("html","word"),
                                                                  selected = "html", inline = TRUE, width = NULL),
                                                     actionButton(inputId = "pvs_button", label= "Analyze", icon = icon("play-circle"),
                                                                  width = NULL,height = NULL),
                                                     br(),
                                                     shiny::wellPanel(
                                                       shiny::HTML("<b> Form Note </b>"),
                                                       shiny::textOutput("pvs_fail_message")
                                                     )
  
                                              )#,#end column
             
                                            )#, end fluidow

                                   ), #end tab Panel "pvs report"
                              
                                   pvs_anova_ui(type="tab", title="PVS Fieldbook Analysis", name="anova_pvs")
                                
                            )#,
                            
                          ),

  br(),
  br(),
  br()

  )#End data_processing tabItem
  ##
    

}