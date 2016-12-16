#' UI for Data Transformation in HiDAP
#' Returns user friendly ui
#' @author Omar Benites
#' @param type type of UI element, deault is a tab in a shinydashboard
#' @param title display title name
#' @param name UI TabName
#' @export
#' 

dtr_ui <- function(type = "tab", title = "Data Transformation", name = "analysis_dtr"){
  
  shinydashboard::tabItem(tabName = name,
                          h2(title),   
                          
                          box(
                            title = " ", status = "primary", solidHeader = TRUE,
                            collapsible = TRUE, width = NULL,
                            #tabsetPanel(
                            tabBox(width = 12,
                                   tabPanel("Data Transformation", #begin tabset "CHECK"
                                            fluidRow( 
                                              column(width = 8,
                                                     shinyFiles::shinyFilesButton('file_dtr', 'Select File', 'Please select a file',FALSE),
                                                     infoBoxOutput("file_message_dtr", width = NULL),
                                                     #selectInput('trait_single', 'Select Trait', c(Choose='', single_select_options()   ), selectize=TRUE),
                                                     #selectInput('rep_single', 'Select Repetition', c(Choose='', names(iris)), selectize=TRUE),
                                                     #selectInput('genotypes_single', 'Select Treatment', c(Choose='', names(iris)), selectize=TRUE),
                                                     
                                                     uiOutput("trait_dtr"),
                                                     
                                                     # selectInput(inputId = 'type_dtr', label = 'Select type of data tranformation', choices = 
                                                     #               c(Choose='', "Logarithmic transformation log(y)",
                                                     #                 "Logarithmic transformation log(y + 1)",
                                                     #                 "Square root transformation sqrt(y)",
                                                     #                 "Square root transformation sqrt(y+1)",
                                                     #                 "Arc-sine transformation arcsin"),
                                                     #             selected = "Logarithmic transformation log(y)",
                                                     #             selectize=FALSE),
                                                    
                                                     # shiny::conditionalPanel(
                                                     #   condition =
                                                     #   "input.type_dtr == 'Logarithmic transformation log(y)'|
                                                     #    input.type_dtr == 'Logarithmic transformation log(y + 1)'",
                                                     #   shiny::numericInput(inputId = "logy_tdr", label = "Base for Logarithmic transformation",
                                                     #                                                      value = 10, min =2, max = 10, step=1)
                                                     #   #uiOutput("trait_single")
                                                     # 
                                                     # ),
                                                     # 
                                                     # shiny::conditionalPanel(
                                                     #   condition =
                                                     #     "input.type_dtr == 'Arc-sine transformation arcsin'",
                                                     #   shiny::numericInput(inputId = "arcsin_tdr", label = "Parameter for arc-sine transformation",
                                                     #                       value = 10, min =2, max = 10, step=1)
                                                     #   #uiOutput("trait_single")
                                                     #   
                                                     # ),
                                           
                                                     uiOutput("tdr_parameter"),
                                                     
                                                     actionButton(inputId = "dtr_button", label= "Transform!", icon = icon("play-circle"),
                                                                  width = NULL,height = NULL),
                                                     
                                                     br(),
                                                     br(),
                                                     
                                                     shiny::fluidRow(
                                                       shinydashboard::box(title = "Fieldbook Preview",
                                                                           status = "primary",
                                                                           #height = 500,
                                                                           #width = NULL,
                                                                           solidHeader = TRUE,
                                                                           width = 12, collapsible = TRUE,
                                                                           rhandsontable::rHandsontableOutput("fbDesign_table_dtr", height = 400)
                                                       ),
                                                       br(),
                                                       br(),
                                                       br()
                                                     ),
                                                     
                                                     HTML('<div style="float: right; margin: 0 5px 5px 10px;">'),
                                                     shiny::actionLink('exportButton_dtr', 'Download data'),
                                                     HTML('</div>')
                                                     
                                                     
                                                     #uiOutput("run_single")
                                              )#end column
                                              )#, end fluidow
                                            
                                            )#,#end tab Panel "CHECK"
                                   
                                   
                                   )
                          ),
                          
                          br(),
                          br(),
                          br()
                          
                          
                          )#End data_processing tabItem
  
}