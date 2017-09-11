#' UI for Multi Environment Analysis (MET) in HiDAP
#' Returns user friendly ui
#' @author Omar Benites
#' @param type type of UI element, deault is a tab in a shinydashboard
#' @param title diaply title name
#' @param name UI TabName
#' @importFrom shiny reactive tabPanel renderUI selectInput icon h2 uiOutput radioButtons actionButton br column fluidRow
#' @importFrom shinydashboard infoBox tabBox infoBoxOutput renderInfoBox
#' @importFrom shinyFiles parseFilePaths
#' @export
#' 

met_ui_sbase <- function(type = "tab", title = "The Statistical Analysis of Multi-Environment Trials (MET)", name = "analysis_met"){
  
  shinydashboard::tabItem(tabName = name,
                          h2(title),   
                          
                          box(
                            title = " ", status = "primary", solidHeader = TRUE,
                            collapsible = TRUE, width = NULL,
                            #tabsetPanel(
                            tabBox(width = 12,
                                   tabPanel("MET Analysis", #begin tabset "CHECK"
                                            fluidRow( 
                                              column(width = 6,
                                                     shinyFiles::shinyFilesButton('file_met_sbase', 'Select Files', 'Select your files',multiple = TRUE),
                                                     infoBoxOutput("file_message_met_sbase",width = NULL),
                                                     #selectInput('trait_met', 'Select Trait', c(Choose='', met_select_options()   ), selectize=TRUE),
                                                     #selectInput('rep_met', 'Select Repetition', c(Choose='', names(iris)), selectize=TRUE),
                                                     #selectInput('genotypes_met', 'Select Treatment', c(Choose='', names(iris)), selectize=TRUE),
                                                     uiOutput("genotypes_met_sbase"),
                                                     uiOutput("env_met_sbase"),
                                                     uiOutput("rep_met_sbase"),
                                                     uiOutput("trait_met_sbase"),
                                                     radioButtons(inputId="format_met_sbase", label="Report format", choices= c("html","word"), 
                                                                  selected = "html", inline = TRUE, width = NULL),
                                                     actionButton(inputId = "met__sbase_button", label= "Analyze", icon = icon("play-circle"),
                                                                  width = NULL,height = NULL)
                                                     #uiOutput("run_met")
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