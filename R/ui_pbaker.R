#' UI for pbaker Analysis in HiDAP
#' Returns user friendly ui
#' @author Omar Benites
#' @param type type of UI element, deault is a tab in a shinydashboard
#' @param title diaply title name
#' @param name UI TabName
#' @export
#' 

pbaker_ui <- function(type = "tab", title = "Pesek-Baker Selection Index", name = "analysis_pbaker"){
  
  shinydashboard::tabItem(tabName = name,
                          h2(title),   
                          
                          box(
                            title = " ", status = "primary", solidHeader = TRUE,
                            collapsible = TRUE, width = NULL,
                            #tabsetPanel(
                            tabBox(width = 12,
                                   tabPanel("Pesek-Baker Index", #begin tabset "CHECK"
                                            fluidRow( 
                                              column(width = 6,
                                                     shinyFiles::shinyFilesButton('file_pbaker', 'Select File', 'Please select your files',multiple = TRUE),
                                                     infoBoxOutput("file_message_pbaker",width = NULL),
                                                     #selectInput('trait_pbaker', 'Select Trait', c(Choose='', pbaker_select_options()   ), selectize=TRUE),
                                                     #selectInput('rep_pbaker', 'Select Repetition', c(Choose='', names(iris)), selectize=TRUE),
                                                     #selectInput('genotypes_pbaker', 'Select Treatment', c(Choose='', names(iris)), selectize=TRUE),
                                                     uiOutput("genotypes_pbaker"),
                                                     uiOutput("env_pbaker"),
                                                     uiOutput("rep_pbaker"),
                                                     
                                                     selectInput(inputId = 'means_pbaker', label='Select type of means',
                                                                 choices=c("single","fitted"), 
                                                                 selected = "single", selectize = FALSE),
                                                     
                                                     uiOutput("model_pbaker"),
                                                     #fluidRow(
                                                     # column(6, offset = 1, 
                                                     uiOutput("trait_pbaker"),
                                                     #),
                                                     
                                                     #column(6, offset = 1,
                                                     uiOutput("weight_pbaker"),
                                                     #)
                                                     #),
                                                     
                                                     br(),
                                                     
                                                     selectInput(inputId = 'units_pbaker', label='Select type of units',
                                                                 choices=c("actual","sdu"), 
                                                                 selected = "sdu", selectize = FALSE),
                                                     
                                                     
                                                     radioButtons(inputId="format_pbaker", label="Report format", choices= c("html","word"), 
                                                                  selected = "html", inline = TRUE, width = NULL),
                                                     actionButton(inputId = "pbaker_button", label= "Analyze", icon = icon("play-circle"),
                                                                  width = NULL,height = NULL)
                                                     #uiOutput("run_pbaker")
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