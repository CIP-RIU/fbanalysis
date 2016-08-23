#' UI for Single Environment Analysis in HiDAP
#' Returns user friendly ui
#' @author Omar Benites
#' @param type type of UI element, deault is a tab in a shinydashboard
#' @param title diaply title name
#' @param name UI TabName
#' @export
#' 

single_ui <- function(type = "tab", title = "The Statistical Analysis of Single-Environment Trial", name = "analysis_single"){
  
  shinydashboard::tabItem(tabName = name,
                          h2(title),   
                          
                          box(
                            title = " ", status = "primary", solidHeader = TRUE,
                            collapsible = TRUE, width = NULL,
                            #tabsetPanel(
                            tabBox(width = 12,
                                   tabPanel("Single Analysis", #begin tabset "CHECK"
                                            fluidRow( 
                                              column(width = 6,
                                              shinyFiles::shinyFilesButton('file_single', 'Select File', 'Please select a file',FALSE),
                                              infoBoxOutput("file_message_single",width = NULL),
                                              #selectInput('trait_single', 'Select Trait', c(Choose='', single_select_options()   ), selectize=TRUE),
                                              #selectInput('rep_single', 'Select Repetition', c(Choose='', names(iris)), selectize=TRUE),
                                              #selectInput('genotypes_single', 'Select Treatment', c(Choose='', names(iris)), selectize=TRUE),
                                              selectInput(inputId = 'design_single', label = 'Select statistical design of your experiment', choices = 
                                                                           c("Complete Randomized Design (CRD)",
                                                                          "Randomized Complete Block Design (RCBD)",
                                                                          "Augmented Block Design (ABD)"),selected = "Randomized Complete Block Design (RCBD)",
                                                                           selectize=FALSE),
                                              uiOutput("genotypes_single"),
                                              uiOutput("rep_single"),
                                              uiOutput("trait_single"),
                                              radioButtons(inputId="format_single", label="Report format", choices= c("html","word"), 
                                                           selected = "html", inline = TRUE, width = NULL),
                                              actionButton(inputId = "single_button", label= "Analyze", icon = icon("play-circle"),
                                                            width = NULL,height = NULL)
                                              
                                              
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