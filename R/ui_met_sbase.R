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

met_sbase_ui <- function(type = "tab", title = "The Statistical Analysis of Multi-Environment Trials (MET)", name = "analysis_met"){
  
  shinydashboard::tabItem(tabName = name,
                          h2(title),   
                          
                          box(
                            title = " ", status = "primary", solidHeader = TRUE,
                            collapsible = TRUE, width = NULL,
                            #tabsetPanel(
                            tabBox(width = 12,
                                   tabPanel("MET Analysis", #begin tabset "CHECK"
                                            
                                            fluidRow( 
                                              
                                              column(width = 12,
                                                     
                                                     box(
                                                       #"Ingrese una lista de familias o clones", width = 4, status = "primary", height = "730px",
                                                       title = "SweetPotato Base Trial", width = NULL,  height = NULL, status = "primary", solidHeader = TRUE, collapsible = TRUE,
                                                       
                                                       #actionButton(inputId= "connect_met_sbase", "Connect to Sweetpotato Base"),
                                                       infoBoxOutput("file_message_met_sbase", width = NULL),
                                                       br(),
                                                       uiOutput("programName_met_sbase"),
                                                       uiOutput("trialName_met_sbase"),
                                                       uiOutput("studyName_met_sbase"),
                                                       shinysky::shinyalert("alert_met_sbase_done", FALSE, auto.close.after = 8),
                                                       br(),
                                                       br(),
                                                       # br(),
                                                       
                                                       br()
                                                     ),
                                                     
                                                     conditionalPanel( condition = "output.show_met_sbase_params",  ##conditional Panel for Inputs 
                                                                       
                                                                       box(
                                                                         #column(width = 6,
                                                                         title = "SweetPotato Base Trial", width = 12,  status = "primary", solidHeader = FALSE, collapsible = TRUE,
                                                                         #shiny::selectInput(inputId = "met_met_list_sbase", label = "Select Files from SweetPotato Base", choices = c("omar","benites")),
                                                                         conditionalPanel( condition = "output.show_met_sbase_len",  
                                                                                           
                                                                                           uiOutput("genotypes_met_sbase"),
                                                                                           uiOutput("env_met_sbase"),
                                                                                           uiOutput("rep_met_sbase"),
                                                                                           uiOutput("trait_met_sbase"),
                                                                                           
                                                                                           radioButtons(inputId="format_met_sbase", label="Report format", choices= c("html","word"), 
                                                                                                        selected = "html",  width = NULL),
                                                                                           
                                                                                           shiny::conditionalPanel(condition = "input.format_met_sbase == 'word'",
                                                                                                                   
                                                                                                                   downloadButton(outputId = "downloadSbase_met_report", label = "Download"),
                                                                                                                   tags$head(tags$style(".butt1{background-color:orange;} .butt1{color: black;} .butt1{font-family: Courier New}")) 
                                                                                           ),
                                                                                           
                                                                                           shiny::conditionalPanel(condition = "input.format_met_sbase == 'html'",
                                                                                                                   
                                                                                                                   actionButton(inputId = "met_sbase_button", label= "Analyze", icon = icon("play-circle"),
                                                                                                                                width = NULL,height = NULL)
                                                                                                                   
                                                                                           )
                                                                         )
                                                                       )
                                                                       
                                                                       
                                                                       
                                                     )#end conditional panel show design parameters
                                                     
                                              ) #end column 
                                              
                                            )#, end fluidow
                                   )#,#end tab Panel "CHECK"
                                   
                            )
                          ),
                          br(),
                          br(),
                          br()
                          
                          
  )#End data_processing tabItem
  
}