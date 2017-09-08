#' UI for Genetic designs
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

genetic_ui <- function(type = "tab", title = "Genetic Analysis", name = "genetic_design_analysis"){
  
  shinydashboard::tabItem(tabName = name,
                          h2(title),   
                          
                          box(
                            title = " ", status = "primary", solidHeader = TRUE,
                            collapsible = TRUE, width = NULL,
                            #tabsetPanel(
                            tabBox(width = 12,
                                   tabPanel("Genetic Analysis", #begin tabset "CHECK"
                                            fluidRow( 
                                              column(width = 8,
                                                     shinyFiles::shinyFilesButton('file_genetic', 'Select File', 'Select a file',FALSE),
                                                     infoBoxOutput("file_message_genetic", width = NULL),
                                                     #selectInput('trait_single', 'Select Trait', c(Choose='', single_select_options()   ), selectize=TRUE),
                                                     #selectInput('rep_single', 'Select Repetition', c(Choose='', names(iris)), selectize=TRUE),
                                                     #selectInput('genotypes_single', 'Select Treatment', c(Choose='', names(iris)), selectize=TRUE),
                                                     selectInput(inputId = 'design_genetic', label = 'Select the genetic design of your experiment', choices = 
                                                                   c("North Carolina Design I (NCI)",
                                                                     "North Carolina Design II (NCII)",
                                                                     "Line by Tester (LxT)"),
                                                                 #"Split Plot with Plots in CRD (SPCRD)",
                                                                 #"Split Plot with Plots in RCBD (SPRCBD)"),
                                                                 selected = "North Carolina Design I (NCI)",
                                                                 selectize=FALSE),
                                             
                                                     shiny::conditionalPanel(                                                               
                                                       condition = "input.design_genetic == 'North Carolina Design I (NCI)'|                      
                                                       input.design_genetic == 'North Carolina Design II (NCII)'|
                                                       input.design_genetic == 'Line by Tester (LxT)'",
                                                       #
                                                       uiOutput("trait_des_genetic"), 
                                                       uiOutput("rep_des_genetic")                                                    
                                                     ), 
                                                     
                                                     
                                                     shiny::conditionalPanel(                                                               
                                                       condition = "input.design_genetic == 'North Carolina Design I (NCI)'|                      
                                                       input.design_genetic == 'North Carolina Design II (NCII)'",
                                                       
                                                       uiOutput("set_des_nc_genetic"),
                                                       uiOutput("male_des_nc_genetic"),
                                                       uiOutput("female_des_nc_genetic")
                                                      
                                                       
                                                     ),  
                                                     
                                                     shiny::conditionalPanel(                                                               
                                                       condition = "input.design_genetic == 'North Carolina Design I (NCI)'",
                                                       uiOutput("progeny_des_nc_genetic")
                                                       
                                                     ),  
                                                     
                                                     
                                                     
                                                     shiny::conditionalPanel(
                                                       condition =  "input.design_genetic == 'Line by Tester (LxT)'",
                                                       uiOutput("line_des_lxt_genetic"),
                                                       uiOutput("tester_des_lxt_genetic"),
                                                       uiOutput("sch_des_lxt_genetic")
                                                     ),
                                                     
                                                     radioButtons(inputId="format_genetic", label="Report format", choices= c("html","word"), 
                                                                  selected = "html", inline = TRUE, width = NULL),
                                                     
                                                     actionButton(inputId = "genetic_button", label= "Analyze", icon = icon("play-circle"),
                                                                  width = NULL,height = NULL),
                                                     
                                                     
                                                     br()#,
                                                    
                                                     
                                                     
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