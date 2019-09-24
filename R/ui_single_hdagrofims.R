#' UI for Single Environment Analysis for HIDAP-AgroFIMS
#' Returns user friendly ui
#' @author Omar Benites
#' @param type type of UI element, deault is a tab in a shinydashboard
#' @param title diaply title name
#' @param name UI TabName
#' @export
#'

single_hdagrofims_ui <- function(type = "tab", title = "Single Fieldbook Analysis", name = "analysis_single"){
  
  
  shinydashboard::tabItem(tabName = name,
                          h2(title),   
                          
                          box(
                            title = " ", status = "primary", solidHeader = FALSE,
                            collapsible = TRUE, width = NULL,
                            #tabsetPanel(
                            tabBox(width = 12,
                                   tabPanel("", #begin tabset "CHECK"
                                            fluidRow( 
                                              column(width = 8,
                                                     useShinyalert(),
                                                     #shinyFiles::shinyFilesButton('file_single_agrofims', 'Select File', 'Select a file',FALSE),
                                                     
                                                     fileInput('file_single_agrofims', 'Choose fieldbook file', accept = c(".zip")),  
                                                               
                                                     infoBoxOutput("file_message_single_agrofims", width = NULL),
                                                     #selectInput('trait_single', 'Select Trait', c(Choose='', single_select_options()   ), selectize=TRUE),
                                                     #selectInput('rep_single', 'Select Repetition', c(Choose='', names(iris)), selectize=TRUE),
                                                     #selectInput('genotypes_single', 'Select Treatment', c(Choose='', names(iris)), selectize=TRUE),
                                                     selectInput(inputId = 'design_single_agrofims', 
                                                                 label = 'Statistical design of your experiment', 
                                                                 choices = 
                                                                   c(
                                                                     "Completely Randomized Design (CRD)",
                                                                     "Randomized Complete Block Design (RCBD)",
                                                                     "Factorial with CRD",
                                                                     "Factorial with RCBD",
                                                                     "Split Plot Design" ,
                                                                     "Split-Split Plot Design"
                                                                     ),
                                                                     #"Split Plot Design"),
                                                                     #"Split Plot with Three Factors",
                                                                     #"Factorial Two-Way Design in CRD (F2CRD)",
                                                                     #"Factorial Two-Way Design in RCBD (F2RCBD)"),
                                                                 #"Split Plot with Plots in CRD (SPCRD)",
                                                                 #"Split Plot with Plots in RCBD (SPRCBD)"),
                                                                 selected = "Randomized Complete Block Design (RCBD)",
                                                                 selectize=FALSE),
                                                     
                                                    
                                                     
                                                     shiny::conditionalPanel(
                                                       condition = "input.design_single_agrofims == 'Factorial with CRD'|
                                                                    input.design_single_agrofims == 'Factorial with RCBD'",
                                                                      
                                                       
                                                       uiOutput("factor_single_agrofims")
                                                     ),
                                                     
                                                     # shiny::conditionalPanel(
                                                     #   condition =  "input.design_single_agrofims == 'Factorial Two-Way Design in CRD (F2CRD)'|
                                                     #   input.design_single_agrofims == 'Factorial Two-Way Design in RCBD (F2RCBD)'",
                                                     #   # input.design_single == 'Split Plot with Plots in CRD (SPCRD)'|
                                                     #   # input.design_single == 'Split Plot with Plots in RCBD (SPRCBD)'|
                                                     #   
                                                     #   uiOutput("factor2_single_agrofims")
                                                     # ),
                                                     
                                                     
                                                     
                                                     
                                                     shiny::conditionalPanel(
                                                       condition = "input.design_single_agrofims == 'Alpha Design(0,1) (AD)'|
                                                       input.design_single_agrofims == 'Completely Randomized Design (CRD)'|
                                                       input.design_single_agrofims == 'Randomized Complete Block Design (RCBD)'|
                                                       input.design_single_agrofims == 'Augmented Block Design (ABD)'",          
                                                       #input.design_single_agrofims == 'Factorial Two-Way Design in CRD (F2CRD)'|
                                                       #input.design_single_agrofims == 'Factorial Two-Way Design in RCBD (F2RCBD)'",
                                                       #input.design_single_agrofims == 'Split Plot with Plots in CRD (SPCRD)'| 
                                                       #input.design_single_agrofims == 'Split Plot with Plots in RCBD (SPRCBD)'|
                                                       uiOutput("trt_single_agrofims")#,                                                    
                                                                                                                  
                                                     ),
                                                     
                                                     # "Factorial Design",
                                                     # "Split Plot with Two Factors",
                                                     # "Split Plot with Three Factors"
                                                     
                                                     
                                                     shiny::conditionalPanel(                                                               
                                                       condition = "input.design_single_agrofims == 'Alpha Design(0,1) (AD)'|                      
                                                       input.design_single_agrofims == 'Randomized Complete Block Design (RCBD)'|
                                                       input.design_single_agrofims == 'Augmented Block Design (ABD)'| 
                                                       input.design_single_agrofims == 'Factorial with RCBD'",
                                                       #input.design_single_agrofims == 'Split Plot Design'",
                                                       #input.design_single == 'Split Plot with Plots in CRD (SPCRD)'|
                                                       #input.design_single == 'Split Plot with Plots in RCBD (SPRCBD)'"
                                                       
                                                       
                                                       uiOutput("rep_single_agrofims")
                                                     ),  
                                                     
                                                                                                        
                                                       uiOutput("trait_single_agrofims"),
                                                    
                                                     shiny::conditionalPanel(
                                                       condition = "input.design_single_agrofims == 'Split Plot Design'",
                                                       uiOutput("mainplot_single_agrofims"),
                                                       uiOutput("subplot_single_agrofims"),
                                                       uiOutput("subsubplot_single_agrofims")

                                                     ),
                                                     
                                                     
                                                      # 
                                                     downloadButton(outputId = "downloadagrofims_single_report", label = "Download"),
                                                     
                                                     
                                                     br()#,
                                                     # shiny::wellPanel(
                                                     #   shiny::HTML("<b>  </b>"),
                                                     #   rHandsontableOutput("single_agrofims_anova_fail_message")
                                                     # )
                                                     
                                                     
                                                     #uiOutput("run_single_agrofims")
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
