#' UI for Single Environment Analysis in HiDAP
#' Returns user friendly ui
#' @author Omar Benites
#' @param type type of UI element, deault is a tab in a shinydashboard
#' @param title diaply title name
#' @param name UI TabName
#' @export
#' 

single_ui <- function(type = "tab", title = "Statistical Analysis for One Environment Trial", name = "analysis_single"){
  
  shinydashboard::tabItem(tabName = name,
                          h2(title),   
                          
                          box(
                            title = " ", status = "primary", solidHeader = TRUE,
                            collapsible = TRUE, width = NULL,
                            #tabsetPanel(
                            tabBox(width = 12,
                                   tabPanel("Single Analysis", #begin tabset "CHECK"
                                            fluidRow( 
                                              column(width = 8,
                                              shinyFiles::shinyFilesButton('file_single', 'Select File', 'Select a file',FALSE),
                                              infoBoxOutput("file_message_single", width = NULL),
                                              #selectInput('trait_single', 'Select Trait', c(Choose='', single_select_options()   ), selectize=TRUE),
                                              #selectInput('rep_single', 'Select Repetition', c(Choose='', names(iris)), selectize=TRUE),
                                              #selectInput('genotypes_single', 'Select Treatment', c(Choose='', names(iris)), selectize=TRUE),
                                              selectInput(inputId = 'design_single', label = 'Select statistical design of your experiment', choices = 
                                                                           c("Completely Randomized Design (CRD)",
                                                                          "Randomized Complete Block Design (RCBD)",
                                                                          "Augmented Block Design (ABD)",
                                                                          "Alpha Design(0,1) (AD)",
                                                                          "Factorial Two-Way Design in CRD (F2CRD)",
                                                                          "Factorial Two-Way Design in RCBD (F2RCBD)",
                                                                          "Split Plot with Plots in CRD (SPCRD)",
                                                                          "Split Plot with Plots in RCBD (SPRCBD)"),
                                                                          selected = "Randomized Complete Block Design (RCBD)",
                                                                          selectize=FALSE),
                                              # 
                                              # shiny::conditionalPanel(
                                              #   condition = 
                                              #   "input.design_single == 'Completely Randomized Design (CRD)' |
                                              #   input.design_single == 'Randomized Complete Block Design (RCBD)'|
                                              #   input.design_single == 'Augmented Block Design (ABD)'
                                              #   input.design_single == 'Alpha Design(0,1) (AD)'",
                                              #   
                                              #   uiOutput("genotypes_single"),
                                              #   uiOutput("trait_single")
                                              #   
                                              # ),
                                              
                                              shiny::conditionalPanel(                                                               
                                                condition = "input.design_single == 'Alpha Design(0,1) (AD)'|                      
                                                             input.design_single == 'Randomized Complete Block Design (RCBD)'|
                                                             input.design_single == 'Augmented Block Design (ABD)'| 
                                                             input.design_single == 'Split Plot with Plots in CRD (SPCRD)'|
                                                             input.design_single == 'Split Plot with Plots in RCBD (SPRCBD)'|     
                                                             input.design_single == 'Factorial Two-Way Design in CRD (F2CRD)'|
                                                             input.design_single == 'Factorial Two-Way Design in RCBD (F2RCBD)'",
                                                
                                                uiOutput("rep_single")
                                              ),  
                                              
                                              shiny::conditionalPanel(
                                                condition =  "input.design_single == 'Split Plot with Plots in CRD (SPCRD)'|
                                                              input.design_single == 'Split Plot with Plots in RCBD (SPRCBD)'|     
                                                              input.design_single == 'Factorial Two-Way Design in CRD (F2CRD)'|
                                                              input.design_single == 'Factorial Two-Way Design in RCBD (F2RCBD)'",
                                                
                                                uiOutput("factor_single")
                                              ),
                                              
                                              
                                              
                                              shiny::conditionalPanel(
                                                condition = "input.design_single == 'Alpha Design(0,1) (AD)'",
                                                
                                                uiOutput("block_single"),
                                                uiOutput("k_single")
                                                
                                              ),
                                              
                                              
                                              
                                              shiny::conditionalPanel(
                                                condition = "input.design_single == 'Alpha Design(0,1) (AD)'|
                                                             input.design_single == 'Completely Randomized Design (CRD)'|
                                                             input.design_single == 'Randomized Complete Block Design (RCBD)'|
                                                             input.design_single == 'Augmented Block Design (ABD)'|          
                                                             input.design_single == 'Split Plot with Plots in CRD (SPCRD)'| 
                                                             input.design_single == 'Split Plot with Plots in RCBD (SPRCBD)'|
                                                             input.design_single == 'Factorial Two-Way Design in CRD (F2CRD)'|
                                                             input.design_single == 'Factorial Two-Way Design in RCBD (F2RCBD)'",
                                                
                                                uiOutput("genotypes_single"),                                                    
                                                uiOutput("trait_single")                                                            
                                              ),                                                                                   
                                                                                                                                   
                                            
                                              
                                              #uiOutput("genotypes_single"),
                                              #uiOutput("rep_single"),
                                            
                                              #uiOutput("trait_single"),
                                          
                                                
                                              radioButtons(inputId="format_single", label="Report format", choices= c("html","word"), 
                                                           selected = "html", inline = TRUE, width = NULL),
                                              actionButton(inputId = "single_button", label= "Analyze", icon = icon("play-circle"),
                                                            width = NULL,height = NULL),
                                              
                                              
                                              br(),
                                              shiny::wellPanel(
                                                shiny::HTML("<b>  </b>"),
                                                rHandsontableOutput("single_anova_fail_message")
                                              )
                                              
                                              
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