#' UI for Trend Environment Analysis for HIDAP-AgroFIMS
#' Returns user friendly ui
#' @author Omar Benites
#' @param type type of UI element, deault is a tab in a shinydashboard
#' @param title diaply title name
#' @param name UI TabName
#' @export
#'

trend_hdagrofims_ui <- function(type = "tab", title = "Trend Analysis", name = "analysis_trend"){
  
  
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
                                                     fileInput('file_trend_agrofims', 'Choose fieldbook file', accept = c(".zip")),  
                                                     
                                                     infoBoxOutput("file_message_trend_agrofims", width = NULL),
                                                      selectInput(inputId = 'design_trend_agrofims', 
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
                                                                 selected = "Completely Randomized Design (CRD)",
                                                                 selectize=FALSE),
                                                     
                                                     
                                                     
                                                     shiny::conditionalPanel(
                                                       condition = "input.design_trend_agrofims == 'Factorial with CRD'|
                                                                    input.design_trend_agrofims == 'Factorial with RCBD'",
                                                       
                                                       
                                                       uiOutput("factor_trend_agrofims")
                                                     ),
                                                     
                                                     # shiny::conditionalPanel(
                                                     #   condition =  "input.design_trend_agrofims == 'Factorial Two-Way Design in CRD (F2CRD)'|
                                                     #   input.design_trend_agrofims == 'Factorial Two-Way Design in RCBD (F2RCBD)'",
                                                     #   # input.design_trend == 'Split Plot with Plots in CRD (SPCRD)'|
                                                     #   # input.design_trend == 'Split Plot with Plots in RCBD (SPRCBD)'|
                                                     #   
                                                     #   uiOutput("factor2_trend_agrofims")
                                                     # ),
                                                     
                                                     
                                                     
                                                     
                                                     shiny::conditionalPanel(
                                                       condition = "input.design_trend_agrofims == 'Alpha Design(0,1) (AD)'|
                                                       input.design_trend_agrofims == 'Completely Randomized Design (CRD)'|
                                                       input.design_trend_agrofims == 'Randomized Complete Block Design (RCBD)'|
                                                       input.design_trend_agrofims == 'Augmented Block Design (ABD)'",          
                                                       #input.design_trend_agrofims == 'Factorial Two-Way Design in CRD (F2CRD)'|
                                                       #input.design_trend_agrofims == 'Factorial Two-Way Design in RCBD (F2RCBD)'",
                                                       #input.design_trend_agrofims == 'Split Plot with Plots in CRD (SPCRD)'| 
                                                       #input.design_trend_agrofims == 'Split Plot with Plots in RCBD (SPRCBD)'|
                                                       uiOutput("trt_trend_agrofims")#,                                                    
                                                       
                                                     ),
                                                     
                                                     # "Factorial Design",
                                                     # "Split Plot with Two Factors",
                                                     # "Split Plot with Three Factors"
                                                     
                                                     
                                                     shiny::conditionalPanel(                                                               
                                                       condition = "input.design_trend_agrofims == 'Alpha Design(0,1) (AD)'|                      
                                                       input.design_trend_agrofims == 'Randomized Complete Block Design (RCBD)'|
                                                       input.design_trend_agrofims == 'Augmented Block Design (ABD)'| 
                                                       input.design_trend_agrofims == 'Factorial with RCBD'",
                                                       #input.design_trend_agrofims == 'Split Plot Design'",
                                                       #input.design_trend == 'Split Plot with Plots in CRD (SPCRD)'|
                                                       #input.design_trend == 'Split Plot with Plots in RCBD (SPRCBD)'"
                                                       
                                                       
                                                       uiOutput("rep_trend_agrofims")
                                                     ),  
                                                     
                                                     #New code
                                                     uiOutput("eu_trend_agrofims"),
                                                     
                                                     checkboxGroupInput("stat_trend_agrofims", "Statistical options",
                                                                        c("Mean" = "mean",
                                                                          #"Gears" = "gear",
                                                                          "Standard error" = "se"),
                                                                        selected = c("mean","se"),
                                                                        inline = TRUE
                                                                        ),
                                                                          
                                                                        
                                                     
                                                     uiOutput("stat_trend_agrofims"),
                                                     #uiOutput("se_trend_agrofims"),
                                                     
                                                     #
                                                     
                                                     uiOutput("trait_trend_agrofims"),
                                                     
                                                     shiny::conditionalPanel(
                                                       condition = "input.design_trend_agrofims == 'Split Plot Design'",
                                                       uiOutput("mainplot_trend_agrofims"),
                                                       uiOutput("subplot_trend_agrofims"),
                                                       uiOutput("subsubplot_trend_agrofims")
                                                       
                                                     ),
                                                     
                                                     
                                                     # 
                                                     downloadButton(outputId = "downloadagrofims_trend_report", label = "Download"),
                                                     
                                                     
                                                     br()#,
                                                     # shiny::wellPanel(
                                                     #   shiny::HTML("<b>  </b>"),
                                                     #   rHandsontableOutput("trend_agrofims_anova_fail_message")
                                                     # )
                                                     
                                                     
                                                     #uiOutput("run_trend_agrofims")
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
