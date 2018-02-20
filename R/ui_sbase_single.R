#' UI for Single Environment Analysis in HiDAP
#' Returns user friendly ui
#' @author Omar Benites
#' @param type type of UI element, deault is a tab in a shinydashboard
#' @param title diaply title name
#' @param name UI TabName
#' @export
#'

single_sbase_ui <- function(type = "tab", title = "Statistical Analysis for One Environment Trial", name = "analysis_single"){
  
  shinydashboard::tabItem(tabName = name,
                          h2(title),
                          
                          box(#begin  box
                            title = " ", status = "primary", solidHeader = TRUE,
                            collapsible = TRUE, width = NULL,
                            #tabsetPanel(
                            tabBox(width = 12, #begintab box
                                   tabPanel("Single Analysis", #begin tabset "single analysis sbase"
                                            fluidRow(
                                              
                                              column(width = 12,
                                                     
                                                     box( # first begin box
                                                       title = " ", status = "primary", solidHeader = FALSE,
                                                       collapsible = FALSE, width = NULL,
                                                       
                                                       #actionButton(inputId= "connect_single_sbase", "Connect to Sweetpotato Base"),
                                                       infoBoxOutput("file_message_single_sbase", width = NULL),
                                                       
                                                       uiOutput("programName_single_sbase"),
                                                       uiOutput("trialName_single_sbase"),
                                                       uiOutput("studyName_single_sbase"),
                                                       shinysky::shinyalert("alert_single_sbase_done", FALSE, auto.close.after = 8)#,
                                                       
                                                     ), #end first box
                                                     
                                                     
                                                     conditionalPanel( condition = "output.show_single_sbase_params",  ##conditional Panel for Inputs        
                                                                       
                                                                       box(
                                                                         #"Ingrese una lista de familias o clones", width = 4, status = "primary", height = "730px",
                                                                         title = " ", width =  NULL, height="500px"  ,status = "primary", solidHeader = FALSE, collapsible = TRUE,
                                                                         
                                                                         conditionalPanel( condition = "output.show_single_sbase_len",
                                                                                           
                                                                                           selectInput(inputId = 'design_single_sbase', label = 'Select statistical design of your experiment', choices =
                                                                                                         c("Completely Randomized Design (CRD)",
                                                                                                           "Randomized Complete Block Design (RCBD)",
                                                                                                           "Augmented Block Design (ABD)",
                                                                                                           "Alpha Design(0,1) (AD)",
                                                                                                           "Factorial Two-Way Design in CRD (F2CRD)",
                                                                                                           "Factorial Two-Way Design in RCBD (F2RCBD)"),
                                                                                                       #"Split Plot with Plots in CRD (SPCRD)",
                                                                                                       #"Split Plot with Plots in RCBD (SPRCBD)"),
                                                                                                       selected = "Completely Randomized Design (CRD)",
                                                                                                       selectize=FALSE),
                                                                                           
                                                                                           
                                                                                           shiny::conditionalPanel(
                                                                                             condition = "input.design_single_sbase == 'Alpha Design(0,1) (AD)'|
                                                                                             input.design_single_sbase == 'Randomized Complete Block Design (RCBD)'|
                                                                                             input.design_single_sbase == 'Augmented Block Design (ABD)'|
                                                                                             input.design_single_sbase == 'Factorial Two-Way Design in CRD (F2CRD)'|
                                                                                             input.design_single_sbase == 'Factorial Two-Way Design in RCBD (F2RCBD)'",#|
                                                                                             #input.design_single == 'Split Plot with Plots in CRD (SPCRD)'|
                                                                                             #input.design_single == 'Split Plot with Plots in RCBD (SPRCBD)'"
                                                                                             uiOutput("rep_single_sbase")
                                                                                           ),
                                                                                           
                                                                                           shiny::conditionalPanel(
                                                                                             condition =  "input.design_single_sbase == 'Factorial Two-Way Design in CRD (F2CRD)'|
                                                                                             input.design_single_sbase == 'Factorial Two-Way Design in RCBD (F2RCBD)'",
                                                                                             # input.design_single == 'Split Plot with Plots in CRD (SPCRD)'|
                                                                                             # input.design_single == 'Split Plot with Plots in RCBD (SPRCBD)'|
                                                                                             uiOutput("factor_single_sbase")
                                                                                           ),
                                                                                           
                                                                                           
                                                                                           shiny::conditionalPanel(
                                                                                             condition = "input.design_single_sbase == 'Alpha Design(0,1) (AD)'",
                                                                                             uiOutput("block_single_sbase"),
                                                                                             uiOutput("k_single_sbase")
                                                                                           ),
                                                                                           
                                                                                           shiny::conditionalPanel(
                                                                                             condition = "input.design_single_sbase == 'Alpha Design(0,1) (AD)'|
                                                                                             input.design_single_sbase == 'Completely Randomized Design (CRD)'|
                                                                                             input.design_single_sbase == 'Randomized Complete Block Design (RCBD)'|
                                                                                             input.design_single_sbase == 'Augmented Block Design (ABD)'|
                                                                                             input.design_single_sbase == 'Factorial Two-Way Design in CRD (F2CRD)'|
                                                                                             input.design_single_sbase == 'Factorial Two-Way Design in RCBD (F2RCBD)'",
                                                                                             uiOutput("genotypes_single_sbase"),
                                                                                             uiOutput("trait_single_sbase")
                                                                                           ),
                                                                                           
                                                                                           # radioButtons(inputId="format_single_sbase", label="Report format", choices= c("html","word"),
                                                                                           #              selected = "html", inline = TRUE, width = NULL),
                                                                                           
                                                                                           radioButtons(inputId="format_single_sbase", label="Report format", choices= c("word"),
                                                                                                        selected = "html", inline = TRUE, width = NULL),
                                                                                           
                                                                                           shiny::conditionalPanel(condition = "input.format_single_sbase == 'word'",
                                                                                                                   
                                                                                                                   downloadButton(outputId = "downloadSbase_single_report", label = "Download"),
                                                                                                                   tags$head(tags$style(".butt1{background-color:orange;} .butt1{color: black;} .butt1{font-family: Courier New}")) 
                                                                                           ),
                                                                                           
                                                                                           shiny::conditionalPanel(condition = "input.format_single_sbase == 'html'",
                                                                                                                   
                                                                                                                   actionButton(inputId = "single_button_sbase", label= "Analyze", icon = icon("play-circle"),
                                                                                                                                width = NULL, height = NULL)
                                                                                                                   
                                                                                           ),
                                                                                           
                                                                                           shinysky::shinyalert("alert_singleReport_sbase_done", FALSE, auto.close.after = 8)#,
                                                                                           
                                                                                           
                                                                                           # br()#,
                                                                         ) #end second box
                                                                       )#, #end conditinal panel for Inputs
                                                                       
                                                                       )
                                                                       )
                                              
                                              
                                              #)#, end fluidow
                                              
                                            )#,#end tab Panel "single analysis"
                                            
                                            
                                            
                                            ) #end tab box
                            ), #end box
                            
                            br(),
                            br(),
                            br()
                          )
                          
                          )#End data_processing tabItem
  
}
