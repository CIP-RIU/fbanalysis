#' UI for Drought Indexes
#' Returns user friendly ui
#' @author Omar Benites
#' @param type type of UI element, deault is a tab in a shinydashboard
#' @param title diaply title name
#' @param name UI TabName
#' @export
#' 

droindex_ui <- function(type = "tab", title = "Drought Index", name = "analysis_droindex"){
  
  shinydashboard::tabItem(tabName = name,
                          h2(title),   
                          
                          box(
                            title = " ", status = "primary", solidHeader = TRUE,
                            collapsible = TRUE, width = NULL,
                            #tabsetPanel(
                            tabBox(width = 12,
                                   tabPanel("Drought Index", #begin tabset "CHECK"
                                            fluidRow( 
                                              column(width = 8,
                                                     shinyFiles::shinyFilesButton('file_droindex', 'Select File', 'Select a file',FALSE),
                                                     infoBoxOutput("file_message_droindex", width = NULL),
                                                     #selectInput('trait_droindex', 'Select Trait', c(Choose='', droindex_select_options()   ), selectize=TRUE),
                                                     #selectInput('rep_droindex', 'Select Repetition', c(Choose='', names(iris)), selectize=TRUE),
                                                     #selectInput('genotypes_droindex', 'Select Treatment', c(Choose='', names(iris)), selectize=TRUE),
                                                    
                                                      # selectInput(inputId = 'design_droindex', label = 'Select statistical design of your experiment', choices = 
                                                      #              c("Completely Randomized Design (CRD)",
                                                      #                "Randomized Complete Block Design (RCBD)",
                                                      #                "Augmented Block Design (ABD)",
                                                      #                "Alpha Design(0,1) (AD)",
                                                      #                "Factorial Two-Way Design in CRD (F2CRD)",
                                                      #                "Factorial Two-Way Design in RCBD (F2RCBD)"),
                                                      #            #"Split Plot with Plots in CRD (SPCRD)",
                                                      #            #"Split Plot with Plots in RCBD (SPRCBD)"),
                                                      #            selected = "Randomized Complete Block Design (RCBD)",
                                                      #            selectize=FALSE),
                                                     
                                                     # 
                                                     # shiny::conditionalPanel(
                                                     #   condition = 
                                                     #   "input.design_droindex == 'Completely Randomized Design (CRD)' |
                                                     #   input.design_droindex == 'Randomized Complete Block Design (RCBD)'|
                                                     #   input.design_droindex == 'Augmented Block Design (ABD)'
                                                     #   input.design_droindex == 'Alpha Design(0,1) (AD)'",
                                                     #   
                                                     #   uiOutput("genotypes_droindex"),
                                                     #   uiOutput("trait_droindex")
                                                     #   
                                                     # ),
                                                     
                                                     
                                                     
                                                     # Repetition input --------------------------------------------------------
                                                     
                                                     # shiny::conditionalPanel(                                                               
                                                     #   condition = "input.design_droindex == 'Alpha Design(0,1) (AD)'|                      
                                                     #   input.design_droindex == 'Randomized Complete Block Design (RCBD)'|
                                                     #   input.design_droindex == 'Augmented Block Design (ABD)'| 
                                                     #   input.design_droindex == 'Factorial Two-Way Design in CRD (F2CRD)'|
                                                     #   input.design_droindex == 'Factorial Two-Way Design in RCBD (F2RCBD)'|
                                                     #   input.design_droindex == 'Split Plot with Plots Design'",
                                                     #   #input.design_droindex == 'Split Plot with Plots in CRD (SPCRD)'|
                                                     #   #input.design_droindex == 'Split Plot with Plots in RCBD (SPRCBD)'"
                                                       
                                                       
                                                       #uiOutput("rep_droindex"),
                                                     #),  
                                                     
                                                     # Factor input --------------------------------------------------------
                                                     
                                                     
                                                     # shiny::conditionalPanel(
                                                     #   condition =  "input.design_droindex == 'Factorial Two-Way Design in CRD (F2CRD)'|
                                                     #   input.design_droindex == 'Factorial Two-Way Design in RCBD (F2RCBD)'",
                                                       # input.design_droindex == 'Split Plot with Plots in CRD (SPCRD)'|
                                                       # input.design_droindex == 'Split Plot with Plots in RCBD (SPRCBD)'|
                                                       
                                                       
                                                       uiOutput("factor_droindex"),
                                                     #),
                                                     
                                                     # K or block alplha input --------------------------------------------------------
                                                     
                                                     # shiny::conditionalPanel(
                                                     #   condition = "input.design_droindex == 'Alpha Design(0,1) (AD)'",
                                                     #   
                                                     #   uiOutput("block_droindex"),
                                                     #   uiOutput("k_droindex")
                                                     #   
                                                     # ),
                                                     # 
                                                     
                                                     # Design input --------------------------------------------------------
                                                     
                                                     # shiny::conditionalPanel(
                                                     #   condition = "input.design_droindex == 'Alpha Design(0,1) (AD)'|
                                                     #   input.design_droindex == 'Completely Randomized Design (CRD)'|
                                                     #   input.design_droindex == 'Randomized Complete Block Design (RCBD)'|
                                                     #   input.design_droindex == 'Augmented Block Design (ABD)'|          
                                                     #   input.design_droindex == 'Factorial Two-Way Design in CRD (F2CRD)'|
                                                     #   input.design_droindex == 'Factorial Two-Way Design in RCBD (F2RCBD)'",
                                                       #input.design_droindex == 'Split Plot with Plots in CRD (SPCRD)'| 
                                                       #input.design_droindex == 'Split Plot with Plots in RCBD (SPRCBD)'|
                                                       
                                                       
                                                       uiOutput("genotypes_droindex"),                                                    
                                                       uiOutput("trait_droindex"),                                                           
                                                     # ),                                                                                   
                                                     
                                                     
                                                     # Level input --------------------------------------------------------
                                                     uiOutput("lvl_stress_droindex"),
                                                     uiOutput("lvl_control_droindex"),
                                                     
                                                     
                                                     #radioButtons(inputId="format_droindex", label="Report format", choices= c("html","word"), 
                                                     #             selected = "html", inline = TRUE, width = NULL),
                                                     actionButton(inputId = "droindex_button", label= "Download", icon = icon("play-circle"),
                                                                  width = NULL,height = NULL),
                                                     
                                                     br(),
                                                     br(),
                                                     br(),
                                                     DT::dataTableOutput('tbl'),
                                                     
                                                     
                                                     br()#,
                                                     # shiny::wellPanel(
                                                     #   shiny::HTML("<b>  </b>"),
                                                     #   rHandsontableOutput("droindex_anova_fail_message")
                                                     # )
                                                     
                                                     
                                                     #uiOutput("run_droindex")
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


