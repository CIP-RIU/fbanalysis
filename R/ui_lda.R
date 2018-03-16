#' UI for Linear Discriminant Analysis in HiDAP
#' 
#' Returns user friendly ui
#' @author Omar Benites
#' @param type type of UI element, deault is a tab in a shinydashboard
#' @param title diaply title name
#' @param name UI TabName
#' @export
#' 

lda_ui <- function(type = "tab", title = "Linear Discriminant Analysis", name = "analysis_lda"){
  
  shinydashboard::tabItem(tabName = name,
                          h2(title),   
                          
                          box(
                            title = " ", status = "primary", solidHeader = TRUE,
                            collapsible = TRUE, width = NULL,
                            #tabsetPanel(
                            tabBox(width = 12,
                                   tabPanel("lda Analysis", #begin tabset "CHECK"
                                            fluidRow( 
                                              column(width = 8,
                                                     shinyFiles::shinyFilesButton('file_lda', 'Select File', 'Select a file',FALSE),
                                                     infoBoxOutput("file_message_lda", width = NULL),
                                                     #selectInput('trait_lda', 'Select Trait', c(Choose='', lda_select_options()   ), selectize=TRUE),
                                                     #selectInput('rep_lda', 'Select Repetition', c(Choose='', names(iris)), selectize=TRUE),
                                                     #selectInput('genotypes_lda', 'Select Treatment', c(Choose='', names(iris)), selectize=TRUE),
                                                     
                                                     
                                                     
                                                     #Internament exlucir PLOT, REP, INSTN, FACTOR.
                                                     
                                                     #Show Intermediate DT table with results of Ginni Index
                                                     
                                                     #Use conditional Panel to show each part after another
                                                     
                                                     uiOutput("uifactor_lda"),
                                                     #uiOutput("lvl_factor_lda"),
                                                     uiOutput("uitrait_lda"),
                                                     
                                                     #1: Correlation matrix
                                                     
                                                     
                                                     #2: GINI TABLE : Terminar GINI TABLE
                                                     DT::dataTableOutput('x12'),
                                                     
                                                     
                                                     # 3: Report with discriminant analysis 
                                                     # VER ABAJO
                                                     
                                                     # selectInput(inputId = 'design_lda', label = 'Select factor', choices =),
                                                     # selectInput(inputId = 'design_lda', label = 'Select level of the factor', choices =),
                                                     # selectInput(inputId = 'design_lda', label = 'Select traits', choices =),
                                                     
                                                     
                                                     
                                                     # 
                                                     # shiny::conditionalPanel(
                                                     #   condition = 
                                                     #   "input.design_lda == 'Completely Randomized Design (CRD)' |
                                                     #   input.design_lda == 'Randomized Complete Block Design (RCBD)'|
                                                     #   input.design_lda == 'Augmented Block Design (ABD)'
                                                     #   input.design_lda == 'Alpha Design(0,1) (AD)'",
                                                     #   
                                                     #   uiOutput("genotypes_lda"),
                                                     #   uiOutput("trait_lda")
                                                     #   
                                                     # ),
                                                   
                                                     
                                                     
                                                     #uiOutput("genotypes_lda"),
                                                     #uiOutput("rep_lda"),
                                                     
                                                     #uiOutput("trait_lda"),
                                                     
                                                     
                                                     radioButtons(inputId="format_lda", label="Report format", choices= c("html","word"), 
                                                                  selected = "html", inline = TRUE, width = NULL),
                                                     actionButton(inputId = "lda_button", label= "Analyze", icon = icon("play-circle"),
                                                                  width = NULL,height = NULL),
                                                     
                                                     
                                                     br()#,
                                                     # shiny::wellPanel(
                                                     #   shiny::HTML("<b>  </b>"),
                                                     #   rHandsontableOutput("lda_anova_fail_message")
                                                     # )
                                                     
                                                     
                                                     #uiOutput("run_lda")
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