#' UI for open books in HiDAP
#' Returns user friendly ui
#' @author Omar Benites
#' @param type type of UI element, deault is a tab in a shinydashboard
#' @param title diaply title name
#' @param name UI TabName
#' @export
#' 

anova_ui <- function(type = "tab", title = "Analysis of Variance", name = "analysis_aov"){
  
  shinydashboard::tabItem(tabName = name,
                          h2(title),   
                          
                          box(
                            title = "Analysis of Variance", status = "primary", solidHeader = TRUE,
                            collapsible = TRUE, width = NULL,
                            #tabsetPanel(
                            tabBox(width = 12,
                                   tabPanel("ANOVA", #begin tabset "CHECK"
                                            fluidRow( 
                                              shinyFiles::shinyFilesButton('file', 'File select', 'Please select a file',FALSE),
                                              #selectInput('trait_anova', 'Select Trait', c(Choose='', anova_select_options()   ), selectize=TRUE),
                                              #selectInput('rep_anova', 'Select Repetition', c(Choose='', names(iris)), selectize=TRUE),
                                              #selectInput('genotypes_anova', 'Select Treatment', c(Choose='', names(iris)), selectize=TRUE),
                                              uiOutput("trait"),
                                              uiOutput("rep"),
                                              uiOutput("genotypes"),
                                              actionButton(inputId = "anova_button", label= "Analyze", icon = icon("play-circle"),
                                                           width = NULL,height = NULL)
                                              
                                            )#, end fluidow
                                            
                                   )#,#end tab Panel "CHECK"
                                   

                            )
                          )
  )#End data_processing tabItem
  
}