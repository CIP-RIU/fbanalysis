#' UI for PVS's analysis of variance
#' Returns user friendly ui
#' @author Omar Benites
#' @param type type of UI element, deault is a tab in a shinydashboard
#' @param title diaply title name
#' @param name UI TabName
#' @export
#'

pvs_anova_ui <- function(type = "tab", title = "Anova PVS", name = "anova_pvs"){

  #shinydashboard::tabItem(tabName = name,
  # tabPanel(title, icon = icon("tags", lib = "glyphicon"),
  #            br(),
  #            h2(title),
  #                   fluidRow(
  #                         box(
  #                           title = " ", status = "primary", solidHeader = TRUE,
  #                           collapsible = TRUE, width = NULL,
  #                           #tabsetPanel(
  #                           tabBox(width = 12,

  #shinydashboard::tabItem(tabName = name,                         
                                   
                          #tabPanel("PVS Anova", #begin tabset "anova pvs"
                                 tabPanel(title,  #begin tabset "anova pvs"
                                          h2(title),
                                           fluidRow(
                                              column(width = 6,
                                                     shinyFiles::shinyFilesButton('file_pvs_anova', 'Select File', 'Please select a file',FALSE),
                                                     infoBoxOutput("pvs_message_anova",width = NULL),

                                                     uiOutput("sheet_pvs_anova"),
                                                       # selectInput(inputId = 'design_pvs_anova', label = 'Select statistical design of your experiment', choices =
                                                       #             c(#"Completely Randomized Design (CRD)",
                                                       #               "Randomized Complete Block Design (RCBD)"
                                                       #               ),
                                                       #           selected = "Randomized Complete Block Design (RCBD)",
                                                       #           selectize=FALSE),


                                                     # shiny::conditionalPanel(
                                                     #   condition = "input.design_pvs_anova == 'Randomized Complete Block Design (RCBD)'",

                                                       # uiOutput("genotypes_pvs_anova"),
                                                       # uiOutput("trait_pvs_anova"),
                                                     # ),

                                                     # shiny::conditionalPanel(
                                                     #   condition =
                                                     #   #"input.design_single == 'Alpha Design(0,1) (AD)'|
                                                     #   "input.design_pvs_anova == 'Randomized Complete Block Design (RCBD)'",
                                                      
                                                       uiOutput("pvs_anova_genotypes"),
                                                       uiOutput("pvs_anova_rep"),
                                                       uiOutput("pvs_anova_trait"),
                                                     
                                                     radioButtons(inputId="format_pvs_anova", label="Report format", choices= c("html","word"),
                                                                  selected = "html", inline = TRUE, width = NULL),
                                                     actionButton(inputId = "button_pvs_anova", label= "Analyze", icon = icon("play-circle"),
                                                                  width = NULL,height = NULL)

                                                     )#end column

                                              )#, end fluidow
                                            )

                                   #       )#,#end tab Panel "anova pvs"

                                   

#                           )#End data_processing tabItem

}