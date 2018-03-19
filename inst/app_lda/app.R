library(shiny)
#library(DT)
library(shinydashboard)
library(shinyFiles)
library(st4gi)
library(pepa)
library(readxl)
library(knitr)

library(randomForest)
library(MASS)

tabNameS <- "analysis_LDA"

server <- function(input, output, session,values) {
  values = shiny::reactiveValues()
  fbanalysis::lda_server(input, output, session, values = values)
}

ui <- dashboardPage(skin = "yellow",
                    dashboardHeader(title = "Linear discriminant analysis"),
                    dashboardSidebar(width = 200,
                                     menuItem("Resources",
                                              sidebarMenu(id = "menu",
                                                          menuSubItem("ANOVA", icon = icon("star"),
                                                                      tabName = tabNameS)
                                              )
                                     )
                    ),
                    dashboardBody(
                      
                      tabItems(
                        fbanalysis::lda_ui(name = tabNameS)
                      )
                    )
)

shinyApp(ui = ui, server = server)




