library(shiny)
#library(DT)
library(shinydashboard)
library(shinyFiles)
library(st4gi)
library(pepa)
library(readxl)
library(knitr)

tabNameS <- "analysis_genetic"

server <- function(input, output, session,values) {
  values = shiny::reactiveValues()
  fbanalysis::genetic_server(input, output, session, values = values)
}

ui <- dashboardPage(skin = "yellow",
                    dashboardHeader(title = "Statistical Analysis"),
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
                        fbanalysis::genetic_ui(name = tabNameS)
                      )
                    )
)

shinyApp(ui = ui, server = server)




