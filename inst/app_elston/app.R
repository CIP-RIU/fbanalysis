library(shiny)
#library(DT)
library(shinydashboard)
library(shinyFiles)
library(st4gi)
library(pepa)
library(readxl)
library(purrr)
library(knitr)
library(data.table)
library(traittools)


tabNameS <- "elston_aov"

server <- function(input, output, session,values) {
  values = shiny::reactiveValues()
  fbanalysis::elston_server(input, output, session, values = values)
}

ui <- dashboardPage(skin = "yellow",
                    dashboardHeader(title = "Statistical Analysis"),
                    dashboardSidebar(width = 200,
                                     menuItem("Resources",
                                              sidebarMenu(id = "menu",
                                                          menuSubItem("Elston Index", icon = icon("star"),
                                                                      tabName = tabNameS)
                                              )
                                     )
                    ),
                    dashboardBody(
                      
                      tabItems(
                        fbanalysis::elston_ui(name = tabNameS)
                      )
                    )
)

shinyApp(ui = ui, server = server)