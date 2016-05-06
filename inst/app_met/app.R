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



tabNameS <- "elston_aov"

server <- function(input, output, session,values) {
  values = shiny::reactiveValues()
  fbanalysis::met_server(input, output, session, values = values)
}

ui <- dashboardPage(skin = "yellow",
                    dashboardHeader(title = "Statistical Analysis"),
                    dashboardSidebar(width = 200,
                                     menuItem("Resources",
                                              sidebarMenu(id = "menu",
                                                          menuSubItem("MET", icon = icon("star"),
                                                                      tabName = tabNameS)
                                              )
                                     )
                    ),
                    dashboardBody(
                      
                      tabItems(
                        fbanalysis::met_ui(name = tabNameS)
                      )
                    )
)

shinyApp(ui = ui, server = server)