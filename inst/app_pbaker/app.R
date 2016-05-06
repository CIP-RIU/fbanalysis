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

tabNameS <- "pbaker_aov"

server <- function(input, output, session,values) {
  values = shiny::reactiveValues()
  fbanalysis::pbaker_server(input, output, session, values = values)
}

ui <- dashboardPage(skin = "yellow",
                    dashboardHeader(title = "Statistical Analysis"),
                    dashboardSidebar(width = 200,
                                     menuItem("Resources",
                                              sidebarMenu(id = "menu",
                                                          menuSubItem("Pesek Baker Index", icon = icon("star"),
                                                                      tabName = tabNameS)
                                              )
                                     )
                    ),
                    dashboardBody(
                      
                      tabItems(
                        fbanalysis::pbaker_ui(name = tabNameS)
                      )
                    )
)

shinyApp(ui = ui, server = server)