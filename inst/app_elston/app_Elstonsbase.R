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

library(pepa)
library(readxl)
library(knitr)

library(dplyr)
library(brapi)

tabNameS <- "elston_sbase"

server <- function(input, output, session,values) {
  values = shiny::reactiveValues()
  fbanalysis::elston_server_sbase(input, output, session, values = values)
}

ui <- dashboardPage(skin = "yellow",
                    dashboardHeader(title = "Elston SweetPotatoBase"),
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
                        fbanalysis::elston_ui_sbase(name = tabNameS)
                      )
                    )
)

shinyApp(ui = ui, server = server)