library(shiny)
#library(DT)
library(shinydashboard)
library(shinyFiles)
library(st4gi)
library(pepa)
library(readxl)
library(knitr)

library(dplyr)
library(brapi)


tabNameS <- "met_aov_sbase"

server <- function(input, output, session,values) {
  values = shiny::reactiveValues()
  fbanalysis::met_server_sbase(input, output, session, values = values)
}

ui <- dashboardPage(skin = "yellow",
                    dashboardHeader(title = "MET Statistical Analysis in SweetPotato Base"),
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
                        fbanalysis::met_sbase_ui(name = tabNameS)
                      )
                    )
)

shinyApp(ui = ui, server = server)