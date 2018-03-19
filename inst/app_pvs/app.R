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


tabNameS <- "psv_analysis"

server <- function(input, output, session,values) {
  values = shiny::reactiveValues()
  fbanalysis::pvs_server(input, output, session, values = values)
}

ui <- dashboardPage(skin = "yellow",
                    dashboardHeader(title = "Statistical Analysis for PVS trials"),
                    dashboardSidebar(width = 200,
                                     menuItem("Resources",
                                              sidebarMenu(id = "menu",
                                                          menuSubItem("PVS trials", icon = icon("star"),
                                                                      tabName = tabNameS)
                                              )
                                     )
                    ),
                    dashboardBody(
                      
                      tabItems(
                        fbanalysis::ui_pvs(name = tabNameS)
                      )
                    )
)

shinyApp(ui = ui, server = server)





