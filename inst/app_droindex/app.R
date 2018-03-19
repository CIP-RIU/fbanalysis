library(shiny)
#library(DT)
library(shinydashboard)
library(shinyFiles)
library(st4gi)
library(pepa)
library(readxl)
library(knitr)

tabNameS <- "analysis_droindex"

server <- function(input, output, session,values) {
  values = shiny::reactiveValues()
  fbanalysis::droindex_server(input, output, session, values = values)
}

ui <- dashboardPage(skin = "yellow",
                    dashboardHeader(title = "Drought Index"),
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
                        fbanalysis::droindex_ui(name = tabNameS)
                      )
                    )
)

shinyApp(ui = ui, server = server)




