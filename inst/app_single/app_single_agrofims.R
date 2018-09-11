library(fbanalysis)
library(httr)
library(shiny)
#library(DT)
library(shinydashboard)
library(shinyFiles)
library(st4gi)
library(pepa)
library(readxl)
library(knitr)
library(stringi)

tabNameS <- "analysis_aov"

server <- function(input, output, session,values) {
  values = shiny::reactiveValues()
  fbanalysis::single_hdagrofims_server(input, output, session, values = values)
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
                        fbanalysis::single_hdagrofims_ui(name = tabNameS)
                      )
                    )
)

shinyApp(ui = ui, server = server)




