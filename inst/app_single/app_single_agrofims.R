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
library(reticulate)
library(shinyalert)

tabNameS <- "analysis_aov"
tabNameS2 <- "analysis_trend"
server <- function(input, output, session,values) {
  values = shiny::reactiveValues()
  fbanalysis::single_hdagrofims_server(input, output, session, values = values)
  fbanalysis::trend_hdagrofims_server(input, output, session, values = values)
}

ui <- dashboardPage(skin = "yellow",
                    dashboardHeader(title = "Statistical Analysis"),
                    dashboardSidebar(width = 200,
                                     menuItem("Resources",
                                              sidebarMenu(id = "menu",
                                                          menuSubItem("ANOVA", icon = icon("star"),
                                                                      tabName = tabNameS),
                                                          menuSubItem("Tred", icon = icon("star"),
                                                                      tabName = tabNameS2)
                                              )
                                     )
                    ),
                    dashboardBody(
                      
                      tabItems(
                        fbanalysis::single_hdagrofims_ui(name = tabNameS),
                        fbanalysis::trend_hdagrofims_ui(name= tabNameS2)
                      )
                    )
)

shinyApp(ui = ui, server = server)




