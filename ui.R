library(shinydashboard)
library(shinythemes)
library(shiny)

dashboardPage(
  
  dashboardHeader(),
  dashboardSidebar(),
  dashboardBody(
    fluidPage(
      fluidRow(
        box(
          leafletOutput(outputId = "leaflet1"), width = 12
        ),
        selectizeInput(inputId = "airlinesLeaflet1", label = "Airlines", 
                        choices = airliners_list,
                        multiple = TRUE,
                       selected = c("UA","US","B6","OO","NK","AA","F9","AS","WN","DL","EV","HA","MQ","VX"))
      ),
      fluidRow(
        box(
          leafletOutput(outputId = "leaflet2"), width = 12
        )
      )
    )
  )
)