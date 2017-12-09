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
          leafletOutput(outputId = "leaflet1", width = "650px"), width = 12
        ),
        selectizeInput(inputId = "airlinesLeaflet1", label = "Airlines", 
                        choices = airliners_list,
                        multiple = TRUE)
      ),
      fluidRow(
        box(
          leafletOutput(outputId = "leaflet2", width = "650px"), width = 12
        )
      )
    )
  )
)