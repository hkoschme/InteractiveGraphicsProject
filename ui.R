library(shinydashboard)
library(shinythemes)

dashboardPage(
  
  dashboardHeader(),
  dashboardSidebar(),
  dashboardBody(
    fluidPage(
      fluidRow(
        box(
          leafletOutput(outputId = "leaflet", width = "650px"), width = 12
        )
      )
    )
  )
)