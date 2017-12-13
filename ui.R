dashboardPage(skin = "black",
  #create dashboard page 2
  dashboardHeader(title = "Group 14: We Speak for Planes"),
  dashboardSidebar(width = "300px",
                   #design sidebar
                   sidebarMenu(
                     menuItem("Which States Have the Most Flights?", tabName = "choropleth", 
                              icon = icon("th")),
                     menuItem("Which Airlines Have the Most Flights?", tabName = "main_plot2", 
                              icon = icon("th")),
                     menuItem("Which Airlines Have the Most Delays?", tabName = "main_plot1", 
                              icon = icon("th")),
                     menuItem("How do Delays Change Over the Year?", tabName = "ts", 
                              icon = icon("th")),
                     menuItem("Which Airports Have the Most Delays", tabName = "leaflet1", 
                              icon = icon("th")),
                     menuItem("Which Routes are the Most Popular?", tabName = "leaflet2", 
                              icon = icon("th")),
                     menuItem("Which States Have Higher Departure Delays?", tabName = "main_plot3", 
                              icon = icon("th")),
                     menuItem("Compare Average Delays by Region & Airline", tabName = "main_plot4", 
                              icon = icon("th"))
                   )
  ),
  dashboardBody( #organize body
    #first change font etc
    tags$head(tags$style(HTML('
      .main-header .logo {
                              font-family: "Georgia", Times, "Times New Roman", serif;
                              font-weight: bold;
                              font-size: 24px;
                              }
                              '))),
    #draw plots/tabs
    tabItems(
      tabItem(tabName = "leaflet1", #if leaflet1 tab, then display 1st graph
              fluidRow(
                box(
                  leafletOutput(outputId = "leaflet1"), width = 12
                ),
                checkboxInput(inputId = "leaflet1_percents", 
                              "Flights Delayed as Percent of
                              All Flights At That Airport", value = FALSE, width = NULL)
                )
              ),
      tabItem(tabName = "leaflet2", #if leaflet2 tab, then display 2nd graph
              fluidRow(
                box(leafletOutput(outputId = "leaflet2"), width = 12),
                selectInput(inputId = "leaflet2_origin", 
                            choices = c("ATL - Hartsfield-Jackson Atlanta International Airport",
                                        "DEN - Denver International Airport",
                                        "EWR - Newark Liberty International Airport",
                                        "LAX - Los Angeles International Airport",
                                        "ORD - Chicago O'Hare International Airport",
                                        "PIT - Pittsburgh International Airport"),
                            selected = "PIT - Pittsburgh International Airport",
                            label = paste("Flights with ","Origin/Destination")
                )
              )
      ),
      ############################# NIKITA NIKITA NIKITA NIKITA###################
      tabItem(tabName = "main_plot1", #if main_plot1 then display 3rd graph
              fluidRow(
                box(
                  plotOutput(outputId = "main_plot1"), width = 12,
                  selectInput(inputId = "subset_data_1", label = "Airline", 
                              choices = c("American Airlines Inc.", 
                                          "American Eagle Airlines Inc.", 
                                          "Atlantic Southeast Airlines",
                                          "Delta Air Lines Inc.", 
                                          "Skywest Airlines Inc.", 
                                          "Southwest Airlines Co.", 
                                          "Alaska Airlines Inc.", 
                                          "Frontier Airlines Inc.", 
                                          "Hawaiian Airlines Inc.", 
                                          "JetBlue Airways", 
                                          "Spirit Air Lines", 
                                          "United Air Lines Inc.",
                                          "US Airways Inc.",
                                          "Virgin America"), 
                              multiple = T, selected = c("US Airways Inc.", 
                                                         "Southwest Airlines Co.",
                                                         "American Airlines Inc.",
                                                         "United Air Lines Inc.")),
                  selectInput(inputId = "dept_arr_plot1",
                              label = "Departure or Arrival",
                              choices = c("Departure", "Arrival"),
                              selected = "Departure")
                )
              )
      ),
      tabItem(tabName = "main_plot2", #if main_plot2 then display 4th graph
              fluidRow(
                box(
                  plotlyOutput(outputId = "main_plot2"), width = 12,
                  selectInput(inputId = "subset_data_2", label = "Airline", 
                              choices = c("American Airlines Inc.", 
                                          "American Eagle Airlines Inc.", 
                                          "Atlantic Southeast Airlines",
                                          "Delta Air Lines Inc.", 
                                          "Skywest Airlines Inc.", 
                                          "Southwest Airlines Co.", 
                                          "Alaska Airlines Inc.", 
                                          "Frontier Airlines Inc.", 
                                          "Hawaiian Airlines Inc.", 
                                          "JetBlue Airways", 
                                          "Spirit Air Lines", 
                                          "United Air Lines Inc.",
                                          "US Airways Inc.",
                                          "Virgin America"), 
                              multiple = T, selected = c("US Airways Inc.", 
                                                         "Southwest Airlines Co.",
                                                         "American Airlines Inc.",
                                                         "United Air Lines Inc."))
                  
                )
              )
      ),
      tabItem(tabName = "main_plot3", #if main_plot3 then display 5th graph
              fluidRow(
                box(
                  plotOutput(outputId = "main_plot3"), width = 6,
                  selectInput(inputId = "subset_data_3", label = "Airline", 
                              choices = c("All", 
                                          "American Airlines Inc.", 
                                          "American Eagle Airlines Inc.", 
                                          "Atlantic Southeast Airlines",
                                          "Delta Air Lines Inc.", 
                                          "Skywest Airlines Inc.", 
                                          "Southwest Airlines Co.", 
                                          "Alaska Airlines Inc.", 
                                          "Frontier Airlines Inc.", 
                                          "JetBlue Airways", 
                                          "Spirit Air Lines", 
                                          "United Air Lines Inc.",
                                          "US Airways Inc.",
                                          "Virgin America"), 
                              selected = "All")
                ),
                box(
                  plotOutput(outputId = "main_plot3b"), width = 6,
                  selectInput(inputId = "subset_data_3b", label = "Airline", 
                              choices = c("All", 
                                          "American Airlines Inc.", 
                                          "American Eagle Airlines Inc.", 
                                          "Atlantic Southeast Airlines",
                                          "Delta Air Lines Inc.", 
                                          "Skywest Airlines Inc.", 
                                          "Southwest Airlines Co.", 
                                          "Alaska Airlines Inc.", 
                                          "Frontier Airlines Inc.", 
                                          "JetBlue Airways", 
                                          "Spirit Air Lines", 
                                          "United Air Lines Inc.",
                                          "US Airways Inc.",
                                          "Virgin America"), 
                              selected = "All")
                )
              )
      ),
      tabItem(tabName = "main_plot4", #if main_plot4 then display 6th graph
              fluidRow(
                box(
                  plotOutput(outputId = "main_plot4"), width = 6,
                  selectInput(inputId = "subset_data_4", label = "Airline", 
                              choices = c("All", 
                                          "American Airlines Inc.", 
                                          "American Eagle Airlines Inc.", 
                                          "Atlantic Southeast Airlines",
                                          "Delta Air Lines Inc.", 
                                          "Skywest Airlines Inc.", 
                                          "Southwest Airlines Co.", 
                                          "Alaska Airlines Inc.", 
                                          "Frontier Airlines Inc.", 
                                          "Hawaiian Airlines Inc.", 
                                          "JetBlue Airways", 
                                          "Spirit Air Lines", 
                                          "United Air Lines Inc.",
                                          "US Airways Inc.",
                                          "Virgin America"), 
                              selected = "All"),
                  selectInput(inputId = "dept_arr_plot4",
                              label = "Departure or Arrival",
                              choices = c("Departure", "Arrival"),
                              selected = "Departure")),
                  box(
                    plotOutput(outputId = "main_plot4b"), width = 6,
                    selectInput(inputId = "subset_data_4b", label = "Airline", 
                                choices = c("All", 
                                            "American Airlines Inc.", 
                                            "American Eagle Airlines Inc.", 
                                            "Atlantic Southeast Airlines",
                                            "Delta Air Lines Inc.", 
                                            "Skywest Airlines Inc.", 
                                            "Southwest Airlines Co.", 
                                            "Alaska Airlines Inc.", 
                                            "Frontier Airlines Inc.", 
                                            "Hawaiian Airlines Inc.", 
                                            "JetBlue Airways", 
                                            "Spirit Air Lines", 
                                            "United Air Lines Inc.",
                                            "US Airways Inc.",
                                            "Virgin America"), 
                                selected = "All"),
                    selectInput(inputId = "dept_arr_plot4b",
                                label = "Departure or Arrival",
                                choices = c("Departure", "Arrival"),
                                selected = "Departure")
                  )
              )
      ),
      #JIN's code
      tabItem(tabName = "ts", #if ts then display 7th graph
              fluidRow(
                box(width = 12,
                    selectInput(inputId = "ts",
                                label = "Which time series to plot?",
                                choices = c("Number of Delays", "Average Delay Time"),
                                selected = "Number of Delays"),
                    selectInput(inputId = "AIRLINE_ts",
                                label = "Airline:",
                                choices = c("All", 
                                            "American Airlines Inc.", 
                                            "American Eagle Airlines Inc.", 
                                            "Atlantic Southeast Airlines",
                                            "Delta Air Lines Inc.", 
                                            "Skywest Airlines Inc.", 
                                            "Southwest Airlines Co.", 
                                            "Alaska Airlines Inc.", 
                                            "Frontier Airlines Inc.", 
                                            "Hawaiian Airlines Inc.", 
                                            "JetBlue Airways", 
                                            "Spirit Air Lines", 
                                            "United Air Lines Inc.",
                                            "US Airways Inc.",
                                            "Virgin America"),
                                selected = "All"),
                    selectizeInput(inputId = "add_dep_arr",
                                   label = "For departure or arrival?",
                                   choices = c("Departure", "Arrival"),
                                   multiple = TRUE,
                                   options = list(placeholder = "Select departure and/or arrival")),
                    checkboxInput(inputId = "add_dep_ma",
                                  label = "Add Departure Moving Average",
                                  value = TRUE),
                    checkboxInput(inputId = "add_arr_ma",
                                  label = "Add Arrival Moving Average",
                                  value = FALSE),
                    plotlyOutput(outputId = "plotly_ts", width = "100%")
                )
              )
      ),
      tabItem(tabName = "choropleth", #if choropleth then display 8th graph
              fluidRow(
                box(
                  leafletOutput(outputId = "choropleth"), width = 12,
                  selectInput(inputId = "dep_or_arr",
                              label = "Departure or Arrival",
                              choices = c("Departure", "Arrival"),
                              selected = "Departure"),
                  selectInput(inputId = "season",
                              label = "Season",
                              choices = c("All Seasons", "Spring",
                                          "Summer", "Fall",
                                          "Winter"),
                              selected = "All Seasons")
                )
              )
      )
      )
)
############################# NIKITA NIKITA NIKITA NIKITA###################
)