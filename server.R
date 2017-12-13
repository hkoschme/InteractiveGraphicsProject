#load libraries
library(shiny)
library(shinydashboard)
library(devtools)
library(leaflet)
library(plotly)
library(tidyverse)
library(forcats)
library(reshape2)
library(sp)
library(datasets)
library(plyr)
library(dplyr)
library(data.table)
library(htmltools)
library(geosphere)
library(rgeos)
library(scales)
library(ggplot2)
library(ggmap)
library(maps)
library(rsconnect)

function(input, output){
  output$leaflet1 <- renderLeaflet({
    
    m_leaflet <- leaflet() %>% addTiles()%>%
      setView(lng = -93.85, lat = 37.45, zoom = 4) %>%
      addProviderTiles("MapBox", 
                       options = providerTileOptions(
                         id = "mapbox.light",
                         accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN'))) 
    
    if(input$leaflet1_percents == "Percent of
                              All Flights At That Airport"){
      m_leaflet <- m_leaflet %>%
        addCircles(airports2, lng = airports2$LONGITUDE, lat = airports2$LATITUDE,
                   radius = airports2$percent_delays*100000)
    } else {
      
      m_leaflet <- m_leaflet %>%
        addCircles(airports2, lng = airports2$LONGITUDE, lat = airports2$LATITUDE,
                   radius = airports2$count_dep_delays,
                   popup = airports2$V1)
    }
    return(m_leaflet)
  })
  
  
  output$leaflet2 <- renderLeaflet({
    
    m_leaflet <- leaflet() %>% addTiles()%>%
      setView(lng = -93.85, lat = 37.45, zoom = 4) %>%
      addProviderTiles("MapBox", 
                       options = providerTileOptions(
                         id = "mapbox.light",
                         accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN')))
    if(input$leaflet2_origin == "ATL - Hartsfield-Jackson Atlanta International Airport"){
      origin_arc <- origin_arc_ATL
      summary_pitt_origin <- summary_atl_origin
    }
    if(input$leaflet2_origin == "DEN - Denver International Airport"){
      origin_arc <- origin_arc_DEN
      summary_pitt_origin <- summary_den_origin
    }
    if(input$leaflet2_origin == "EWR - Newark Liberty International Airport"){
      origin_arc <- origin_arc_EWR
      summary_pitt_origin <- summary_ewr_origin
    }
    if(input$leaflet2_origin == "LAX - Los Angeles International Airport"){
      origin_arc <- origin_arc_LAX
      summary_pitt_origin <- summary_lax_origin
    }
    if(input$leaflet2_origin == "ORD - Chicago O'Hare International Airport"){
      origin_arc <- origin_arc_ORD
      summary_pitt_origin <- summary_ord_origin
    }
    if(input$leaflet2_origin == "PIT - Pittsburgh International Airport"){
      origin_arc <- origin_arc_PIT
      summary_pitt_origin <- summary_pit_origin
    }
    
    for(flight in 1:length(origin_arc)){
      m_leaflet <- m_leaflet %>%
        addPolylines(data = origin_arc[[flight]], weight = summary_pitt_origin[flight, "most_traveled"]/1000)
    }
    return(m_leaflet)
  })
  ###################NIKITA NIKITA NIKITA NIKITA NIKITA##################
  #make graph showing proportion of flights delayed by airline, filtering by month
  output$main_plot1  <- renderPlot({
    if (input$dept_arr_plot1 == "Arrival"){
      plot1 <- ggplot(data = subset(flights_percent_delay, 
                                    AIRLINE_FULL %in% input$subset_data_1)) + 
        aes(x = reorder(MONTH_FULL, MONTH), y = ARR_DELAY/COUNT, 
            group = AIRLINE_FULL, color = AIRLINE_FULL) + 
        geom_line(size = 2) + nikitagu_315_theme + 
        labs(x = "Month", y = "Fraction of Total Flights Delayed", 
             title = "Fraction of Flights Delayed Over 2015", 
             color = "Airline") + 
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
        scale_colour_brewer()
    } else {
      plot1 <- ggplot(data = subset(flights_percent_delay, AIRLINE_FULL %in% input$subset_data_1)) + 
        aes(x = reorder(MONTH_FULL, MONTH), y = DEPT_DELAY/COUNT, 
            group = AIRLINE_FULL, color = AIRLINE_FULL) + 
        geom_line(size = 2) + nikitagu_315_theme + 
        labs(x = "Month", y = "Fraction of Total Flights Delayed", 
             title = "Fraction of Flights Delayed Over 2015", 
             color = "Airline") + 
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
        scale_colour_brewer()
    }
    return(plot1)
  })
  #make graph showing number of flights by airline by weekday/end, interacting by month
  #note: change color scheme
  output$main_plot2  <- renderPlotly({
    plot2 <- ggplot(data = subset(flights_percent_delay, 
                                  AIRLINE_FULL %in% input$subset_data_2),
                    aes(x = reorder(MONTH_FULL, MONTH), 
                        y = COUNT, group = AIRLINE_FULL,
                        color = AIRLINE_FULL)) + 
      geom_line(size = 2) + nikitagu_315_theme + 
      labs(x = "Month", y = "Number of Flights", 
           title = "Number of Flights Over 2015", 
           color = "Airline") + 
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
      scale_colour_brewer()
    plot2_plotly <- ggplotly(plot2, tooltip = c("text", "y"))
    return(plot2_plotly)
  })
  # KEVIN
  output$main_plot3  <- renderPlot({
    if(input$subset_data_3 == "All") {
      state_border_subset <- state_borders
      state_border_subset$delay_color <- cut(state_border_subset$delay, 
                                             breaks = quantile(state_border_subset$delay, seq(0, 1, 0.2), na.rm = TRUE),
                                             c(paste(round(quantile(state_border_subset$delay, 0, na.rm = TRUE), 2), "-", round(quantile(state_border_subset$delay, 0.2, na.rm = TRUE), 2), sep = ""),
                                               paste(round(quantile(state_border_subset$delay, 0.2, na.rm = TRUE), 2), "-", round(quantile(state_border_subset$delay, 0.4, na.rm = TRUE), 2), sep = ""),
                                               paste(round(quantile(state_border_subset$delay, 0.4, na.rm = TRUE), 2), "-", round(quantile(state_border_subset$delay, 0.6, na.rm = TRUE), 2), sep = ""),
                                               paste(round(quantile(state_border_subset$delay, 0.6, na.rm = TRUE), 2), "-", round(quantile(state_border_subset$delay, 0.8, na.rm = TRUE), 2), sep = ""),
                                               paste(round(quantile(state_border_subset$delay, 0.8, na.rm = TRUE), 2), "-", round(quantile(state_border_subset$delay, 1, na.rm = TRUE), 2), sep = "")), include.lowest = TRUE)
    } else {
      state_border_subset <- state_borders_airline %>% dplyr::filter(as.character(AIRLINE_FULL) == input$subset_data_3)
      state_border_subset$delay_color <- cut(state_border_subset$delay, 
                                             breaks = quantile(state_border_subset$delay, seq(0, 1, 0.2), na.rm = TRUE),
                                             c(paste(round(quantile(state_border_subset$delay, 0, na.rm = TRUE), 2), "-", round(quantile(state_border_subset$delay, 0.2, na.rm = TRUE), 2), sep = ""),
                                               paste(round(quantile(state_border_subset$delay, 0.2, na.rm = TRUE), 2), "-", round(quantile(state_border_subset$delay, 0.4, na.rm = TRUE), 2), sep = ""),
                                               paste(round(quantile(state_border_subset$delay, 0.4, na.rm = TRUE), 2), "-", round(quantile(state_border_subset$delay, 0.6, na.rm = TRUE), 2), sep = ""),
                                               paste(round(quantile(state_border_subset$delay, 0.6, na.rm = TRUE), 2), "-", round(quantile(state_border_subset$delay, 0.8, na.rm = TRUE), 2), sep = ""),
                                               paste(round(quantile(state_border_subset$delay, 0.8, na.rm = TRUE), 2), "-", round(quantile(state_border_subset$delay, 1, na.rm = TRUE), 2), sep = "")), include.lowest = TRUE)
    }
    plot3 <- ggplot(state_border_subset) +
      geom_polygon(aes(x = long, y = lat, group = group, fill = delay_color), 
                   color = "black") + coord_map("polyconic") + 
      scale_fill_manual(values = c("purple", "plum",
                                   "pink", "gold", "orange"),
                        na.value = "black") +
      labs(title = "Average Departure Delays By State in 2015",
           x = "Longitude", y = "Latitude", 
           fill = "Average \nDelay Time \n(minutes)") + nikitagu_315_theme
    return(plot3)
  })
  output$main_plot3b  <- renderPlot({
    if(input$subset_data_3b == "All") {
      state_border_subset <- state_borders
      state_border_subset$delay_color <- cut(state_border_subset$delay, 
                                             breaks = quantile(state_border_subset$delay, seq(0, 1, 0.2), na.rm = TRUE),
                                             c(paste(round(quantile(state_border_subset$delay, 0, na.rm = TRUE), 2), "-", round(quantile(state_border_subset$delay, 0.2, na.rm = TRUE), 2), sep = ""),
                                               paste(round(quantile(state_border_subset$delay, 0.2, na.rm = TRUE), 2), "-", round(quantile(state_border_subset$delay, 0.4, na.rm = TRUE), 2), sep = ""),
                                               paste(round(quantile(state_border_subset$delay, 0.4, na.rm = TRUE), 2), "-", round(quantile(state_border_subset$delay, 0.6, na.rm = TRUE), 2), sep = ""),
                                               paste(round(quantile(state_border_subset$delay, 0.6, na.rm = TRUE), 2), "-", round(quantile(state_border_subset$delay, 0.8, na.rm = TRUE), 2), sep = ""),
                                               paste(round(quantile(state_border_subset$delay, 0.8, na.rm = TRUE), 2), "-", round(quantile(state_border_subset$delay, 1, na.rm = TRUE), 2), sep = "")), include.lowest = TRUE)
    } else {
      state_border_subset <- state_borders_airline %>% dplyr::filter(as.character(AIRLINE_FULL) == input$subset_data_3b)
      state_border_subset$delay_color <- cut(state_border_subset$delay, 
                                             breaks = quantile(state_border_subset$delay, seq(0, 1, 0.2), na.rm = TRUE),
                                             c(paste(round(quantile(state_border_subset$delay, 0, na.rm = TRUE), 2), "-", round(quantile(state_border_subset$delay, 0.2, na.rm = TRUE), 2), sep = ""),
                                               paste(round(quantile(state_border_subset$delay, 0.2, na.rm = TRUE), 2), "-", round(quantile(state_border_subset$delay, 0.4, na.rm = TRUE), 2), sep = ""),
                                               paste(round(quantile(state_border_subset$delay, 0.4, na.rm = TRUE), 2), "-", round(quantile(state_border_subset$delay, 0.6, na.rm = TRUE), 2), sep = ""),
                                               paste(round(quantile(state_border_subset$delay, 0.6, na.rm = TRUE), 2), "-", round(quantile(state_border_subset$delay, 0.8, na.rm = TRUE), 2), sep = ""),
                                               paste(round(quantile(state_border_subset$delay, 0.8, na.rm = TRUE), 2), "-", round(quantile(state_border_subset$delay, 1, na.rm = TRUE), 2), sep = "")), include.lowest = TRUE)
    }
    plot3b <- ggplot(state_border_subset) +
      geom_polygon(aes(x = long, y = lat, group = group, fill = delay_color), 
                   color = "black") + coord_map("polyconic") + 
      scale_fill_manual(values = c("purple", "plum",
                                   "pink", "gold", "orange"),
                        na.value = "black") +
      labs(title = "Average Departure Delays By State in 2015",
           x = "Longitude", y = "Latitude", 
           fill = "Average \nDelay Time \n(minutes)") + nikitagu_315_theme
    return(plot3b)
  })
  output$main_plot4 <- renderPlot({
    if(input$dept_arr_plot4 == "Departure") {
      if(input$subset_data_4 == "All") {
        plot4 <- ggplot(data = subset(flights_by_region_orig),
                        aes(x = Day_of_week, y = DEPT_DELAY)) + 
          geom_bar(stat = "identity", fill = "royalblue2") + facet_wrap(~ ORIGIN_REGION) +
          labs(title = "Average Departure Delay by Day and Region",
               x = "Day of Week", 
               y = "Average Delay (Minutes)") + nikitagu_315_theme + 
          theme(axis.text.x = element_text(angle = 45, hjust = 1))
      } else {
        plot4 <- ggplot(data = subset(flights_by_region_orig, 
                                      AIRLINE_FULL == input$subset_data_4),
                        aes(x = Day_of_week, y = DEPT_DELAY)) + 
          geom_bar(stat = "identity", fill = "royalblue2") + facet_wrap(~ ORIGIN_REGION) +
          labs(title = "Average Departure Delay by Day and Region",
               x = "Day of Week", 
               y = "Average Delay (Minutes)") + nikitagu_315_theme +
          theme(axis.text.x = element_text(angle = 45, hjust = 1))
      }
    } else {
      if(input$subset_data_4 == "All") {
        plot4 <- ggplot(data = subset(flights_by_region_dest),
                        aes(x = Day_of_week, y = ARR_DELAY)) + 
          geom_bar(stat = "identity", fill = "royalblue2") + facet_wrap(~ DEST_REGION) +
          labs(title = "Average Arrival Delay by Day and Region",
               x = "Day of Week", 
               y = "Average Delay (Minutes)") + nikitagu_315_theme + 
          theme(axis.text.x = element_text(angle = 45, hjust = 1))
      } else {
        plot4 <- ggplot(data = subset(flights_by_region_dest, 
                                      AIRLINE_FULL == input$subset_data_4),
                        aes(x = Day_of_week, y = ARR_DELAY)) + 
          geom_bar(stat = "identity", fill = "royalblue2") + facet_wrap(~ DEST_REGION) +
          labs(title = "Average Arrival Delay by Day and Region",
               x = "Day of Week", 
               y = "Average Delay (Minutes)", 
               fill = "Average Delay in Minutes"
          ) + nikitagu_315_theme +
          theme(axis.text.x = element_text(angle = 45, hjust = 1))
      }
    }
    return(plot4)
  })
  output$main_plot4b <- renderPlot({
    if(input$dept_arr_plot4b == "Departure") {
      if(input$subset_data_4b == "All") {
        plot4b <- ggplot(data = subset(flights_by_region_orig),
                        aes(x = Day_of_week, y = DEPT_DELAY)) + 
          geom_bar(stat = "identity", fill = "royalblue2") + facet_wrap(~ ORIGIN_REGION) +
          labs(title = "Average Departure Delay by Day and Region",
               x = "Day of Week", 
               y = "Average Delay (Minutes)") + nikitagu_315_theme + 
          theme(axis.text.x = element_text(angle = 45, hjust = 1))
      } else {
        plot4b <- ggplot(data = subset(flights_by_region_orig, 
                                      AIRLINE_FULL == input$subset_data_4b),
                        aes(x = Day_of_week, y = DEPT_DELAY)) + 
          geom_bar(stat = "identity", fill = "royalblue2") + facet_wrap(~ ORIGIN_REGION) +
          labs(title = "Average Departure Delay by Day and Region",
               x = "Day of Week", 
               y = "Average Delay (Minutes)") + nikitagu_315_theme +
          theme(axis.text.x = element_text(angle = 45, hjust = 1))
      }
    } else {
      if(input$subset_data_4b == "All") {
        plot4b <- ggplot(data = subset(flights_by_region_dest),
                        aes(x = Day_of_week, y = ARR_DELAY)) + 
          geom_bar(stat = "identity", fill = "royalblue2") + facet_wrap(~ DEST_REGION) +
          labs(title = "Average Arrival Delay by Day and Region",
               x = "Day of Week", 
               y = "Average Delay (Minutes)") + nikitagu_315_theme + 
          theme(axis.text.x = element_text(angle = 45, hjust = 1))
      } else {
        plot4b <- ggplot(data = subset(flights_by_region_dest, 
                                      AIRLINE_FULL == input$subset_data_4b),
                        aes(x = Day_of_week, y = ARR_DELAY)) + 
          geom_bar(stat = "identity", fill = "royalblue2") + facet_wrap(~ DEST_REGION) +
          labs(title = "Average Arrival Delay by Day and Region",
               x = "Day of Week", 
               y = "Average Delay (Minutes)", 
               fill = "Average Delay in Minutes"
          ) + nikitagu_315_theme +
          theme(axis.text.x = element_text(angle = 45, hjust = 1))
      }
    }
    return(plot4b)
  })
  #jin code
  output$plotly_ts <- renderPlotly({
    ts_dep_react <- reactive({
      if(input$AIRLINE_ts == "All") {
        dep_delay_ts_all <- dep_delay_ts %>%
          group_by(DATE) %>%
          dplyr::summarize(NumberofDelays = sum(NumberofDelays, na.rm = TRUE),
                           AverageDelayTime = mean(AverageDelayTime, na.rm = TRUE))
        dep_ma_28_num <- dep_delay_ts_all$NumberofDelays %>%
          get_weighted_moving_averages(ww = 28, 
                                       weights = c(rep(0.5, 21), 1, 1, 1, 1, 1, 3, 5))
        dep_ma_28_avg <- dep_delay_ts_all$AverageDelayTime %>%
          get_weighted_moving_averages(ww = 28, 
                                       weights = c(rep(0.5, 21), 1, 1, 1, 1, 1, 3, 5))
        dep_delay_ts_all <- dep_delay_ts_all %>% plyr::mutate(dep_ma_28_num = dep_ma_28_num,
                                                        dep_ma_28_avg = dep_ma_28_avg)
        return(dep_delay_ts_all)
      } else {
        dep_delay_ts$AIRLINE_FULL <- as.character(dep_delay_ts$AIRLINE_FULL)
        dep_delay_ts_filtered <- dep_delay_ts %>% dplyr::filter(AIRLINE_FULL == input$AIRLINE_ts)%>% group_by(DATE)
        dep_ma_28_num <- dep_delay_ts_filtered$NumberofDelays %>%
          get_weighted_moving_averages(ww = 28, 
                                     weights = c(rep(0.5, 21), 1, 1, 1, 1, 1, 3, 5))
        dep_ma_28_avg <- dep_delay_ts_filtered$AverageDelayTime %>%
          get_weighted_moving_averages(ww = 28, 
                                     weights = c(rep(0.5, 21), 1, 1, 1, 1, 1, 3, 5))
      return(dep_delay_ts_filtered %>% plyr::mutate(dep_ma_28_num = dep_ma_28_num,
                                              dep_ma_28_avg = dep_ma_28_avg))
      }
      })
    ts_arr_react <- reactive({
      if(input$AIRLINE_ts == "All") {
        arr_delay_ts_all <- arr_delay_ts %>%
          group_by(DATE) %>%
          dplyr::summarize(NumberofDelays = sum(NumberofDelays, na.rm = TRUE),
                           AverageDelayTime = mean(AverageDelayTime, na.rm = TRUE))
        arr_ma_28_num <- arr_delay_ts_all$NumberofDelays %>%
          get_weighted_moving_averages(ww = 28, 
                                       weights = c(rep(0.5, 21), 1, 1, 1, 1, 1, 3, 5))
        arr_ma_28_avg <- arr_delay_ts_all$AverageDelayTime %>%
          get_weighted_moving_averages(ww = 28, 
                                       weights = c(rep(0.5, 21), 1, 1, 1, 1, 1, 3, 5))
        arr_delay_ts_all <- arr_delay_ts_all %>% plyr::mutate(arr_ma_28_num = arr_ma_28_num,
                                                        arr_ma_28_avg = arr_ma_28_avg)
        return(arr_delay_ts_all)
      } else {
        arr_delay_ts$AIRLINE_FULL <- as.character(arr_delay_ts$AIRLINE_FULL)
        arr_delay_ts_filtered <- arr_delay_ts %>% 
          group_by(DATE) %>%
          dplyr::filter(AIRLINE_FULL == input$AIRLINE_ts)
        arr_ma_28_num <- arr_delay_ts_filtered$NumberofDelays %>%
          get_weighted_moving_averages(ww = 28, 
                                       weights = c(rep(0.5, 21), 1, 1, 1, 1, 1, 3, 5))
        arr_ma_28_avg <- arr_delay_ts_filtered$AverageDelayTime %>%
          get_weighted_moving_averages(ww = 28,
                                       weights = c(rep(0.5, 21), 1, 1, 1, 1, 1, 3, 5))
        return(arr_delay_ts_filtered %>% plyr::mutate(arr_ma_28_num = arr_ma_28_num,
                                              arr_ma_28_avg = arr_ma_28_avg))
      }
      })
    plot <- ggplot() + scale_x_date() +
      labs(title = paste(input$ts, "Over Time", sep = " "),
           x = "Time",
           y = input$ts
      ) + nikitagu_315_theme
    if("Departure" %in% input$add_dep_arr) {
      plot <- plot + geom_line(data = ts_dep_react(), 
                               aes_string(x = "DATE", 
                                          y = paste(unlist(strsplit(input$ts, split = " ")), 
                                                    collapse = ""),
                                          text = "DATE"),
                               color = "purple")
    }
    if("Arrival" %in% input$add_dep_arr) {
      plot <- plot +  geom_line(data = ts_arr_react(), 
                                aes_string(x = "DATE", paste(unlist(strsplit(input$ts, split = " ")), 
                                                             collapse = ""),
                                           text = "DATE"),
                                color = "orange", alpha = 0.75)
    }
    if(input$add_dep_ma & input$ts == "Number of Delays") {
      plot <- plot + geom_line(data = ts_dep_react(),
                               aes(x = DATE, y = dep_ma_28_num,
                                   text = DATE),
                               color = "red", alpha = 0.75)
    }
    if(input$add_dep_ma & input$ts == "Average Delay Time") {
      plot <- plot + geom_line(data = ts_dep_react(),
                               aes(x = DATE, y = dep_ma_28_avg,
                                   text = DATE),
                               color = "red", alpha = 0.75)
    }
    if(input$add_arr_ma & input$ts == "Number of Delays") {
      plot <- plot + geom_line(data = ts_arr_react(),
                               aes(x = DATE, y = arr_ma_28_num,
                                   text = DATE),
                               color = "blue", alpha = 0.75)
    }
    if(input$add_arr_ma & input$ts == "Average Delay Time") {
      plot <- plot + geom_line(data = ts_arr_react(),
                               aes(x = DATE, y = arr_ma_28_avg,
                                   text = DATE),
                               color = "blue", alpha = 0.75)
    }
    plot_plotly <- ggplotly(plot, tooltip = c("text", "y"))
    plot_plotly$elementId <- NULL
    return(plot_plotly)
  })
  output$choropleth <- renderLeaflet({
    if(input$season == "All Seasons") {
      us_data_dep_season <- us_data_dep
      us_data_arr_season <- us_data_arr
      us_data_dep_by_region_season <- us_data_dep_by_region
      us_data_arr_by_region_season <- us_data_arr_by_region
    }
    else{
      dep_by_state_season_filtered <- dep_by_state_season %>% 
        dplyr::filter(SEASON == input$season)
      arr_by_state_season_filtered <- arr_by_state_season %>% 
        dplyr::filter(SEASON == input$season)
      us_data_dep_season <- us_data %>%
        left_join(dep_by_state_season_filtered, 
                  by = c("region" = "O_STATE_NAME"))
      us_data_arr_season <- us_data %>%
        left_join(arr_by_state_season_filtered, 
                  by = c("region" = "D_STATE_NAME"))
      us_data_dep_by_region_season <- us_data_dep_season %>%
        group_by(region) %>%  dplyr::summarize(O_STATE = unique(O_STATE),
                                               COUNT = unique(COUNT)) %>%
        plyr::mutate(PERCENT = COUNT/sum(COUNT, na.rm = TRUE)*100)
      us_data_arr_by_region_season <- us_data_arr_season %>%
        group_by(region) %>%  dplyr::summarize(D_STATE = unique(D_STATE),
                                               COUNT = unique(COUNT)) %>%
        plyr::mutate(PERCENT = COUNT/sum(COUNT, na.rm = TRUE)*100)
    }
    if(input$dep_or_arr == "Departure") {
      df_chosen <- us_data_dep_season
      df_chosen2 <- us_data_dep_by_region_season
      pal_chosen <- colorBin(c("purple", "plum", "pink",
                               "gold", "orange"), domain = us_data_dep_season$COUNT,
                             bins = quantile(
                               us_data_dep_season$COUNT,
                               seq(0,1,by = .2),na.rm = T))
      labels_chosen <- paste0(
        "<strong>", us_data_dep_by_region_season$O_STATE, 
        "</strong><br/>Number of Flight Departure: ",
        us_data_dep_by_region_season$COUNT,
        "<br/>Percent of Total Departures: ",
        round(us_data_dep_by_region_season$PERCENT, 3)) %>% lapply(htmltools::HTML)
    }
    else {
      df_chosen <- us_data_arr_season
      df_chosen2 <- us_data_arr_by_region_season
      pal_chosen <- colorBin(c("purple", "plum", "pink",
                               "gold", "orange"), domain = us_data_arr_season$COUNT,
                             bins = quantile(
                               us_data_arr_season$COUNT,
                               seq(0,1,by = .2),na.rm = T))
      labels_chosen <- paste0(
        "<strong>", us_data_arr_by_region_season$D_STATE, 
        "</strong><br/>Number of Flight Arrivals: ",
        us_data_arr_by_region_season$COUNT,
        "<br/>Percent of Total Arrivals: ",
        round(us_data_arr_by_region_season$PERCENT, 3)) %>% lapply(htmltools::HTML)
      
    }
    spPolygons <- SpatialPolygons(lapply(unique(df_chosen$region), get_spPoly, df = df_chosen))
    spdf <- SpatialPolygonsDataFrame(spPolygons, data=data.frame(df_chosen2, row.names = df_chosen2$region))
    m_leaflet <- leaflet(spdf) %>%
      setView(lng = -93.85, lat = 37.45, zoom = 4) %>%
      addProviderTiles("MapBox", 
                       options = providerTileOptions(
                         id = "mapbox.light",
                         accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN'))) %>%
      addPolygons(
        fillColor = pal_chosen(df_chosen2$COUNT),
        weight = 1,
        opacity = 1,
        color = "white",
        dashArray = "3",
        fillOpacity = 0.7,
        highlight = highlightOptions(
          weight = 3,
          color = "#666",
          dashArray = "",
          fillOpacity = 0.7,
          bringToFront = TRUE),
        label = labels_chosen,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "16px",
          direction = "auto")) %>%
      addLegend(pal = pal_chosen, 
                values = df_chosen2$COUNT,
                title = "Number of Departures/Arrivals by State",
                position = "bottomleft")
    return(m_leaflet)
  })
  ###################NIKITA NIKITA NIKITA NIKITA NIKITA##################
}