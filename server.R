

function(input, output){
  output$leaflet1 <- renderLeaflet({
    
    # selected_data <- subset(airports, a)
    
    m_leaflet <- leaflet() %>% addTiles()%>%
      setView(lng = -93.85, lat = 37.45, zoom = 4) %>%
      addProviderTiles("MapBox", 
                       options = providerTileOptions(
                         id = "mapbox.light",
                         accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN'))) # %>%
      # addPopups(airports, lng = airports$LONGITUDE, lat = airports$LATITUDE, popup = htmlEscape("Potato"))
    
    if(input$leaflet1_percents){
      m_leaflet <- m_leaflet %>%
        addCircles(airports, lng = airports$LONGITUDE, lat = airports$LATITUDE,
                   radius = airports$percent_delays*50000)
                   #, 
                   #label= ~htmlEscape())
    } else {
      textlist <- ddply(airports, .(AIRPORT), function(x){
        values <- c(x$AA, x$AS, x$B6, x$DL, x$EV, x$F9,x$HA, x$MQ, x$NK,x$OO,x$UA,
             x$US,x$VX)
        for( i in 1:length(values)){
          if(is.na(values[i])) values[i] = 0
        }
        values <- as.character(values)
        print(values[1])
        paste("United Air Lines Inc.", values[1], "<br/>","American Airlines Inc.", values[2], "<br/>", 
"US Airways Inc.", values[3], "<br/>","Frontier Airlines Inc.", values[4], "<br/>","JetBlue Airways", values[5], "<br/>",
"Skywest Airlines Inc.", values[6], "<br/>","Alaska Airlines Inc.", values[7], "<br/>", "Spirit Air Lines", values[8], "<br/>",
"Southwest Airlines Co." , values[9], "<br/>","Delta Air Lines Inc.", values[10], 
"<br/>","Atlantic Southeast Airlines", values[11], "<br/>",
"Hawaiian Airlines Inc.", values[12], "<br/>", "American Eagle Airlines Inc.", values[13], "<br/>", 
"Virgin America", values[14], "<br/>")
        
      }
      )
      
      
      m_leaflet <- m_leaflet %>%
        addCircles(airports, lng = airports$LONGITUDE, lat = airports$LATITUDE,
                   radius = airports$count_dep_delays,
                   popup = textlist$V1
                    ) 
    #label = htmlEscape(names(airliners_list)),
      #labelOptions = labelOptions(
      # noHide = 'T', textOnly = T,
      # offset=c(-5,-10), textsize = '15px',
      # style=list('color'='black'))
    }#daply(airports, .(AIRPORT), listfun)
    
    
    # if()
    # addLegend(pal = pal, 
    #           values = ~world_info$lifeExp,
    #           title = "Life Expectancy, 2007 (Qrt)")
    # 
    
   
      
    return(m_leaflet)
  })
  output$leaflet2 <- renderLeaflet({
    
    m_leaflet <- leaflet() %>% addTiles()%>%
      setView(lng = -93.85, lat = 37.45, zoom = 4) %>%
      addProviderTiles("MapBox", 
                       options = providerTileOptions(
                         id = "mapbox.light",
                         accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN')))
    if(input$leaflet2_destination){
      for(flight in 1:length(destination_arc)){
        m_leaflet <- m_leaflet %>%
          addPolylines(data = destination_arc[[flight]])
      }
  
    } else {
      for(flight in 1:length(origin_arc)){
        m_leaflet <- m_leaflet %>% 
          addPolylines(data = origin_arc[[flight]])
      }
    }
    return(m_leaflet)
  })
}

###################NIKITA NIKITA NIKITA NIKITA NIKITA##################
#make graph showing proportion of flights delayed by airline, filtering by month
output$main_plot1 <- renderPlot({
  if (input$subset_data == "2015") { 
    plot1 <- ggplot(data = subset(flights), #DO NOT FILTER BY MONTH IF ALL SELECTED
                    aes(x = AIRLINE_FULL, fill = delay_var)) + 
      geom_bar(position = "fill") + coord_flip() + nikitagu_315_theme + 
      labs(x = "Airline", y = "Proportion of Flights Delayed", 
           fill = "Flight Status", 
           title = "Proportion of Flights Delayed by Airline in 2015")  + 
      scale_fill_manual(labels = c("Not Delayed", "Delayed", "Cancelled"), 
                        values = c("blue", "red"), na.value = "black")
  } else {
    plot1 <- ggplot(data = subset(flights, MONTH_FULL == input$subset_data), #filter by month as input
                    aes(x = AIRLINE_FULL, fill = delay_var)) + 
      geom_bar(position = "fill") + coord_flip() + nikitagu_315_theme + 
      labs(x = "Airline", y = "Proportion of Flights Delayed", 
           fill = "Flight Status", 
           title = "Proportion of Flights Delayed by Airline in 2015") + 
      scale_fill_manual(labels = c("Not Delayed", "Delayed", "Cancelled"), 
                        values = c("blue", "red"), na.value = "black")
  }
  return(plot1)
})
#make graph showing number of flights by airline by weekday/end, interacting by month
output$main_plot2  <- renderPlot({
  if (input$subset_data_2 == "2015") { 
    plot2 <- ggplot(data = flights, #DO NOT FILTER BY MONTH IF ALL SELECTED
                    aes(x = AIRLINE_FULL, fill = as.factor(DAY_OF_WEEK))) + 
      geom_bar() + coord_flip() + nikitagu_315_theme + 
      labs(x = "Airline", y = "Number of Flights", fill = "Day of Week",
           title = "Number of Flights by Airline by Day of Week in 2015") + 
      scale_fill_manual(labels = c("Monday", "Tuesday", "Wednesday", "Thursday", 
                                   "Friday", "Saturday", "Sunday"), 
                        values = c("#000000", "#E69900", "#59B3E6", "#F2E640", 
                                   "#0073B3", "#CC6600", "#CC99B3"))
  } else {
    plot2 <- ggplot(data = subset(flights, MONTH_FULL == input$subset_data_2), #DO NOT FILTER BY MONTH IF ALL SELECTED
                    aes(x = AIRLINE_FULL, fill = as.factor(DAY_OF_WEEK))) + 
      geom_bar() + coord_flip() + nikitagu_315_theme + 
      labs(x = "Airline", y = "Number of Flights", fill = "Day of Week",
           title = "Number of Flights by Airline by Day of Week in 2015") + 
      scale_fill_manual(labels = c("Monday", "Tuesday", "Wednesday", "Thursday", 
                                   "Friday", "Saturday", "Sunday"), 
                        values = c("#000000", "#E69900", "#59B3E6", "#F2E640", 
                                   "#0073B3", "#CC6600", "#CC99B3"))
  }
  return(plot2)
})
# KEVIN
output$main_plot3  <- renderPlot({
  if(input$subset_data_3 == "All") {
    plot3 <- ggplot(state_borders) +
      geom_polygon(aes(x = long, y = lat, group = group, fill = delay), 
                   color = "black") + coord_map("polyconic") + 
      scale_fill_gradient(low = "plum1", high = "purple4", na.value = "black", 
                          breaks = c(6, 10, 14), 
                          labels = c("Low", "Medium", "High")) + 
      labs(title = "Average Departure Delays By State in 2015",
           x = "Longitude", y = "Latitude", 
           fill = "Average \nDelay Time \n(minutes)")
  } else {
    plot3 <- ggplot(subset(state_borders, AIRLINE_FULL == input$subset_data_3)) +
      geom_polygon(aes(x = long, y = lat, group = group, fill = delay,
                       midpoint = mean(delay)), 
                   color = "black") + coord_map("polyconic") + 
      scale_fill_gradient(low = "plum1", high = "purple4", na.value = "black", 
                          breaks = c(6, 10, 14), 
                          labels = c("Low", "Medium", "High")) +
      labs(title = "Average Departure Delays By State in 2015",
           x = "Longitude", y = "Latitude", 
           fill = "Average \nDelay Time \n(minutes)")
  }
  return(plot3)
})
output$main_plot4 <- renderPlot({
  if(input$subset_data_4 == "All") {
    plot4 <- ggplot(data = flights_day_region_airline, 
                    aes(x = DAY_OF_WEEK_FULL, fill = ORIGIN_REGION, y = DELAY)) + 
      geom_bar(stat = "identity") + nikitagu_315_theme + 
      labs(x = "Day of Week", y = "Average Time Delayed (in minutes)", 
           fill = "Region", 
           title = "Average Flight Delays by Region by Day of Week in 2015")
  } else {
    plot4 <- ggplot(data = subset(flights_day_region_airline, 
                                  AIRLINE_FULL == input$subset_data_4), 
                    aes(x = DAY_OF_WEEK_FULL, fill = ORIGIN_REGION, y = DELAY)) + 
      geom_bar(stat = "identity") + nikitagu_315_theme + 
      labs(x = "Day of Week", y = "Average Time Delayed (in minutes)", 
           fill = "Region", 
           title = "Average Flight Delays by Region by Day of Week in 2015")
  }
  return(plot4)
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
      dep_delay_ts_all <- dep_delay_ts_all %>% mutate(dep_ma_28_num = dep_ma_28_num,
                                                      dep_ma_28_avg = dep_ma_28_avg)
      return(dep_delay_ts_all)
    }
    dep_delay_ts_filtered <- dep_delay_ts %>% dplyr::filter(AIRLINE_FULL == input$AIRLINE_ts)
    dep_ma_28_num <- dep_delay_ts_filtered$NumberofDelays %>%
      get_weighted_moving_averages(ww = 28, 
                                   weights = c(rep(0.5, 21), 1, 1, 1, 1, 1, 3, 5))
    dep_ma_28_avg <- dep_delay_ts_filtered$AverageDelayTime %>%
      get_weighted_moving_averages(ww = 28, 
                                   weights = c(rep(0.5, 21), 1, 1, 1, 1, 1, 3, 5))
    return(dep_delay_ts_filtered %>% mutate(dep_ma_28_num = dep_ma_28_num,
                                            dep_ma_28_avg = dep_ma_28_avg))
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
      arr_delay_ts_all <- arr_delay_ts_all %>% mutate(arr_ma_28_num = arr_ma_28_num,
                                                      arr_ma_28_avg = arr_ma_28_avg)
      return(arr_delay_ts_all)
    }
    arr_delay_ts_filtered <- arr_delay_ts %>% dplyr::filter(AIRLINE_FULL == input$AIRLINE_ts)
    arr_ma_28_num <- arr_delay_ts_filtered$NumberofDelays %>%
      get_weighted_moving_averages(ww = 28, 
                                   weights = c(rep(0.5, 21), 1, 1, 1, 1, 1, 3, 5))
    arr_ma_28_avg <- arr_delay_ts_filtered$AverageDelayTime %>%
      get_weighted_moving_averages(ww = 28, 
                                   weights = c(rep(0.5, 21), 1, 1, 1, 1, 1, 3, 5))
    return(arr_delay_ts_filtered %>% mutate(arr_ma_28_num = arr_ma_28_num,
                                            arr_ma_28_avg = arr_ma_28_avg))
  })
  plot <- ggplot() + scale_x_date() +
    labs(title = paste(input$ts, "Over Time", sep = " "),
         x = "Time",
         y = input$ts
    ) + nikitagu_315_theme
  if("Departure" %in% input$add_dep_arr) {
    plot <- plot + geom_line(data = ts_dep_react(), 
                             aes_string(x = "DATE", y = input$ts,
                                        text = "DATE"),
                             color = "purple")
  }
  if("Arrival" %in% input$add_dep_arr) {
    plot <- plot +  geom_line(data = ts_arr_react(), 
                              aes_string(x = "DATE", y = input$ts,
                                         text = "DATE"),
                              color = "orange", alpha = 0.75)
  }
  if(input$add_dep_ma & input$ts == "NumberofDelays") {
    plot <- plot + geom_line(data = ts_dep_react(),
                             aes(x = DATE, y = dep_ma_28_num,
                                 text = DATE),
                             color = "red", alpha = 0.75)
  }
  if(input$add_dep_ma & input$ts == "AverageDelayTime") {
    plot <- plot + geom_line(data = ts_dep_react(),
                             aes(x = DATE, y = dep_ma_28_avg,
                                 text = DATE),
                             color = "red", alpha = 0.75)
  }
  if(input$add_arr_ma & input$ts == "NumberofDelays") {
    plot <- plot + geom_line(data = ts_arr_react(),
                             aes(x = DATE, y = arr_ma_28_num,
                                 text = DATE),
                             color = "blue", alpha = 0.75)
  }
  if(input$add_arr_ma & input$ts == "AverageDelayTime") {
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
      mutate(PERCENT = COUNT/sum(COUNT, na.rm = TRUE)*100)
    us_data_arr_by_region_season <- us_data_arr_season %>%
      group_by(region) %>%  dplyr::summarize(D_STATE = unique(D_STATE),
                                             COUNT = unique(COUNT)) %>%
      mutate(PERCENT = COUNT/sum(COUNT, na.rm = TRUE)*100)
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