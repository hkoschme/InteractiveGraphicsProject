


function(input, output, session){
  output$leaflet1 <- renderLeaflet({
    
    # selected_data <- subset(airports, a)
    
    m_leaflet <- leaflet() %>% addTiles()%>%
      setView(lng = -93.85, lat = 37.45, zoom = 4) %>%
      addProviderTiles("MapBox", 
                       options = providerTileOptions(
                         id = "mapbox.light",
                         accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN'))) %>%
      addCircles(airports, lng = airports$LONGITUDE, lat = airports$LATITUDE, 
                 radius = airports$MQ)
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
  })
}