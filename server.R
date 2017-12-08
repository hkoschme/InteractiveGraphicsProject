


function(input, output){
  output$leaflet <- renderLeaflet({
    
    m_leaflet <- leaflet() %>% addTiles()%>%
      setView(lng = -93.85, lat = 37.45, zoom = 4) %>%
      addProviderTiles("MapBox", 
                       options = providerTileOptions(
                         id = "mapbox.light",
                         accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN'))) %>%
      addCircles(airports, lng = airports$LONGITUDE, lat = airports$LATITUDE, 
                 radius = airports$count_dep_delays)
    # addLegend(pal = pal, 
    #           values = ~world_info$lifeExp,
    #           title = "Life Expectancy, 2007 (Qrt)")
    # 
    return(m_leaflet)
  })
}