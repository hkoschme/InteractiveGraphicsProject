
library(geosphere)
library(sp)

function(input, output, session){
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