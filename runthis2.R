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
################# NIKITA NIKITA NIKITA
#load data
flights_percent_delay <- readRDS("flights_percent_delay.rds")
state_borders <- readRDS("state_borders.rds")
state_borders_airline <- readRDS("state_borders_airline.rds")
dep_delay_ts <- readRDS("dep_delay_ts.rds")
arr_delay_ts <- readRDS("arr_delay_ts.rds")
us_data_dep <- readRDS("us_data_dep.rds")
us_data_arr <- readRDS("us_data_arr.rds")
us_data_dep_by_region <- readRDS("us_data_dep_by_region.rds")
us_data_arr_by_region <- readRDS("us_data_arr_by_region.rds")
dep_by_state_season <- readRDS("dep_by_state_season.rds")
arr_by_state_season <- readRDS("arr_by_state_season.rds")




#add theme
nikitagu_315_theme <-  theme_bw() + 
  theme(axis.text = element_text(size = 10, color = "royalblue2"), 
        text = element_text(size = 12, face = "bold", 
                            color = "royalblue4"))


############NIKITA NIKITA NIKITA NIKITA OVER #################

flights <- fread("flights.csv")
airports <- read.csv("airports.csv")
airlines <- read.csv("airlines.csv")
################ HANNAH HANNAH HANNAH
#remove small airports

big_flights <- flights[grep("[[:digit:]]+",flights$ORIGIN_AIRPORT, invert=TRUE), ]

#add binary variable containing whether there was a delay
delay_var <- big_flights$DEPARTURE_DELAY > 0
big_flights$delay_var <- delay_var

#add number of delays per airport to airports df
origins_and_departures <- big_flights[,c("ORIGIN_AIRPORT","delay_var", "AIRLINE")]
split_origin_airport <- split(origins_and_departures, origins_and_departures$ORIGIN_AIRPORT)
sumfun <- function(x){
  sum(x$delay_var, na.rm = TRUE)
}
count_dep_delays <- sapply(split_origin_airport, sumfun)
count_dep_delays_df <- as.data.frame(count_dep_delays)
count_dep_delays_df$code <- rownames(count_dep_delays_df)
airports$IATA_CODE <- as.character(airports$IATA_CODE)
airports <- left_join(airports, count_dep_delays_df, by = c("IATA_CODE"  = "code"))


#add delays as a percent of flights at the airport
#number of flights at the airport
count_flights <- sapply(split_origin_airport, nrow)
count_flights <- data.frame(count_flights)
count_flights$airportname <- row.names(count_flights)
airports <- left_join(airports, count_flights, by = c("IATA_CODE" = "airportname"))
airports$percent_delays <- airports$count_dep_delays/airports$count_flights

#add delays at each airport by airline to airports df
v1fun <- function(x){
  x$V1
}
origin_airport_airline_delays <- ddply(origins_and_departures, .(ORIGIN_AIRPORT, AIRLINE), sumfun)
airport_vs_airline_delays <- daply(origin_airport_airline_delays, .(ORIGIN_AIRPORT, AIRLINE), v1fun)
airport_vs_airline_delays <- as.data.frame(airport_vs_airline_delays)
airport_vs_airline_delays$names <- row.names(airport_vs_airline_delays)
airports <- left_join(airports, airport_vs_airline_delays, by = c("IATA_CODE" = "names"))

textlist <- ddply(airports, .(AIRPORT), 
                  function(x){
                    values <- c(x$AA, x$AS, x$B6, x$DL, x$EV, 
                                x$F9,x$HA, x$MQ, x$NK,x$OO,x$UA,
                                x$US,x$VX)
                    for(i in 1:length(values)){
                      if(is.na(values[i])) values[i] = 0
                    }
                    values <- as.character(values)
                    paste(x$AIRPORT,
                          "<br/>","United Air Lines Inc.", values[1], 
                          "<br/>","American Airlines Inc.", values[2], 
                          "<br/>", "US Airways Inc.", values[3], 
                          "<br/>","Frontier Airlines Inc.", values[4],
                          "<br/>","JetBlue Airways", values[5], 
                          "<br/>","Skywest Airlines Inc.", values[6],
                          "<br/>","Alaska Airlines Inc.", values[7], 
                          "<br/>", "Spirit Air Lines", values[8], 
                          "<br/>", "Southwest Airlines Co." , values[9], 
                          "<br/>","Delta Air Lines Inc.", values[10], 
                          "<br/>","Atlantic Southeast Airlines", values[11], 
                          "<br/>", "Hawaiian Airlines Inc.", values[12],
                          "<br/>", "American Eagle Airlines Inc.", values[13])
                  }
)

airports <- left_join(airports, textlist, by = c("AIRPORT" = "AIRPORT"))


saveRDS(airports, file = "airports.rds")

#airport names and airlines
airport_codes <- airports$IATA_CODE
airliners <- airlines$IATA_CODE
airliners_list <- as.list(airliners)
names(airliners_list) <- airlines$AIRLINE
#########################################################################

#Graph 2

#########################################################################
#pittsburgh flights data frames for departures and arrivals
pittsburgh_flights_origin <- flights[flights$ORIGIN_AIRPORT == 
                                       "PIT", c("DAY_OF_WEEK",
                                                "AIRLINE", 
                                                "ORIGIN_AIRPORT",
                                                "DESTINATION_AIRPORT")]
pittsburgh_flights_destination <- flights[flights$DESTINATION_AIRPORT == 
                                            "PIT",c("DAY_OF_WEEK","AIRLINE", 
                                                    "ORIGIN_AIRPORT", 
                                                    "DESTINATION_AIRPORT")]


####number of flights from and to pittsburgh
summary_pitt_origin <- pittsburgh_flights_origin %>%
  group_by(DESTINATION_AIRPORT) %>%
  dplyr::summarize(
    most_traveled = n()
  ) 

summary_pitt_dest <- pittsburgh_flights_destination %>%
  group_by(ORIGIN_AIRPORT) %>%
  dplyr::summarize(
    most_traveled = n()
  ) 

summary_pitt_origin <- as.data.frame(summary_pitt_origin)
summary_pitt_dest <- as.data.frame(summary_pitt_dest)


##### adding lat lon data to summary flights to and from pittsburgh
##adding lat lon of destinations to pittsburgh originating flights summary 
summary_pitt_origin <- left_join(summary_pitt_origin, 
                                 airports[,c("CITY", "LATITUDE",
                                             "LONGITUDE","AIRPORT",
                                             "IATA_CODE")], 
                                 by = c("DESTINATION_AIRPORT" =
                                          "IATA_CODE"))
names(summary_pitt_origin) <- c("DESTINATION_AIRPORT", "most_traveled", "d_CITY","d_LATITUDE",
                                "d_LONGITUDE", "d_Name")

#adding pittsburgh as the origin 
summary_pitt_origin$ORIGIN_AIRPORT <- rep("PIT",times = nrow(summary_pitt_origin))
##adding lat lon of pittsburgh to pittsburgh originating flights summary
summary_pitt_origin <- left_join(summary_pitt_origin, 
                                 airports[,c("CITY", "LATITUDE",
                                             "LONGITUDE","AIRPORT",
                                             "IATA_CODE")], 
                                 by = c("ORIGIN_AIRPORT" =
                                          "IATA_CODE"))
names(summary_pitt_origin) <- c("DESTINATION_AIRPORT", "most_traveled", "d_CITY","d_LATITUDE",
                                "d_LONGITUDE", "d_Airport", "ORIGIN_AIRPORT","o_CITY","o_LATITUDE",
                                "o_LONGITUDE", "o_Airport")


##adding lat lon of origins to pittsburgh destination flights summary
summary_pitt_dest <- left_join(summary_pitt_dest, 
                               airports[,c("CITY", "LATITUDE", 
                                           "LONGITUDE", "AIRPORT",
                                           "IATA_CODE")], 
                               by = c("ORIGIN_AIRPORT" = "IATA_CODE"))
names(summary_pitt_dest) <- c("ORIGIN_AIRPORT", "most_traveled",
                              "o_CITY","o_LATITUDE", 
                              "o_LONGITUDE", "o_AIRPORT")

#adding pittsburgh as destination to pittsburgh destination flights summary
summary_pitt_dest$DESTINATION_AIRPORT <- rep("PIT", nrow(summary_pitt_dest))
##adding lat lon of 
summary_pitt_dest <- left_join(summary_pitt_dest, 
                               airports[,c("CITY", "LATITUDE", 
                                           "LONGITUDE", "AIRPORT",
                                           "IATA_CODE")], 
                               by = c("DESTINATION_AIRPORT" = "IATA_CODE"))
names(summary_pitt_dest) <- c("ORIGIN_AIRPORT", "most_traveled",
                              "o_CITY", "o_LATITUDE", 
                              "o_LONGITUDE", "o_AIRPORT", "DESTINATION_AIRPORT",
                              "d_CITY","d_LATITUDE", 
                              "d_LONGITUDE", "d_AIRPORT")



# destination_arc <- list()
# for(i in 1:nrow(summary_pitt_dest)){
#   arc <- gcIntermediate( c(summary_pitt_dest[i, "o_LONGITUDE"], summary_pitt_dest[i, "o_LATITUDE"]),
#                          c(summary_pitt_dest[i, "d_LONGITUDE"], summary_pitt_dest[i, "d_LATITUDE"]),
#                                      n=1000, addStartEnd=TRUE, sp =TRUE )
#   destination_arc[[i]] <- arc
# }

origin_arc_PIT <- list()
for(i in 1:nrow(summary_pitt_origin)){
  arc <- gcIntermediate( c(summary_pitt_origin[i, "o_LONGITUDE"], summary_pitt_origin[i, "o_LATITUDE"]),
                         c(summary_pitt_origin[i, "d_LONGITUDE"], summary_pitt_origin[i, "d_LATITUDE"]),
                         n=1000, addStartEnd=TRUE, sp =TRUE )
  origin_arc_PIT[[i]] <- arc
}

saveRDS(summary_pitt_origin, "summary_pit_origin.rds")
###############################################################################ATL
#pittsburgh flights data frames for departures and arrivals
pittsburgh_flights_origin <- flights[flights$ORIGIN_AIRPORT == 
                                       "ATL", c("DAY_OF_WEEK",
                                                "AIRLINE", 
                                                "ORIGIN_AIRPORT",
                                                "DESTINATION_AIRPORT")]

####number of flights from and to pittsburgh
summary_pitt_origin <- pittsburgh_flights_origin %>%
  group_by(DESTINATION_AIRPORT) %>%
  dplyr::summarize(
    most_traveled = n()
  ) 


summary_pitt_origin <- as.data.frame(summary_pitt_origin)



##### adding lat lon data to summary flights to and from pittsburgh
##adding lat lon of destinations to pittsburgh originating flights summary 
summary_pitt_origin <- left_join(summary_pitt_origin, 
                                 airports[,c("CITY", "LATITUDE",
                                             "LONGITUDE","AIRPORT",
                                             "IATA_CODE")], 
                                 by = c("DESTINATION_AIRPORT" =
                                          "IATA_CODE"))
names(summary_pitt_origin) <- c("DESTINATION_AIRPORT", "most_traveled", "d_CITY","d_LATITUDE",
                                "d_LONGITUDE", "d_Name")

#adding pittsburgh as the origin 
summary_pitt_origin$ORIGIN_AIRPORT <- rep("ATL",times = nrow(summary_pitt_origin))
##adding lat lon of pittsburgh to pittsburgh originating flights summary
summary_pitt_origin <- left_join(summary_pitt_origin, 
                                 airports[,c("CITY", "LATITUDE",
                                             "LONGITUDE","AIRPORT",
                                             "IATA_CODE")], 
                                 by = c("ORIGIN_AIRPORT" =
                                          "IATA_CODE"))
names(summary_pitt_origin) <- c("DESTINATION_AIRPORT", "most_traveled", "d_CITY","d_LATITUDE",
                                "d_LONGITUDE", "d_Airport", "ORIGIN_AIRPORT","o_CITY","o_LATITUDE",
                                "o_LONGITUDE", "o_Airport")


summary_pitt_origin <- summary_pitt_origin[!is.na(summary_pitt_origin$d_LATITUDE),]

origin_arc_ATL <- list()
for(i in 1:nrow(summary_pitt_origin)){
  
  arc <- gcIntermediate( c(summary_pitt_origin[i, "o_LONGITUDE"], summary_pitt_origin[i, "o_LATITUDE"]),
                         c(summary_pitt_origin[i, "d_LONGITUDE"], summary_pitt_origin[i, "d_LATITUDE"]),
                         n=1000, addStartEnd=TRUE, sp =TRUE )
  origin_arc_ATL[[i]] <- arc
}
saveRDS(summary_pitt_origin, "summary_atl_origin.rds")
###############################################################################LAX
#pittsburgh flights data frames for departures and arrivals
pittsburgh_flights_origin <- flights[flights$ORIGIN_AIRPORT == 
                                       "LAX", c("DAY_OF_WEEK",
                                                "AIRLINE", 
                                                "ORIGIN_AIRPORT",
                                                "DESTINATION_AIRPORT")]

####number of flights from and to pittsburgh
summary_pitt_origin <- pittsburgh_flights_origin %>%
  group_by(DESTINATION_AIRPORT) %>%
  dplyr::summarize(
    most_traveled = n()
  ) 


summary_pitt_origin <- as.data.frame(summary_pitt_origin)



##### adding lat lon data to summary flights to and from pittsburgh
##adding lat lon of destinations to pittsburgh originating flights summary 
summary_pitt_origin <- left_join(summary_pitt_origin, 
                                 airports[,c("CITY", "LATITUDE",
                                             "LONGITUDE","AIRPORT",
                                             "IATA_CODE")], 
                                 by = c("DESTINATION_AIRPORT" =
                                          "IATA_CODE"))
names(summary_pitt_origin) <- c("DESTINATION_AIRPORT", "most_traveled", "d_CITY","d_LATITUDE",
                                "d_LONGITUDE", "d_Name")

#adding pittsburgh as the origin 
summary_pitt_origin$ORIGIN_AIRPORT <- rep("LAX",times = nrow(summary_pitt_origin))
##adding lat lon of pittsburgh to pittsburgh originating flights summary
summary_pitt_origin <- left_join(summary_pitt_origin, 
                                 airports[,c("CITY", "LATITUDE",
                                             "LONGITUDE","AIRPORT",
                                             "IATA_CODE")], 
                                 by = c("ORIGIN_AIRPORT" =
                                          "IATA_CODE"))
names(summary_pitt_origin) <- c("DESTINATION_AIRPORT", "most_traveled", "d_CITY","d_LATITUDE",
                                "d_LONGITUDE", "d_Airport", "ORIGIN_AIRPORT","o_CITY","o_LATITUDE",
                                "o_LONGITUDE", "o_Airport")




origin_arc_LAX <- list()
for(i in 1:nrow(summary_pitt_origin)){
  arc <- gcIntermediate( c(summary_pitt_origin[i, "o_LONGITUDE"], summary_pitt_origin[i, "o_LATITUDE"]),
                         c(summary_pitt_origin[i, "d_LONGITUDE"], summary_pitt_origin[i, "d_LATITUDE"]),
                         n=1000, addStartEnd=TRUE, sp =TRUE )
  origin_arc_LAX[[i]] <- arc
}

saveRDS(summary_pitt_origin, "summary_lax_origin.rds")

###############################################################################ORD
#pittsburgh flights data frames for departures and arrivals
pittsburgh_flights_origin <- flights[flights$ORIGIN_AIRPORT == 
                                       "ORD", c("DAY_OF_WEEK",
                                                "AIRLINE", 
                                                "ORIGIN_AIRPORT",
                                                "DESTINATION_AIRPORT")]

####number of flights from and to pittsburgh
summary_pitt_origin <- pittsburgh_flights_origin %>%
  group_by(DESTINATION_AIRPORT) %>%
  dplyr::summarize(
    most_traveled = n()
  ) 


summary_pitt_origin <- as.data.frame(summary_pitt_origin)



##### adding lat lon data to summary flights to and from pittsburgh
##adding lat lon of destinations to pittsburgh originating flights summary 
summary_pitt_origin <- left_join(summary_pitt_origin, 
                                 airports[,c("CITY", "LATITUDE",
                                             "LONGITUDE","AIRPORT",
                                             "IATA_CODE")], 
                                 by = c("DESTINATION_AIRPORT" =
                                          "IATA_CODE"))
names(summary_pitt_origin) <- c("DESTINATION_AIRPORT", "most_traveled", "d_CITY","d_LATITUDE",
                                "d_LONGITUDE", "d_Name")

#adding pittsburgh as the origin 
summary_pitt_origin$ORIGIN_AIRPORT <- rep("ORD",times = nrow(summary_pitt_origin))
##adding lat lon of pittsburgh to pittsburgh originating flights summary
summary_pitt_origin <- left_join(summary_pitt_origin, 
                                 airports[,c("CITY", "LATITUDE",
                                             "LONGITUDE","AIRPORT",
                                             "IATA_CODE")], 
                                 by = c("ORIGIN_AIRPORT" =
                                          "IATA_CODE"))
names(summary_pitt_origin) <- c("DESTINATION_AIRPORT", "most_traveled", "d_CITY","d_LATITUDE",
                                "d_LONGITUDE", "d_Airport", "ORIGIN_AIRPORT","o_CITY","o_LATITUDE",
                                "o_LONGITUDE", "o_Airport")




origin_arc_ORD <- list()
for(i in 1:nrow(summary_pitt_origin)){
  arc <- gcIntermediate( c(summary_pitt_origin[i, "o_LONGITUDE"], summary_pitt_origin[i, "o_LATITUDE"]),
                         c(summary_pitt_origin[i, "d_LONGITUDE"], summary_pitt_origin[i, "d_LATITUDE"]),
                         n=1000, addStartEnd=TRUE, sp =TRUE )
  origin_arc_ORD[[i]] <- arc
}

saveRDS(summary_pitt_origin, "summary_ord_origin.rds")
###############################################################################EWR
#pittsburgh flights data frames for departures and arrivals
pittsburgh_flights_origin <- flights[flights$ORIGIN_AIRPORT == 
                                       "EWR", c("DAY_OF_WEEK",
                                                "AIRLINE", 
                                                "ORIGIN_AIRPORT",
                                                "DESTINATION_AIRPORT")]

####number of flights from and to pittsburgh
summary_pitt_origin <- pittsburgh_flights_origin %>%
  group_by(DESTINATION_AIRPORT) %>%
  dplyr::summarize(
    most_traveled = n()
  ) 


summary_pitt_origin <- as.data.frame(summary_pitt_origin)



##### adding lat lon data to summary flights to and from pittsburgh
##adding lat lon of destinations to pittsburgh originating flights summary 
summary_pitt_origin <- left_join(summary_pitt_origin, 
                                 airports[,c("CITY", "LATITUDE",
                                             "LONGITUDE","AIRPORT",
                                             "IATA_CODE")], 
                                 by = c("DESTINATION_AIRPORT" =
                                          "IATA_CODE"))
names(summary_pitt_origin) <- c("DESTINATION_AIRPORT", "most_traveled", "d_CITY","d_LATITUDE",
                                "d_LONGITUDE", "d_Name")

#adding pittsburgh as the origin 
summary_pitt_origin$ORIGIN_AIRPORT <- rep("EWR",times = nrow(summary_pitt_origin))
##adding lat lon of pittsburgh to pittsburgh originating flights summary
summary_pitt_origin <- left_join(summary_pitt_origin, 
                                 airports[,c("CITY", "LATITUDE",
                                             "LONGITUDE","AIRPORT",
                                             "IATA_CODE")], 
                                 by = c("ORIGIN_AIRPORT" =
                                          "IATA_CODE"))
names(summary_pitt_origin) <- c("DESTINATION_AIRPORT", "most_traveled", "d_CITY","d_LATITUDE",
                                "d_LONGITUDE", "d_Airport", "ORIGIN_AIRPORT","o_CITY","o_LATITUDE",
                                "o_LONGITUDE", "o_Airport")




origin_arc_EWR <- list()
for(i in 1:nrow(summary_pitt_origin)){
  arc <- gcIntermediate( c(summary_pitt_origin[i, "o_LONGITUDE"], summary_pitt_origin[i, "o_LATITUDE"]),
                         c(summary_pitt_origin[i, "d_LONGITUDE"], summary_pitt_origin[i, "d_LATITUDE"]),
                         n=1000, addStartEnd=TRUE, sp =TRUE )
  origin_arc_EWR[[i]] <- arc
}

saveRDS(summary_pitt_origin, "summary_ewr_origin.rds")
###############################################################################DEN
#pittsburgh flights data frames for departures and arrivals
pittsburgh_flights_origin <- flights[flights$ORIGIN_AIRPORT == 
                                       "DEN", c("DAY_OF_WEEK",
                                                "AIRLINE", 
                                                "ORIGIN_AIRPORT",
                                                "DESTINATION_AIRPORT")]

####number of flights from and to pittsburgh
summary_pitt_origin <- pittsburgh_flights_origin %>%
  group_by(DESTINATION_AIRPORT) %>%
  dplyr::summarize(
    most_traveled = n()
  ) 


summary_pitt_origin <- as.data.frame(summary_pitt_origin)



##### adding lat lon data to summary flights to and from pittsburgh
##adding lat lon of destinations to pittsburgh originating flights summary 
summary_pitt_origin <- left_join(summary_pitt_origin, 
                                 airports[,c("CITY", "LATITUDE",
                                             "LONGITUDE","AIRPORT",
                                             "IATA_CODE")], 
                                 by = c("DESTINATION_AIRPORT" =
                                          "IATA_CODE"))
names(summary_pitt_origin) <- c("DESTINATION_AIRPORT", "most_traveled", "d_CITY","d_LATITUDE",
                                "d_LONGITUDE", "d_Name")

#adding pittsburgh as the origin 
summary_pitt_origin$ORIGIN_AIRPORT <- rep("DEN",times = nrow(summary_pitt_origin))
##adding lat lon of pittsburgh to pittsburgh originating flights summary
summary_pitt_origin <- left_join(summary_pitt_origin, 
                                 airports[,c("CITY", "LATITUDE",
                                             "LONGITUDE","AIRPORT",
                                             "IATA_CODE")], 
                                 by = c("ORIGIN_AIRPORT" =
                                          "IATA_CODE"))
names(summary_pitt_origin) <- c("DESTINATION_AIRPORT", "most_traveled", "d_CITY","d_LATITUDE",
                                "d_LONGITUDE", "d_Airport", "ORIGIN_AIRPORT","o_CITY","o_LATITUDE",
                                "o_LONGITUDE", "o_Airport")




origin_arc_DEN <- list()
for(i in 1:nrow(summary_pitt_origin)){
  arc <- gcIntermediate( c(summary_pitt_origin[i, "o_LONGITUDE"], summary_pitt_origin[i, "o_LATITUDE"]),
                         c(summary_pitt_origin[i, "d_LONGITUDE"], summary_pitt_origin[i, "d_LATITUDE"]),
                         n=1000, addStartEnd=TRUE, sp =TRUE )
  origin_arc_DEN[[i]] <- arc
}
saveRDS(summary_pitt_origin, "summary_den_origin.rds")


saveRDS(origin_arc_DEN, "origin_arc_DEN.rds")
saveRDS(origin_arc_EWR, "origin_arc_EWR.rds")
saveRDS(origin_arc_ORD, "origin_arc_ORD.rds")
saveRDS(origin_arc_LAX, "origin_arc_LAX.rds")
saveRDS(origin_arc_PIT, "origin_arc_PIT.rds")
saveRDS(origin_arc_ATL, "origin_arc_ATL.rds")



