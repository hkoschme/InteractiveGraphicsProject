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
library(DT)
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




#airport names and airlines
airport_codes <- airports$IATA_CODE
airliners <- airlines$IATA_CODE
airliners_list <- as.list(airliners)
names(airliners_list) <- airlines$AIRLINE

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

origin_arc <- list()
for(i in 1:nrow(summary_pitt_origin)){
  arc <- gcIntermediate( c(summary_pitt_origin[i, "o_LONGITUDE"], summary_pitt_origin[i, "o_LATITUDE"]),
                         c(summary_pitt_origin[i, "d_LONGITUDE"], summary_pitt_origin[i, "d_LATITUDE"]),
                         n=1000, addStartEnd=TRUE, sp =TRUE )
  origin_arc[[i]] <- arc
}

