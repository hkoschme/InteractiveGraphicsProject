#load libraries
library(shiny)
library(datasets)
library(ggmap)
library(leaflet)
library(plyr)
library(dplyr)

#load data
flights <- read.csv("flights.csv")
airports <- read.csv("airports.csv")
airlines <- read.csv("airlines.csv")



#adjustments to data

#remove small airports
flights$ORIGIN_AIRPORT <- as.character(flights$ORIGIN_AIRPORT)
flights <- flights[grep("[[:digit:]]+",flights$ORIGIN_AIRPORT, invert=TRUE), ]

#add binary variable containing whether there was a delay
delay_var <- flights$DEPARTURE_DELAY > 0
flights$delay_var <- delay_var

#add number of delays per airport to airports df
origins_and_departures <- flights[,c("ORIGIN_AIRPORT","delay_var", "AIRLINE")]
split_origin_airport <- split(origins_and_departures, origins_and_departures$ORIGIN_AIRPORT)
sumfun <- function(x){
  sum(x$delay_var, na.rm = TRUE)
}
count_dep_delays <- sapply(split_origin_airport, sumfun)
count_dep_delays_df <- as.data.frame(count_dep_delays)
count_dep_delays_df$code <- rownames(count_dep_delays_df)
airports$IATA_CODE <- as.character(airports$IATA_CODE)
airports <- left_join(airports, count_dep_delays_df, by = c("IATA_CODE"  = "code"))

#add delays at each airport by airline to airports df
v1fun <- function(x){
  x$V1
}
origin_airport_airline_delays <- ddply(origins_and_departures, .(ORIGIN_AIRPORT, AIRLINE), sumfun)
airport_vs_airline_delays <- daply(origin_airport_airline_delays, .(ORIGIN_AIRPORT, AIRLINE), v1fun)
airport_vs_airline_delays <- as.data.frame(airport_vs_airline_delays)
airport_vs_airline_delays$names <- row.names(airport_vs_airline_delays)
airports <- left_join(airports, airport_vs_airline_delays, by = c("IATA_CODE" = "names"))


