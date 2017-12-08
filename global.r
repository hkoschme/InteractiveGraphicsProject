#load libraries

library(datasets)
library(ggmap)
library(leaflet)
library(shiny)
library(plyr)

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

