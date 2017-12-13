#NEED
#load libraries
library(shiny)
library(shinydashboard)
library(devtools)
library(leaflet)
library(plotly)
library(plyr)
library(dplyr)
library(tidyverse)
library(forcats)
library(reshape2)
library(sp)
library(datasets)
library(data.table)
library(htmltools)
library(geosphere)
library(rgeos)
library(scales)
library(ggplot2)
library(ggmap)
library(maps)
library(rsconnect)
airports <- read.csv("airports.csv")
weighted_moving_average <- function(tt, time_series, ww, weights = NULL) {
  if (ww > length(time_series))  
    stop("Window width is greater than length of time series")
  
  if (is.null(weights))  weights <- rep(1/ww, ww)
  
  if (length(weights) != ww)  
    stop("Weights should have the same length as the window width")
  
  if (tt < ww)  return(NA)
  
  weights <- weights / sum(weights)
  return(sum(time_series[(tt-ww+1):tt] * weights, 
             na.rm = TRUE))
}

get_weighted_moving_averages <- function(time_series, ww, weights) {
  if (ww > length(time_series))  stop("Window width is greater than length of time series")
  
  if (is.null(weights))  weights <- rep(1/ww, ww)
  
  if (length(weights) != ww)  
    stop("Weights should have the same length as the window width")
  
  weights <- weights / sum(weights)
  return(sapply(1:length(time_series), 
                weighted_moving_average, 
                time_series = time_series, ww = ww, 
                weights = weights))
}

get_grpPoly <- function(group, ID, df) {
  Polygon(coordinates(df[which(df$region==ID & df$group==group), c("long", "lat")]),
          hole = FALSE)
}
get_spPoly <- function(ID, df) {
  Polygons(lapply(unique(df[df$region==ID,]$group), get_grpPoly, ID, df), ID)
}
sumfun <- function(x){
  sum(x$delay_var, na.rm = TRUE)
}
v1fun <- function(x){
  x$V1
}

#season isnt working?
us_data <- map_data("state")

#add theme
nikitagu_315_theme <-  theme_bw() + 
  theme(axis.text = element_text(size = 10, color = "royalblue2"), 
        text = element_text(size = 12, face = "bold", 
                            color = "royalblue4"))

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
flights_by_region_orig <- readRDS("flights_by_region_orig.rds")
flights_by_region_dest <- readRDS("flights_by_region_dest.rds")
airports2 <- readRDS("airports.rds")
origin_arc_DEN <- readRDS("origin_arc_DEN.rds")
origin_arc_EWR <- readRDS("origin_arc_EWR.rds")
origin_arc_ORD <- readRDS("origin_arc_ORD.rds")
origin_arc_LAX <- readRDS("origin_arc_LAX.rds")
origin_arc_PIT <- readRDS("origin_arc_PIT.rds")
origin_arc_ATL <- readRDS("origin_arc_ATL.rds")
summary_pit_origin <- readRDS("summary_pit_origin.rds")
summary_atl_origin <- readRDS("summary_atl_origin.rds")
summary_ord_origin <- readRDS("summary_ord_origin.rds")
summary_den_origin <- readRDS("summary_den_origin.rds")
summary_ewr_origin <- readRDS("summary_ewr_origin.rds")
summary_lax_origin <- readRDS("summary_lax_origin.rds")

