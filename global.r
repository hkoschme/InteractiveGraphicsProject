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

##############adjustments to data

#converting to characters from factors
# flights$ORIGIN_AIRPORT <- as.character(flights$ORIGIN_AIRPORT)
# flights$DESTINATION_AIRPORT <- as.character(flights$DESTINATION_AIRPORT)
# airlines$IATA_CODE <- as.character(airlines$IATA_CODE)


################# NIKITA NIKITA NIKITA
#add binary variable containing whether there was a delay
flights$dept_delay_var <- ifelse((flights$DEPARTURE_DELAY > 0 | is.na(flights$DEPARTURE_DELAY)), T, F)
flights$arr_delay_var <- ifelse((flights$ARRIVAL_DELAY > 0 | is.na(flights$ARRIVAL_DELAY)), T, F)

#some of our airports (the small ones) have numbers for IATA_CODE
#read the dataset linking IATA_CODE numbers with airport names & locations
airport_usdot <- read_csv("airport_usdot.csv")
#edit the dataset and rearrange columns to bind this dataset with the old airports df
for(i in 1:nrow(airport_usdot)) {
  airport_usdot$Name[i] <- substr(airport_usdot$Name[i], 1,
                                  nchar(airport_usdot$Name[i])-1)
}
airport_usdot_US <-
  airport_usdot[nchar(airport_usdot$State_Country) == 2,]
airport_usdot_US <- 
  data.frame(IATA_CODE = as.factor(airport_usdot_US$Code),
             AIRPORT = airport_usdot_US$Name,
             CITY = NA,
             STATE = airport_usdot_US$State_Country,
             COUNTRY = "USA",
             LATITUDE = NA,
             LONGITUDE = NA)
airports_complete <- rbind(airports, airport_usdot_US)
#now we have finished combining the airports datasets


#add region to dataset
airports_complete$REGION <- state.region[match(airports_complete$STATE,state.abb)]
#remove datasets with missing regions (Puerto Rico) and rename to airports dataset
airports <- as.data.frame(airports_complete[!is.na(airports_complete$REGION),])


#combine airlines with flights dataset & rename new column
flights <- left_join(flights, airlines, by = c("AIRLINE" = "IATA_CODE"))
colnames(flights)[which(colnames(flights) == "AIRLINE.y")] <- "AIRLINE_FULL"

################ SIDENOTE
#Jin uses a separate dataset - flights_complete
flights_complete <- flights %>%
  left_join(airports_complete, by = c("ORIGIN_AIRPORT" = "IATA_CODE")) %>%
  select(YEAR, MONTH, DAY, AIRLINE, AIRLINE_FULL, DEPARTURE_DELAY, DISTANCE, ARRIVAL_DELAY, 
         DIVERTED, CANCELLED, WEATHER_DELAY, DESTINATION_AIRPORT, STATE, 
         LATITUDE, LONGITUDE) %>%
  rename(replace = c("LATITUDE" = "O_LAT", "LONGITUDE" = "O_LON", "STATE" = "O_STATE")) %>% 
  left_join(airports_complete, by = c("DESTINATION_AIRPORT" = "IATA_CODE")) %>%
  select(YEAR, MONTH, DAY, AIRLINE, AIRLINE_FULL, DEPARTURE_DELAY, DISTANCE, ARRIVAL_DELAY, 
         DIVERTED, CANCELLED, WEATHER_DELAY, O_LAT, O_LON, O_STATE, STATE, 
         LATITUDE, LONGITUDE) %>%
  rename(replace = c("LATITUDE" = "D_LAT", "LONGITUDE" = "D_LON", "STATE" = "D_STATE"))

flights_complete <- flights_complete %>% 
  mutate(DATE = as.Date(with(flights_complete, 
                             paste(YEAR, MONTH, DAY, sep = "-")),
                        format = "%Y-%m-%d"))
#done with editing Jin's code in for this part
#################### SIDENOTE OVER

#combine airports with flights dataset
#do this for origin/departure airport, then rename
flights <- left_join(flights, airports, by = c("ORIGIN_AIRPORT" = "IATA_CODE"))
flights <- setnames(flights, old = c("AIRPORT", "CITY", "STATE", "COUNTRY", "LATITUDE", "LONGITUDE", "REGION"), new = c("ORIGIN_AIRPORT_FULL", "ORIGIN_CITY", "ORIGIN_STATE", "ORIGIN_COUNTRY", "ORIGIN_LATITUDE", "ORIGIN_LONGITUDE", "ORIGIN_REGION"))
#then do this for arrival/destination airport & rename
flights <- left_join(flights, airports, by = c("DESTINATION_AIRPORT" = "IATA_CODE"))
flights <- setnames(flights, old = c("AIRPORT", "CITY", "STATE", "COUNTRY", "LATITUDE", "LONGITUDE", "REGION"), new = c("DEST_AIRPORT_FULL", "DEST_CITY", "DEST_STATE", "DEST_COUNTRY", "DEST_LATITUDE", "DEST_LONGITUDE", "DEST_REGION"))

#remove international
flights <- flights[!(is.na(flights$ORIGIN_REGION)),]
flights <- flights[!(is.na(flights$DEST_REGION)),]

#add column for month names
flights$MONTH_FULL <- month.name[flights$MONTH]
#flights percent delay
flights_percent_delay <- flights %>% 
  select(AIRLINE_FULL, MONTH, MONTH_FULL, arr_delay_var, dept_delay_var) %>%
  group_by(AIRLINE_FULL, MONTH_FULL, MONTH) %>%
  dplyr::summarize(COUNT = n(), ARR_DELAY = sum(arr_delay_var), DEPT_DELAY = sum(dept_delay_var))

######################## SIDENOTE 
#add in Jin's code again

us_data <- map_data("state")

dep_by_state <- flights_complete %>% 
  group_by(O_STATE) %>%
  dplyr::summarize(COUNT = n())

arr_by_state <- flights_complete %>%
  group_by(D_STATE) %>%
  dplyr::summarize(COUNT = n())
dep_by_state <- dep_by_state %>% 
  mutate(O_STATE_NAME = tolower(state.name[match(O_STATE, state.abb)]))
arr_by_state <- arr_by_state %>% 
  mutate(D_STATE_NAME = tolower(state.name[match(D_STATE, state.abb)]))
us_data_dep <- us_data %>%
  left_join(dep_by_state, 
            by = c("region" = "O_STATE_NAME"))
us_data_arr <- us_data %>%
  left_join(arr_by_state, 
            by = c("region" = "D_STATE_NAME"))

dep_delay_ts <- flights_complete %>% 
  group_by(DATE, AIRLINE_FULL) %>% 
  filter(DEPARTURE_DELAY > 0) %>%
  dplyr::summarize(NumberofDelays = n(),
                   AverageDelayTime = mean(DEPARTURE_DELAY, 
                                           na.rm = TRUE)
  )
arr_delay_ts <- flights_complete %>% 
  group_by(DATE, AIRLINE_FULL) %>% 
  filter(ARRIVAL_DELAY > 0) %>%
  dplyr::summarize(NumberofDelays = n(),
                   AverageDelayTime = mean(ARRIVAL_DELAY, 
                                           na.rm = TRUE)
  )

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

# Code for Number of Departures/Arrivals Choropleth
us_data_dep_by_region <- us_data_dep %>%
  group_by(region) %>%  dplyr::summarize(O_STATE = unique(O_STATE),
                                         COUNT = unique(COUNT)) %>%
  mutate(PERCENT = COUNT/sum(COUNT, na.rm = TRUE)*100)
us_data_arr_by_region <- us_data_arr %>%
  group_by(region) %>%  dplyr::summarize(D_STATE = unique(D_STATE),
                                         COUNT = unique(COUNT)) %>%
  mutate(PERCENT = COUNT/sum(COUNT, na.rm = TRUE)*100)
flights_complete <- flights_complete %>% 
  mutate(SEASON = ifelse(MONTH %in% c(3, 4, 5), "Spring", ifelse(MONTH %in% c(6, 7, 8), "Summer", ifelse(MONTH %in% c(9, 10, 11), "Fall", "Winter"))))
dep_by_state_season <- flights_complete %>% 
  group_by(O_STATE, SEASON) %>%
  dplyr::summarize(COUNT = n())
arr_by_state_season <- flights_complete %>%
  group_by(D_STATE, SEASON) %>%
  dplyr::summarize(COUNT = n())
dep_by_state_season <- dep_by_state_season %>% 
  mutate(O_STATE_NAME = tolower(state.name[match(O_STATE, state.abb)]))
arr_by_state_season <- arr_by_state_season %>% 
  mutate(D_STATE_NAME = tolower(state.name[match(D_STATE, state.abb)]))

dep_by_state_season$O_STATE <- as.character(dep_by_state_season$O_STATE)
arr_by_state_season$O_STATE <- as.character(arr_by_state_season$D_STATE)

dep_by_state_season <- data_frame(O_STATE = c(dep_by_state_season$O_STATE, "DE", "DE"),
                                  SEASON = c(dep_by_state_season$SEASON, "Summer", "Fall"),
                                  COUNT = c(dep_by_state_season$COUNT, 0, 0),
                                  O_STATE_NAME = c(dep_by_state_season$O_STATE_NAME, "delaware", "delaware"))

arr_by_state_season <- data_frame(D_STATE = c(arr_by_state_season$D_STATE, "DE", "DE"),
                                  SEASON = c(arr_by_state_season$SEASON, "Summer", "Fall"),
                                  COUNT = c(arr_by_state_season$COUNT, 0, 0),
                                  D_STATE_NAME = c(arr_by_state_season$D_STATE_NAME, "delaware", "delaware"))

get_grpPoly <- function(group, ID, df) {
  Polygon(coordinates(df[which(df$region==ID & df$group==group), c("long", "lat")]),
          hole = FALSE)
}
get_spPoly <- function(ID, df) {
  Polygons(lapply(unique(df[df$region==ID,]$group), get_grpPoly, ID, df), ID)
}
############################ SIDENOTE OVER

############################ SIDENOTE: ADD KEVIN'S CODE
state_borders <- ggplot2::map_data("state")

state_data <- data_frame(state.abb, state.name = tolower(state.name))
new_data <- flights %>% 
  group_by(ORIGIN_STATE, AIRLINE_FULL) %>% dplyr::summarize(Count = n(), 
                                                            delay = mean(DEPARTURE_DELAY, 
                                                                         na.rm = TRUE))
new_data <- left_join(new_data, state_data, by = c("ORIGIN_STATE" = "state.abb"))
state_borders <- left_join(state_borders, new_data, by = c("region" = "state.name"))
state_borders <- state_borders[!is.na(state_borders$AIRLINE_FULL),]

state_borders_airline <- ggplot2::map_data("state")
state_data_airline <- c()
for(i in 1:length(unique(flights$AIRLINE_FULL))) {
  d1 <- data_frame(state.abb, state.name = tolower(state.name),  AIRLINE_FULL = unique(flights$AIRLINE_FULL)[i])
  state_data_airline <- rbind(state_data_airline, d1)
}
state_data_airline <- as_data_frame(state_data_airline)
new_data_airline <- flights %>% 
  group_by(ORIGIN_STATE, AIRLINE_FULL) %>% dplyr::summarize(Count = n(), 
                                                            delay = mean(DEPARTURE_DELAY, 
                                                                         na.rm = TRUE))
new_data_airline <- left_join(state_data_airline, new_data_airline, by = c("state.abb" = "ORIGIN_STATE",
                                                                           "AIRLINE_FULL" = "AIRLINE_FULL"))
state_borders_airline <- left_join(state_borders_airline, new_data_airline, by = c("region" = "state.name"))


#NIKITA: add code for 4th plot
#create new df getting average delay by airline, day of week, region
flights_day_region_airline <- flights %>% 
  group_by(AIRLINE_FULL, DAY_OF_WEEK, ORIGIN_REGION) %>% 
  dplyr::summarize(Count = n(), DELAY = mean(DEPARTURE_DELAY, na.rm = TRUE))
#change number to factor
flights_day_region_airline$DAY_OF_WEEK <- as.factor(flights_day_region_airline$DAY_OF_WEEK)
week_day_match <- cbind(c(1, 2, 3, 4, 5, 6, 7), 
                        c("Monday", "Tuesday", "Wednesday", "Thursday", 
                          "Friday", "Saturday", "Sunday"))
#match week day number to week day name
flights_day_region_airline$DAY_OF_WEEK_FULL <- 
  week_day_match[flights_day_region_airline$DAY_OF_WEEK, 2]

############NIKITA NIKITA NIKITA NIKITA OVER #################


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

