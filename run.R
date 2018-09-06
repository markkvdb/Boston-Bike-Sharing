# Author:   markvanderbroek@gmail.com
# Date:     06-09-2018
# Purpose:  Exploratory analysis of the bike sharing system of Boston

# Load libraries
library(data.table)
library(tidyverse)
library(leaflet)
library(leaflet.extras)
library(geosphere)
library(lubridate)
library(gridExtra)
library(grid)
library(psych)
library(car)

# Load the datasets
trip_data = as_tibble(fread("Data/201807-bluebikes-tripdata.csv")) #faster
station_data = read_csv("Data/Hubway_Stations_as_of_July_2017.csv")


#### Exploration and Cleaning ####

# Create map showing all the dock stations in Boston
dock_map = station_data %>%
  leaflet() %>%
  setView(lng=-71.0589, lat=42.3601, zoom=13) %>%
  addTiles() %>%
  addMarkers(lng=station_data$Longitude, lat=station_data$Latitude,
             popup=station_data$Station)

print(dock_map)

# Lets discover the trip data
glimpse(trip_data)

# Recode the gender, age and tripduration
trip_data = trip_data %>%
  mutate(minutes = round(tripduration / 60),
         age = 2018 - `birth year`,
         gender = case_when(
           gender == 0 ~ "male",
           gender == 1 ~ "female",
           gender == 2 ~ "not specified"
         ))

# Start and stop times are now character strings. We turn them into dates.
trip_data = trip_data %>%
  mutate(starttime = ymd_hms(starttime),
         stoptime = ymd_hms(stoptime)) %>%
  mutate_at(vars(starttime), funs("start_date" = date(.))) %>%
  mutate_at(vars(stoptime), funs("stop_date" = date(.)))

# Extract week, weekday and hour
trip_data = trip_data %>%
  mutate(day = day(starttime),
         weekday = wday(starttime, label=TRUE),
         hour = hour(starttime))

# Before we continue, lets first clean up the names of the columns. I prefer
# the lowercase syntax. We also drop the variables which are not relevant any
# longer
trip_data = trip_data %>%
  select(trip_duration=tripduration, start_time=starttime, stop_time=stoptime, 
         start_date, stop_date, start_lat=`start station latitude`, 
         start_long=`start station longitude`, stop_lat=`end station latitude`, 
         stop_long=`end station longitude`, start_id=`start station id`, 
         stop_id=`end station id`, bike_id=bikeid, gender, minutes, age, day, 
         weekday, hour) %>%
  mutate(gender = as_factor(gender),
         weekday = as_factor(weekday))

# Calculate distance using the haversine formula
haversine_dist = function(long1, lat1, long2, lat2) {
  R = 6371
  radian =  pi / 180
  a1 = lat1 * radian
  a2 = long1 * radian
  b1 = lat2 * radian
  b2 = long2 * radian
  diff_long = b2 - a2
  diff_lat = b1 - a1
  a = (sin(diff_lat/2))^2 + cos(a1) * cos(b1) * (sin(diff_long/2))^2
  c = 2 * atan2(sqrt(a), sqrt(1 - a))
  d = R * c
  return(d)
}

# Add distance to data. Note that some return their bikes to the same station,
# so their distance will be zero.
trip_data = trip_data %>%
  mutate(distance = haversine_dist(start_long, start_lat, stop_long, stop_lat))

# To get initial insights in the data we construct a scatterplot matrix for
# selected variables. This might hint to errors in the data.
# scatterplot.matrix(~trip_duration+start_date+age+weekday, data=trip_data)
summary(trip_data)

# Some ages are above 99, which is unlikely so we discard them
trip_data = trip_data %>%
  filter(age <= 99)

#### Exploration ####

