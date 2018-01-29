library(jsonlite)
library(ggplot2)
library(ggmap)
library(lubridate)

# Read in location history if we haven't already
locationHistory <- fromJSON("~/dev/projects/location-history-analysis/LocationHistory.json")

# Construct a data frame from the latitude and longitude history
locations <- locationHistory$locations

# converting time column from posix milliseconds into a readable time scale
locations$time <- as.POSIXct(as.numeric(locations$timestampMs)/1000, origin = "1970-01-01")
locations$year <- year(locations$time)
locations$month <- month(locations$time)
locations$date <- as.Date(locations$time, format = '%Y-%m-%d')

# Scaling data to conform to normal latitude/longitude
locations$lat <- locations$latitudeE7/10000000
locations$lon <- locations$longitudeE7/10000000

### 4 Maps: Neighborhood, NYC, Toledo, U.S. ###

Neighborhood <- get_map(location = c(lon = -73.9645139, lat = 40.7748793), zoom = 14)
hoodplot <- ggmap(Neighborhood)
hoodplot <- hoodplot + geom_path(data=locations, aes(lon, lat), alpha=0.15, color="red")
hoodplot

NYC <- get_map(location = c(lon = -73.9448158, lat = 40.7783566), zoom = 11)
#NYC <- get_map(location = c(-74.1117095947,40.5404169876,-73.6729431152,40.9498254118))
nyplot <- ggmap(NYC)
nyplot <- nyplot + geom_path(data=locations, aes(lon, lat), alpha=0.05, color="red")
#nyplot <- nyplot + facet_wrap(~ year) + geom_path(data=locations, aes(x = lon, y = lat), alpha=0.05)
#nyplot <- nyplot + geom_point(data=locations, aes(lon, lat), alpha=0.01, color="red")
nyplot

Maui <- get_map("Pukalani, HI", zoom = 10)
mauiplot <- ggmap(Maui)
mauiplot <- mauiplot + geom_path(data=locations, aes(lon, lat), alpha=0.15, color="red")
mauiplot

Rome <- get_map("Rome, Italy", zoom = 13)
romeplot <- ggmap(Rome)
romeplot <- romeplot + geom_path(data=locations, aes(lon, lat), alpha=0.75, color="red")
romeplot

US <- get_map("United States", zoom=4)
usplot <- ggmap(US)
usplot <- usplot + geom_path(data=locations, aes(lon, lat), color="red")
usplot
