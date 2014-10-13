setwd("/home/michael/Documents/Projects/location")

library(jsonlite)
library(ggplot2)
library(ggmap)

# Read in location history if we haven't already
if(!exists("locationHistory")) {
    locationHistory <- fromJSON("LocationHistory.json")
}

# Construct a data frame from the latitude and longitude history
d <- data.frame(lat=locationHistory$locations$latitudeE7,
                lon=locationHistory$locations$longitudeE7)

# Scaling data to conform to normal latitude/longitude
d <- d/10000000

### 4 Maps: Neighborhood, NYC, Toledo, U.S. ###

if(!exists("Neighborhood")) {
    Neighborhood <- get_map(location = c(lon = -73.9645139, lat = 40.7748793), 
                            zoom = 14)
}

hoodplot <- ggmap(Neighborhood)
hoodplot <- hoodplot + geom_path(data=d, aes(lon, lat), alpha=0.15, color="red")

if(!exists("NYC")) {
    NYC <- get_map(location = c(lon = -73.9448158, lat = 40.7783566), zoom = 11)
}

nyplot <- ggmap(NYC)
nyplot <- nyplot + geom_path(data=d, aes(lon, lat), alpha=0.15, color="red")

if(!exists("Toledo")) {
    Toledo <- get_map("Toledo, OH", zoom = 12)
}

tolplot <- ggmap(Toledo)
tolplot <- tolplot + geom_path(data=d, aes(lon, lat), alpha=0.5, color="red")

if(!exists("US")) {
    US <- get_map("United States", zoom=4)
}

usplot <- ggmap(US)
usplot <- usplot + geom_path(data=d, aes(lon, lat), color="red")

#To display plot, use e.g. ggplot_build(usplot)