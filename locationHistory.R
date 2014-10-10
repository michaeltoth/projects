setwd("/home/michael/Documents/Projects/location")

library(jsonlite)
library(ggplot2)
library(ggmap)

if(!exists("locationHistory")) {
    locationHistory <- fromJSON("LocationHistory.json")
}

d <- data.frame(lat=locationHistory$locations$latitudeE7,
                lon=locationHistory$locations$longitudeE7)

# Scaling data to conform to normal latitude/longitude
d <- d/10000000

if(!exists("NYC")) {
    NYC <- get_map("New York,United States", zoom=12)
}

if(!exists("US")) {
    US <- get_map("United States", zoom=4)
}

nyplot <- ggmap(NYC)
nyplot <- nyplot + geom_point(data=d, aes(lon, lat), alpha=0.07, color="red")
ggplot_build(nyplot)

usplot <- ggmap(US)
usplot <- usplot + geom_point(data=d, aes(lon, lat), alpha=0.07, color="red")
ggplot_build(usplot)

usplot <- ggmap(US)
usplot <- usplot + geom_path(data=d, aes(lon, lat), color="red")
ggplot_build(usplot)