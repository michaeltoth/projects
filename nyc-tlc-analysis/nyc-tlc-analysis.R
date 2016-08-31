library(RCurl)
library(XML)
library(ggplot2)
library(dplyr)
# Generate List of Links:
setwd('~/Github/nyc-tlc-analysis/')

input <- getURL('http://www.nyc.gov/html/tlc/html/about/trip_record_data.shtml')
doc <- htmlParse(input)

files_to_pull <- xpathSApply(doc, "//tbody/tr/td/a", xmlGetAttr, "href") %>%
  grep(pattern = ".csv", value = TRUE)

# Download all files. WARNING: Large
for(file in files_to_pull) {
  if(!file.exists(file)) {
    filename = tail(strsplit(file, split = "/")[[1]], n = 1)
    download.file(file, filename, method = 'curl')
  }  
}

columns <- c(rep("NULL", 4), rep("numeric", 4), rep("NULL", 13))
    
# Read in and combine files:
green_201401 <- read.csv('green_tripdata_2014-01.csv', header = TRUE, colClasses = columns)
green_201402 <- read.csv('green_tripdata_2014-02.csv', header = TRUE, colClasses = columns)
green_201403 <- read.csv('green_tripdata_2014-03.csv', header = TRUE, colClasses = columns)
green_201404 <- read.csv('green_tripdata_2014-04.csv', header = TRUE, colClasses = columns)
green_201405 <- read.csv('green_tripdata_2014-05.csv', header = TRUE, colClasses = columns)
green_201406 <- read.csv('green_tripdata_2014-06.csv', header = TRUE, colClasses = columns)

green_2014_h1 <- rbind(green_201401, green_201402, green_201403, green_201404, green_201405, green_201406)

green_2014_h1 <- filter(green_2014_h1, Dropoff_longitude != 0, Dropoff_latitude != 0, Pickup_longitude != 0, Pickup_latitude != 0)

# Bucket by 4-decimal dropoff lat/lon
green_2014_h1_bucketed <- round(green_2014_h1, 6)
green_2014_h1_bucketed <- green_2014_h1_bucketed %>% group_by(Dropoff_longitude, Dropoff_latitude) %>% summarize(count = n())

ggplot(green_2014_h1_bucketed) +
    geom_point(aes(x = Dropoff_longitude, y = Dropoff_latitude, alpha = log(count + 1, base = 1000)), size = 0.1, color = 'white') +
    ylim(40.5, 41.05) +
    xlim(-74.1, -73.7) +
    theme(axis.line = element_blank(), 
          axis.text.x = element_blank(), 
          axis.text.y = element_blank(), 
          axis.ticks = element_blank(), 
          axis.title.x = element_blank(), 
          axis.title.y = element_blank(), 
          panel.background = element_rect(fill = 'black'), 
          plot.background = element_rect(fill = 'black'), 
          panel.border = element_blank(), 
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(), 
          plot.background = element_blank())






# Dropoffs:
ggplot(green_2014_h1) +
  geom_point(aes(x = Dropoff_longitude, y = Dropoff_latitude), , alpha = 0.1, size = 0.1, color = 'black') +
  ylim(40.5, 41.05) +
  xlim(-74.1, -73.7) +
  theme(axis.line = element_blank(), 
        axis.text.x = element_blank(), 
        axis.text.y = element_blank(), 
        axis.ticks = element_blank(), 
        axis.title.x = element_blank(), 
        axis.title.y = element_blank(), 
        panel.background = element_rect(fill = 'tan'), 
        plot.background = element_rect(fill = 'tan'), 
        panel.border = element_blank(), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        plot.background = element_blank())


# Pickups:
ggplot(green_2014_h1) +
  geom_point(aes(x = Pickup_longitude, y = Pickup_latitude), size = 0.1, color = 'white') +
  ylim(40.5, 41.05) +
  xlim(-74.1, -73.7) +
  theme(axis.line = element_blank(), 
        axis.text.x = element_blank(), 
        axis.text.y = element_blank(), 
        axis.ticks = element_blank(), 
        axis.title.x = element_blank(), 
        axis.title.y = element_blank(), 
        panel.background = element_rect(fill = 'black'), 
        plot.background = element_rect(fill = 'black'), 
        panel.border = element_blank(), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        plot.background = element_blank())
