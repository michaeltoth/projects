library(xlsx)
library(ggplot2)
library(reshape)

# Download data from College Board
if (!file.exists("college-pricing.xlsx")) {
    fileUrl <- "https://secure-media.collegeboard.org/digitalServices/misc/trends/2014-trends-college-pricing-source-data-final.xlsx"
    download.file(fileUrl, destfile = "college-pricing.xlsx", method="curl")
    dateDownloaded <- date()
}


#### Historical Cost ####

# Read in data from sheet 'Table 2', add column names and year variable
historical_cost <- read.xlsx("college-pricing.xlsx", sheetName = "Table 2", 
                             startRow = 3, endRow = 47, colIndex = c(2, 4, 8, 10))
colnames(historical_cost) <- c("TF.Private", "TF.Public", "TFRB.Private", "TFRB.Public")
historical_cost$Year <- seq(1971, 2014)

g <- ggplot(historical_cost, aes(Year)) +
    geom_line(aes(y = TF.Private, color = "Tuition & Fees - Private")) +
    geom_line(aes(y = TF.Public, color = "Tuition & Fees - Public")) +
    geom_line(aes(y = TFRB.Private, color = "Tuition Fees Room & Board - Private")) +
    geom_line(aes(y = TFRB.Public, color = "Tuition Fees Room & Board - Public"))


#### Net Historical Cost ####

# Read and transpose data from sheet 'Table 7', add column names and year variable
net_historical_cost <- read.xlsx("college-pricing.xlsx", sheetName = "Table 7", 
                             rowIndex = c(9:15, 17:22))
net_historical_cost <- data.frame(t(net_historical_cost[,2:26]))
colnames(net_historical_cost) = c("PublishedTF.Public", "NetTF.Public", 
                                   "Aid.Public", "RoomBoard.Public", 
                                   "PublishedTFRB.Public", "NetTFRB.Public",
                                   "PublishedTF.Private", "NetTF.Private", 
                                   "Aid.Private", "RoomBoard.Private", 
                                   "PublishedTFRB.Private", "NetTFRB.Private")
net_historical_cost$Year <- seq(1990, 2014)

g <- ggplot(net_historical_cost, aes(Year)) +
    geom_line(aes(y = PublishedTFRB.Public, color = "Published TFRB - Public")) +
    geom_line(aes(y = NetTFRB.Public, color = "Net TFRB - Public")) +
    geom_line(aes(y = PublishedTFRB.Private, color = "Published TFRB - Private")) +
    geom_line(aes(y = NetTFRB.Private, color = "Net TFRB - Private"))


#### Regional Cost ####

# Read data from sheet 'Table 4'.  Rename first two columns
regional_cost <- read.xlsx("college-pricing.xlsx", sheetName = "Table 4", 
                                 rowIndex = c(2:23), colIndex=c(1:27))
colnames(regional_cost)[1:2] <- c("Region","Type")

# Reformat "date" column names to 2-digit year names
#colnames(regional_cost) <- sub("X(\\d{2})\\.\\d{2}", "\\1", colnames(regional_cost))

# Subset the data set by public and private, only using 4-year school data
regional_cost_pub <- regional_cost[(regional_cost$Type) == "Public Four-Year",]
regional_cost_priv <- regional_cost[regional_cost$Type == "Private Nonprofit Four-Year",]

# Transpose the data sets and add column names for each region
regional_cost_pub <- data.frame(t(regional_cost_pub[,3:27]))
regional_cost_priv <- data.frame(t(regional_cost_priv[,3:27]))

cols <- c("National", "Middle States", "Midwest", "New England", "South", "Southwest", "West")
colnames(regional_cost_pub) <- cols
colnames(regional_cost_priv) <- cols

# Add year variable
regional_cost_pub$Year <- seq(1990,2014)
regional_cost_priv$Year <- seq(1990,2014)

# Use melt to get all data in one column for plotting by Region
regional_cost_pub <- melt(regional_cost_pub, id = "Year", variable_name = "Region")
regional_cost_priv <- melt(regional_cost_priv, id = "Year", variable_name = "Region")


g <- ggplot(regional_cost_pub, aes(Year, value)) + geom_line(aes(color = Region))
g

# Alt facet grid
g <- ggplot(regional_cost_pub, aes(Year, value)) + geom_line() + facet_grid(. ~ Region)
g


g <- ggplot(regional_cost_priv, aes(Year, value)) + geom_line(aes(color = Region))
g
