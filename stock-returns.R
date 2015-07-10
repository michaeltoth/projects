library(xlsx)
library(dplyr)
library(ggplot2)


# Read in Shiller historical market data ---------------------------------------
if (!file.exists('ie_data.xls')) {
    url = 'http://www.econ.yale.edu/~shiller/data/ie_data.xls'
    download.file(url, destfile = 'ie_data.xls')
}

classes = c("character", rep("numeric", 11))
data <- read.xlsx("ie_data.xls", sheetName = "Data", startRow = 8, 
                  as.data.frame = TRUE, colClasses = classes)

# Filter out relevant data:
data <- filter(data, !is.na(Date))
data <- select(data, Date, P, D)
colnames(data) <- c("Date", "Price", "Dividend")

# Column operations:
data$Date <- paste(data$Date, ".01", sep="")
data$Date <- as.Date(data$Date, format = "%Y.%m.%d")

