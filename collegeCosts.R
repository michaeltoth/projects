library(xlsx)
library(ggplot2)

if (!file.exists("college-pricing.xlsx")) {
    fileUrl <- "https://secure-media.collegeboard.org/digitalServices/misc/trends/2014-trends-college-pricing-source-data-final.xlsx"
    download.file(fileUrl, destfile = "college-pricing.xlsx", method="curl")
    dateDownloaded <- date()
}

historical_cost <- read.xlsx("college-pricing.xlsx", sheetName = "Table 2", 
                             startRow = 3, endRow = 47, colIndex = c(2, 4, 8, 10))
colnames(historical_cost) <- c("TF.Private", "TF.Public", "TFRB.Private", "TFRB.Public")
historical_cost$Year <- seq(1971, 2014)

g <- ggplot(historical_cost, aes(Year)) +
    geom_line(aes(y = TF.Private, color = "Tuition & Fees - Private")) +
    geom_line(aes(y = TF.Public, color = "Tuition & Fees - Public")) +
    geom_line(aes(y = TFRB.Private, color = "Tuition Fees Room & Board - Private")) +
    geom_line(aes(y = TFRB.Public, color = "Tuition Fees Room & Board - Public"))


net_historical_cost <- read.xlsx("college-pricing.xlsx", sheetName = "Table 7", 
                             rowIndex = c(9:15, 17:22))
net_historical_cost2 <- data.frame(t(net_historical_cost[,2:26]))
colnames(net_historical_cost2) = c("PublishedTF.Public", "NetTF.Public", 
                                   "Aid.Public", "RoomBoard.Public", 
                                   "PublishedTFRB.Public", "NetTFRB.Public",
                                   "PublishedTF.Private", "NetTF.Private", 
                                   "Aid.Private", "RoomBoard.Private", 
                                   "PublishedTFRB.Private", "NetTFRB.Private")
rownames(net_historical_cost2) <- seq(1990, 2014)
net_historical_cost2$Year <- seq(1990, 2014)

g <- ggplot(net_historical_cost2, aes(Year)) +
    geom_line(aes(y = PublishedTFRB.Public, color = "Published TFRB - Public")) +
    geom_line(aes(y = NetTFRB.Public, color = "Net TFRB - Public")) +
    geom_line(aes(y = PublishedTFRB.Private, color = "Published TFRB - Private")) +
    geom_line(aes(y = NetTFRB.Private, color = "Net TFRB - Private"))
