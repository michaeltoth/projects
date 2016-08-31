library(xlsx)
library(dplyr)
library(ggplot2)
library(quantmod)
library(PerformanceAnalytics)


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
colnames(data) <- c("date", "price", "dividend")

# Date formatting:
data$date <- sub(pattern = "\\.1$", replacement = ".10", x = data$date)
data$date <- paste(data$date, ".01", sep="")
data$date <- as.Date(data$date, format = "%Y.%m.%d")


# Operations -------------------------------------------------------------------

# NOTE: Review dates for data series

# Create time series from data:
ts <- xts(x = data$price, order.by = data$date)

# Calculate monthly return time series:
monthly <- monthlyReturn(ts)


# This function takes in a time series of returns, a start date, and an end date
# and returns the annualized return between those dates
calculate_annualized_return <- function(ts_returns, start, end) {
    # Convert time series data to df, filter, and reconvert to xts:
    df <- data.frame(date = index(ts_returns), ret = coredata(ts_returns))
    df_subset <- filter(df, date > start & date < end)
    ts_subset <- xts(df_subset$monthly.returns, order.by = df_subset$date)
    
    # Calculate annualized returns over defined period:
    Return.annualized(ts_subset)
}

calculate_average_horizon_return <- function(ts_returns, n_months) {
    # Initialize empty data frame
    output_df <- data.frame(return = numeric(), start = as.Date(character()),
                           end = as.Date(character()))
    total_trials = length(ts_returns) - n_months
    for (i in 1:total_trials) {
        start = index(ts_returns[i])
        end = index(ts_returns[i + n_months])
        return <- calculate_annualized_return(ts_returns, start, end)
        temp <- data.frame(return, start, end, row.names = NULL)
        output_df <- rbind(output_df, temp)
    }
    output_df
}

returns_30_year <- calculate_average_horizon_return(monthly, 12*30)
arrange(returns_30_year, return)
quantile(returns_30_year$return)
mean(returns_30_year$return)
sd(returns_30_year$return)


returns_10_year <- calculate_average_horizon_return(monthly, 12*10)
arrange(returns_10_year, return)
quantile(returns_10_year$return)
mean(returns_10_year$return)
sd(returns_10_year$return)
