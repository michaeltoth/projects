library(YieldCurve)
library(animation)
library(lubridate)
library(XML)

# Getting yield curve data through 2012
data(FedYieldCurve)

# Pull 2013 and 2014 yield curve data separately from the treasury:
url_2013 <- "http://data.treasury.gov/feed.svc/DailyTreasuryYieldCurveRateData?$filter=year(NEW_DATE)%20eq%202013"
curve_2013 <- xmlTreeParse(url_2013,useInternal=TRUE)
url_2014 <- "http://data.treasury.gov/feed.svc/DailyTreasuryYieldCurveRateData?$filter=year(NEW_DATE)%20eq%202014"
curve_2014 <- xmlTreeParse(url_2014,useInternal=TRUE)
root_2013 <- xmlRoot(curve_2013)
root_2014 <- xmlRoot(curve_2014)

maturities <- c(3/12,6/12,1,2,3,5,7,10)
numloops <- nrow(FedYieldCurve)

# Save current graphical parameters
par(bg="#DCE6EC", mar=c(5,4,3,2), xpd=FALSE, mgp=c(2.8,0.3,0.5), font.main=2,
    col.lab="black", col.axis="black", col.main="black", cex.axis=0.8, 
    cex.lab=0.8, family="Helvetica", lend=1, 
    tck=0, las=1, bty="n")
opar <- par()

plot(0,0,type="n",lwd=3, col="black", xlab="Maturity", ylab="Rates",ylim=c(0,15), xlim = c(0,10), xaxt="n", yaxt="n")
title(main=paste("Yield Curve", year(time(FedYieldCurve[i]))))
grid(NA, NULL, col="white", lty="solid", lwd=1.5)
lines(maturities, FedYieldCurve[50,], lwd=3, col="#244A5D")
axis(1, tick=FALSE, col.axis="black")
axis(2, tick=FALSE, col.axis="black")


# Note: must install ImageMagick program for saveGIF function to work
saveGIF({
    for (i in 1:numloops) {
        if (FedYieldCurve$R_3M[i] > FedYieldCurve$R_10Y[i]) {
            par(opar)
            plot(0, 0, type="n", xlab=expression(italic("Maturity")), 
                 ylab=expression(italic("Rates")), ylim=c(0,15), xlim=c(0,10), 
                 xaxt="n", yaxt="n")
            title(main=paste("Yield Curve: ", year(time(FedYieldCurve[i]))))
            grid(NA, NULL, col="white", lty="solid", lwd=1.5)
            lines(maturities, FedYieldCurve[i,], lwd=3, col="red")
            axis(1, tick=FALSE, col.axis="black")
            axis(2, tick=FALSE, col.axis="black")
        }
        else {
            par(opar)
            plot(0, 0, type="n", xlab=expression(italic("Maturity")), 
                 ylab=expression(italic("Rates")), ylim=c(0,15), xlim=c(0,10), 
                 xaxt="n", yaxt="n")
            title(main=paste("Yield Curve: ", year(time(FedYieldCurve[i]))))
            grid(NA, NULL, col="white", lty="solid", lwd=1.5)
            lines(maturities, FedYieldCurve[i,], lwd=3, col="#244A5D")
            axis(1, tick=FALSE, col.axis="black")
            axis(2, tick=FALSE, col.axis="black")
        }
    }
},interval=.1,movie.name="evolution.gif", ani.width=400,ani.height=400)
