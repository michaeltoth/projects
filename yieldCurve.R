library(YieldCurve)
library(animation)
library(lubridate)
library(XML)
library(mosaic)

# Getting yield curve data through 2012
data(FedYieldCurve)

# Pull 2013 and 2014 data separately from Google Docs (Source U.S. Treasury)
end_curve <- fetchGoogle("https://docs.google.com/spreadsheets/d/1Yc3Og9g0Ko_SMh6l0EEZcqIQ85godDxgpnkbfK_N-Gk/export?format=csv&id")

# Change formatting to xts and combine with FedYieldCurve data
end_curve$Date <- as.POSIXct(as.character(end_curve$Date), format="%m/%d/%Y")
end_curve_xts <- xts(end_curve[,2:9], order.by = end_curve$Date)
final_curves <- rbind(FedYieldCurve, end_curve_xts)

maturities <- c(3/12,6/12,1,2,3,5,7,10)
numloops <- nrow(final_curves)

# Set graphical parameters
par(bg="#DCE6EC", mar=c(5,4,3,2), xpd=FALSE, mgp=c(2.8,0.3,0.5), font.main=2,
    col.lab="black", col.axis="black", col.main="black", cex.axis=0.8, 
    cex.lab=0.8, cex.main=0.9, family="Helvetica", lend=1, 
    tck=0, las=1, bty="n")
opar <- par()

# Note: must install ImageMagick program for saveGIF function to work
saveGIF({
    # Create one panel for each date
    for (i in 1:numloops) {
        par(opar)
        plot(0, 0, type="n", xlab=expression(italic("Maturity")), 
             ylab=expression(italic("Rates")), ylim=c(0,15), xlim=c(0,10), 
             xaxt="n", yaxt="n")
        title(main=paste("Yield Curve: ", year(time(final_curves[i]))))
        grid(NA, NULL, col="white", lty="solid", lwd=1.5)
        axis(1, tick=FALSE, col.axis="black")
        axis(2, tick=FALSE, col.axis="black")
        
        # If yield curve is inverted, plot in red, else dark blue
        if (final_curves$R_3M[i] > final_curves$R_10Y[i]) {
            lines(maturities, final_curves[i,], lwd=3, col="red")
        }
        else {
            lines(maturities, final_curves[i,], lwd=3, col="#244A5D")
        }
    }
},
interval=.1,
movie.name="yieldOutput.gif", 
ani.width=400,
ani.height=400)
