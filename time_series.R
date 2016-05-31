library(data.table)
library(TTR)
library(forecast)

# Read data and subsample one adzone data
data <- read.csv(file='cpm_ecpm_timeseries_all.dat', header=F, sep='\001', encoding='utf-8', colClasses=c("character", "character", "numeric", "numeric"))
subdata <- data.table(data[data$V2=='22212944', c(1,3)])
setnames(subdata, names(subdata), c("date", "cpm"))

# compute the quantile of adzone in each date
subdata.quant <- subdata[, quantile(sort(as.numeric(cpm)), 0.8), by=date]
date.order <- order(subdata.quant$date)
subdata.quant <- subdata.quant[date.order]
upper.bound <- length(date.order)

# timeseries analysis
quant.array <- subdata.quant$V1[1:(upper.bound)]
quant.array <- quant.array * 1000
quant.ts <- ts(quant.array[1:(length(quant.array))])
quant.ts.sma <- SMA(quant.ts, n=15)
quant.ts.hw <- HoltWinters(quant.ts, beta=1.0, gamma=F)
par(mfrow=c(2,1))
plot(quant.ts.hw, intervals=T, xaxt='n', ylab="CPM", main="Timeseries Analysis of Daily CPM Quantile (80%) - 无线首焦2")

# forecasting
lower.80 <- rep(0, 10)
lower.95 <- rep(0, 10)
upper.80 <- rep(0, 10)
upper.95 <- rep(0, 10)
ts.mean <- rep(0, 10)
ts.len <- length(date.order)
for (step in seq(ts.len-10,0,-1)) {
    quant.array <- subdata.quant$V1[1:(upper.bound-step)]
    quant.array <- quant.array * 1000
    quant.ts <- ts(quant.array[1:(length(quant.array))])
    quant.ts.hw <- HoltWinters(quant.ts, gamma=F)
    quant.ts.fc <- forecast.HoltWinters(quant.ts.hw, h=1)
    lower.80 <- c(lower.80, quant.ts.fc$lower[1])
    lower.95 <- c(lower.95, quant.ts.fc$lower[2])
    upper.80 <- c(upper.80, quant.ts.fc$upper[1])
    upper.95 <- c(upper.95, quant.ts.fc$upper[2])
    ts.mean <- c(ts.mean, quant.ts.fc$mean)
}
max.cpm <- max(subdata.quant$V1) * 1000
plot(quant.ts, ylim=c(0, max.cpm+5), xlim=c(0,ts.len), lwd=3, xaxt='n', xlab="", ylab="CPM", xaxs='i', yaxs='i')
tick.index = seq(1, length(quant.ts))
Axis(side=1, at=tick.index, labels=subdata.quant$date[tick.index], las=2, cex.axis=0.7, cex.lab=0.7)
grid(nx=ts.len, ny=100)

par(new=T)
plot(ts(ts.mean), ylim=c(0, max.cpm+5), xlim=c(0,ts.len), lty=3, col="gold", lwd=4, ylab="", xlab="", xaxt='n', xaxs='i', yaxs='i')

par(new=T)
plot(ts(lower.80), ylim=c(0, max.cpm+5), xlim=c(0,ts.len), lty=3, col="blue", lwd=2, ylab="", xlab="", xaxt='n', xaxs='i', yaxs='i')

par(new=T)
plot(ts(upper.80), ylim=c(0, max.cpm+5), xlim=c(0,ts.len), lty=3, col="blue", lwd=2, ylab="", xlab="", xaxt='n', xaxs='i', yaxs='i')

par(new=T)
plot(ts(lower.95), ylim=c(0, max.cpm+5), xlim=c(0,ts.len), lty=3, col="red", lwd=2, ylab="", xlab="", xaxt='n', xaxs='i', yaxs='i')

par(new=T)
plot(ts(upper.95), ylim=c(0, max.cpm+5), xlim=c(0,ts.len), lty=3, col="red", lwd=2, ylab="", xlab="", xaxt='n', xaxs='i', yaxs='i')

cpmArray <- read.csv(file='CpmTimeseriesForecast.dat', header=F, encoding='utf-8', sep=",", stringsAsFactors = F, colClasses=c("numeric", "numeric"))
par(new=T)
plot(ts(cpmArray[,1] * 1000), ylim=c(0, max.cpm+5), xlim=c(0,ts.len), lty=3, col="darkgreen", lwd=4, ylab="", xlab="", xaxt='n', xaxs='i', yaxs='i')
par(new=T)
plot(ts(cpmArray[,2] * 1000), ylim=c(0, max.cpm+5), xlim=c(0,ts.len), lty=3, col="limegreen", lwd=4, ylab="", xlab="", xaxt='n', xaxs='i', yaxs='i')

