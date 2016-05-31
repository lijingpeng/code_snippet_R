library(data.table)
library(TTR)
library(forecast)

data = read.csv(file = 'dsp_hf.csv')
setnames(data, names(data), c("dsp_id", "date","cpm"))
subdata = data.table(data[data$dsp_id=='110534214', c(2,3)])
setnames(subdata, names(subdata), c("date", "cpm"))

subdata.quant <- subdata[, quantile(sort(as.numeric(cpm)), 0.8), by=date]
date.order <- order(subdata.quant$date)
subdata.quant <- subdata.quant[date.order]
upper.bound <- length(date.order)

# timeseries analysis
quant.array <- subdata.quant$V1[1:(upper.bound)]
quant.ts <- ts(quant.array[1:(length(quant.array))])
#quant.ts.sma <- SMA(quant.ts, n=15)
quant.ts.hw <- HoltWinters(quant.ts, gamma=F)
par(mfrow=c(2,1))
plot(quant.ts.hw, intervals=T, xaxt='n', ylab="CPM", main="Timeseries Analysis of Daily CPM Quantile (80%)")
