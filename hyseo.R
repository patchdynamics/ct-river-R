library(xts)
library(lubridate)
setwd('/Users/matthewxi/Documents/Projects/PrecipGeoStats/R')
load('ts_fdom_discharge.Rdata')

ts = tscombined$Discharge['2011-01-01/2016-01-01']
times = index(ts)
times = seq.POSIXt(min(times), max(times), 'day')
#times = round(times, "days")
hour(times) <- 0
tsfilled <-merge(ts, xts(, order.by=times))
plot(tsfilled)
tsfilledvalues = na.approx(tsfilled, na.rm = FALSE)
tsfilled[1:length(tsfilled)] = tsfilledvalues
tsfilled$Discharge[tsfilled$Discharge < 0] = 0
plot(tsfilled)


L = nrow(tsfilled)
baseflow = numeric(length = length(tsfilled))
baseflow[1:length(tsfilled)] = NA
window = 11
last_minima = NA;
hw = floor(window/2);
discharge = as.data.frame(tsfilled)$Discharge
for (i in 1:L) {
  if (i <=  hw) {
    baseflow[i] = NA;
    next
  }

  if (i >= L - hw) {
    baseflow[i] = NA;
    next
  }

  minima = min(discharge[(i-hw):(i+hw)])
  if (is.na(last_minima)){
    baseflow[i] = minima;
  } else if (discharge[i] == minima) {
    baseflow[i] = discharge[i];    
  } else  if (discharge[i] < last_minima) {
    baseflow[i] = discharge[i];
  }
  if (!is.na(baseflow[i])) {
    last_minima = baseflow[i];
  }
}
head(baseflow)

baseflowvals = na.approx(baseflow, na.rm=FALSE)
baseflowvals = na.locf(baseflowvals, na.rm=FALSE)
baseflowvals = na.locf(baseflowvals, na.rm=FALSE, fromLast = TRUE)

tsbaseflow = tsfilled
tsbaseflow[1:length(tsfilled)] = baseflowvals
plot(tsbaseflow)

names(tsbaseflow) = c('Discharge')
head(tsbaseflow)

plot(tsbaseflow$Discharge)

tsquickflow = tsbaseflow
tsquickflow[1:length(tsquickflow)] = tsfilled - tsbaseflow


# separate rising and falling hydrograph
prime = diff.xts(tsbaseflow$Discharge)
par(mfrow=c(2,1))
plot(tsbaseflow$Discharge)
plot(prime)


par(mfrow=c(3,1))
plot(tsfilled$Discharge)
plot(tsbaseflow$Discharge)
plot(prime)