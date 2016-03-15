library(xts)




data.frame$mydatetime = strptime(FORMAT STRING)

timeseries = xts(data.frame, order.by=data.frame$mydatetime)


timeseries = timeseries[.indexminute(timeseries) < 15 | .indexminute(timeseries) > 45 ]
timeseries$hour[.indexminute(timeseries) < 30] = .indexhour(timeseries[.indexminute(timeseries) < 30]) 
timeseries$hour[.indexminute(timeseries) >= 30] = .indexhour(timeseries[.indexminute(timeseries) >= 30]) + 1

tsts = tslval
tsts$year[.indexyear(tsts) == 115] = 3
