
plot.rising.class = function(timeseries, class, window){
  
  tshval = rising.step(timeseries)
  tslval = falling.step(timeseries)
  
  # create nomax
  mg <- aggregate(as.vector(tslval$Temperature), by=list(.indexyear(tslval)), FUN=max) 
  tslval.nomax = tslval
  compare = mg[.indexyear(tslval.nomax)- 110,]
  tslval.nomax[tslval.nomax$Temperature >= (compare$x-1)] = NA
  
  min = class - window/2
  max = min + window
  
  times = index(tshval[tshval$Temperature >= min & tshval$Temperature <= max & is.na(tslval.nomax$Temperature)])
  ts.window = tscopy[times]
  
  hpoa.avgs = timeseries.bin.and.average(ts.window, 'HPOA.mgl', bin.number=nbins)
  lm.hpoa = lm( var.mean ~ poly(discharge.mean, 1), data=hpoa.avgs)
  test.hpoa = coeftest(lm.hpoa, NeweyWest(lm.hpoa))
  print(paste('HPOA',test.hpoa[2,4]))
  
  
  flux.avgs = timeseries.bin.and.average(ts.window, 'flux.mgs', bin.number=nbins)
  lm.flux = lm( var.mean ~ poly(discharge.mean, 1), data=flux.avgs)
  test.flux = coeftest(lm.flux, NeweyWest(lm.flux))
  print(paste('Flux', test.flux[2,4]))
  
  
  df = as.data.frame(ts.window)
  par(mfrow=c(1,2))
  plot(df$Discharge, df$flux.mgs, xlab='Discharge (cfs)', ylab='HPOA Flux (mg/s)', 
         xlim=c(0,100000), 
         ylim=c(0, 8000000),
         col = rainbow(6)[.indexyear(ts.window)-110],
         main = paste0(min, ' to ', max),
         sub = test.flux[2,4])
  lines(flux.avgs$discharge.mean, predict(lm.flux), col = 'red', lwd=3)
  
  
  
  plot(df$Discharge, df$HPOA.mgl, xlab='Discharge (cfs)', ylab='HPOA (ppm)', 
       xlim=c(0,100000), 
       ylim=c(0, 4),
       col = rainbow(6)[.indexyear(ts.window)-110],
       main = paste0(min, ' to ', max),
       sub = test.flux[2,4])
  lines(hpoa.avgs$discharge.mean, predict(lm.hpoa), col = 'red', lwd=3)
  
}

plot.falling.class = function(timeseries, class, window){
  
  tshval = rising.step(timeseries)
  tslval = falling.step(timeseries)
  
  # create nomax
  mg <- aggregate(as.vector(tslval$Temperature), by=list(.indexyear(tslval)), FUN=max) 
  tslval.nomax = tslval
  compare = mg[.indexyear(tslval.nomax)- 110,]
  tslval.nomax[tslval.nomax$Temperature >= (compare$x-1)] = NA
  
  max = class + window/2
  min = max - window
  
  times = index(tshval[!is.na(tslval.nomax$Temperature) & tslval$Temperature <= max & tslval$Temperature >= min ])
  ts.window = tscopy[times]
  
  hpoa.avgs = timeseries.bin.and.average(ts.window, 'HPOA.mgl', bin.number=nbins)
  lm.hpoa = lm( var.mean ~ poly(discharge.mean, 1), data=hpoa.avgs)
  test.hpoa = coeftest(lm.hpoa, NeweyWest(lm.hpoa))
  print(paste('HPOA',test.hpoa[2,4]))
  
  
  flux.avgs = timeseries.bin.and.average(ts.window, 'flux.mgs', bin.number=nbins)
  lm.flux = lm( var.mean ~ poly(discharge.mean, 1), data=flux.avgs)
  test.flux = coeftest(lm.flux, NeweyWest(lm.flux))
  print(paste('Flux', test.flux[2,4]))
  
  
  df = as.data.frame(ts.window)
  par(mfrow=c(1,2))
  plot(df$Discharge, df$flux.mgs, xlab='Discharge (cfs)', ylab='HPOA Flux (mg/s)', 
       xlim=c(0,100000), 
       ylim=c(0, 8000000),
       col = rainbow(6)[.indexyear(ts.window)-110],
       main = paste0(min, ' to ', max),
       sub = test.flux[2,4])
  lines(flux.avgs$discharge.mean, predict(lm.flux), col = 'red', lwd=3)
  
  
  
  plot(df$Discharge, df$HPOA.mgl, xlab='Discharge (cfs)', ylab='HPOA (ppm)', 
       xlim=c(0,100000), 
       ylim=c(0, 4),
       col = rainbow(6)[.indexyear(ts.window)-110],
       main = paste0(min, ' to ', max),
       sub = test.flux[2,4])
  lines(hpoa.avgs$discharge.mean, predict(lm.hpoa), col = 'red', lwd=3)
  
}


#plot.rising.class(ts.stemp$SoilTemperature2, 270, 8)

for(i in 270:290) {
  plot.rising.class(ts.stemp$SoilTemperature2, i, 8)  
}

for(i in 290:270) {
  plot.falling.class(ts.stemp$SoilTemperature2, i, 8)  
}

for(i in 40:80){
  plot.rising.class(ts.ndvi, i*100, 500)  
}


par(mfrow=c(1,1))
#ts.temperature = ts.water.temp$WaterTemperature
ts.temperature = ts.stemp$SoilTemperature2
ts.values = tscopy
variable = 'HPOA.mgl'
window = 8
rval = running.regression(ts.values, ts.temperature, window, variable, mode = 2, maxy = 4, plot=FALSE)
par(mfrow=c(2,1))
plot(rval[[4]], xlab='Temperature Class', ylab='RMSE of target set')
plot(rval$TTests, ylim=c(0,1))
rval[[3]]
