library(reshape2)
library(ggplot2)
library(Hmisc)
library(plyr)
library(Metrics)
source('multiplot.R')
source('tools.R')

#save(tscopy, file='tscopy.withflux.Rdata')
#tscopy.original = tscopy
tscopy = tscopy.original
#tscopy$flux.mgs[tscopy$flux.mgs > 6000000] = NA


# get the warming and cooling phase values
#ts.temperature = ts.stemp$SoilTemperature2
ts.temperature = tstemp

running.regression(ts.stemp$SoilTemperature2, 2, 'flux.mgs')
running.regression(ts.stemp$SoilTemperature2, 2, 'flux.mgs', mode = 2)

rval = running.regression(ts.stemp$SoilTemperature2, 4, 'HPOA.mgl', 
                          mode = 2, maxy = 3, step=1)
# , warming.stop.t = 282
# , warming.stop.t = 400
running.regression = function(tscopy, ts.temperature, window, variable, 
                              nbins = 20, mode = 1, maxy=80000,
                              plot=TRUE){
  
  rising.temp = rising.step(ts.temperature)
  falling.temp = falling.step(ts.temperature)
  names(rising.temp) = c("Temperature")
  names(falling.temp) = c("Temperature")
  
  tshval = rising.temp
  tslval = falling.temp
  
  
  # create nomax
  mg <- aggregate(as.vector(tslval$Temperature), by=list(.indexyear(tslval)), FUN=max) 
  tslval.nomax = tslval
  compare = mg[.indexyear(tslval.nomax)- 110,]
  tslval.nomax[tslval.nomax$Temperature >= (compare$x-1)] = NA
  
  
  # just the warming phase
  start = min(tshval)
  stop.t = max(tshval)
  plots = floor((stop.t - start) / window) + 1
  plots = floor(stop.t - start)
  colors = colorRampPalette(c("blue", "red"))( plots ) 
  
  next.t = start
  i = 1
  all.avgs = NULL
  class.count = NULL
  regressions = NULL
  lms = NULL
  point.estimates = NULL
  slopes = NULL
  intercepts = NULL
  rmses = NULL
  while(next.t < stop.t) {
    #print(next.t)
    min = next.t
    max = min + window
    times = index(tshval[tshval$Temperature >= min & tshval$Temperature <= max & is.na(tslval.nomax$Temperature)])
    class.count = rbind(class.count, length(times))
    ts.window = tscopy[times]
    if(length(times) <= 1){
      print(paste0('Skip ', next.t))
      next.t = next.t + 1
      rmses = rbind(rmses, NA)
      intercepts = rbind(intercepts, NA)
      slopes = rbind(slopes, NA)
      next
    }
    
    avgs = timeseries.bin.and.average(ts.window, variable, bin.number=nbins)
    if(nrow(avgs) <= 1){
      print(paste0('Skip ', next.t))
      next.t = next.t + 1
      rmses = rbind(rmses, NA)
      intercepts = rbind(intercepts, NA)
      slopes = rbind(slopes, NA)
      next
    }
    
    lm.avgs = lm( var.mean ~ poly(discharge.mean, 1), data=avgs)
    intercepts = rbind(intercepts, lm.avgs$coefficients[1])
    slopes = rbind(slopes, lm.avgs$coefficients[2])
    
    lms[[i]] <- lm.avgs
    #print(summary(lm.avgs)$r.squared)
    avgs$temperature.class = i
    avgs$temperature.class.base = next.t
    avgs$predictions = predict(lm.avgs)
    all.avgs = rbind(all.avgs, avgs)
    
    df = as.data.frame(ts.window)
    if(plot) {
      plot(df$Discharge, df[,variable], xlab='Discharge (cfs)', ylab='HPOA Flux (mg/s)', 
         xlim=c(0,100000), 
        ylim=c(0, maxy),
        col = rainbow(6)[.indexyear(ts.window)-110],
        main = paste0(min, ' to ', max))
      lines(avgs$discharge.mean, avgs$predictions, col = colors[i], lwd=3)
    }
    # assign original / prediction list
    temperature.target = min + window/2
    times = index(tshval[tshval$Temperature >= (temperature.target-.5) & tshval$Temperature < (temperature.target + .5) & is.na(tslval.nomax$Temperature)])
    if(length(times) != 0){
      ts.window = tscopy[times]
      df = as.data.frame(ts.window)
      df = df[!is.na(df[,variable]) & !is.na(df$Discharge),]
      predictions = predict(lm.avgs, data.frame(discharge.mean = df$Discharge))
      lm.point.estimates = cbind(df[,variable], predictions)
      point.estimates = rbind(point.estimates, lm.point.estimates)
      rmses = rbind(rmses, rmse(df[,variable], predictions))
    } else {
      rmses = rbind(rmses, NA)
    }
    
    next.t = next.t + step
    i = i + 1
  }
  
  
  
  avgs.warming = all.avgs
  #regression.warming = regressions
  #warming.class.max = max(regression.warming$temperature.class)
  
  # add fall
  if(mode == 1) {
  avgs.cooling = NULL
  regressions.cooling = NULL
  
  start = max(tslval)
  stop.t = min(tslval)
  plots = floor((stop.t - start) / window) + 1
  plots = floor(start - stop.t)
  colors = colorRampPalette(c("red", "blue"))( plots ) 
  
  next.t = start
  j = 1
  while(next.t > stop.t) {
    max = next.t
    min = max - window
    
    # this next command is causing some issues
    # with the call to flux.avg below
    times = index(tshval[!is.na(tslval.nomax$Temperature) & tslval$Temperature <= max & tslval$Temperature >= min ])
    if(length(times) <= 2){
      next.t = next.t - 1
      next
    }
    class.count = rbind(class.count, length(times))
    ts.window = tscopy[times]
    
    avgs = timeseries.bin.and.average(ts.window, variable, bin.number=nbins)
    lm.avgs = lm( var.mean ~ poly(discharge.mean, 1), data=avgs)
    lms[[i]] <- lm.avgs
    
    #print(summary(lm.avgs)$r.squared)
    avgs$temperature.class = i
    avgs$temperature.class.base = next.t
    
    avgs$predictions = predict(lm.avgs)
    avgs.cooling = rbind(avgs.cooling, avgs)
    
    #regressions.cooling = rbind(regressions.cooling, data.frame(discharge.mean = new.data$discharge.mean, 
    #                                            predictions = predict(lm.avgs, new.data),
    #                                            temperature.class = i)) 
    
    if(plot) {
      
    #if(j == 1) {
    #  plot(avgs$discharge.mean, avgs$predictions, typ='l',
    #       xlab='Discharge (cfs)', ylab='HPOA Flux (mg/s)', 
    #       xlim=c(0,100000), 
    #       ylim=c(0, 8000000),
    #       col = colors[j],
    #       lwd = 3
    #  )
    #} else {
    #  lines(avgs$discharge.mean, avgs$predictions, col = colors[j], lwd=3)
    #}
    }
    
    temperature.target = min + window/2
    times = index(tshval[tshval$Temperature >= (temperature.target-.5) & tshval$Temperature < (temperature.target + .5) & is.na(tslval.nomax$Temperature)])
    if(length(times) != 0){
      ts.window = tscopy[times]
      df = as.data.frame(ts.window)
      df = df[!is.na(df[,variable]) & !is.na(df$Discharge),]
      predictions = predict(lm.avgs, data.frame(discharge.mean = df$Discharge))
      lm.point.estimates = cbind(df[,variable], predictions)
      point.estimates = rbind(point.estimates, lm.point.estimates)
      rmses = rbind(rmses, rmse(df[,variable], predictions))
    }
    
    next.t = next.t - step
    i = i + 1
    j = j + 1
  }
  all.avgs = rbind(all.avgs, avgs.cooling)
  regressions = rbind(regressions, regressions.cooling)
  }
  
  point.estimates = data.frame(actual = point.estimates[,1], predicted = point.estimates[,2])
  point.estimates = point.estimates[!is.na(point.estimates$actual),]
  point.estimates = point.estimates[!is.na(point.estimates$predicted),]
  print(nrow(point.estimates))
  rmserror = rmse(point.estimates$actual, point.estimates$predicted)
  rval = list(intercepts, slopes, rmserror, rmses)
  return(rval)
}

# ad hov remove hurricane
tscopy.no.hurricane = tscopy[!(.indexyear(tscopy) == 111 & .indexyday(tscopy) > 230 & .indexyday(tscopy) < 260) ]

par(mfrow=c(1,1))
ts.temperature = ts.water.temp$WaterTemperature
ts.values = tscopy
varialbe = 'HPOA.mgl'
window = 8
rval = running.regression(ts.values, ts.temperature , window, variable, mode = 1, maxy = 4, plot=FALSE)
par(mfrow=c(1,1))
plot(rval[[4]], xlab='Temperature Class', ylab='RMSE of target set')


par(mfrow=c(2,1))
plot(rval[[1]], ylim=c(0,3), xlab='Temperature Class', ylab='Y Intercept')
plot(rval[[2]], ylim=c(0,2), xlab='Tempearture Class', ylab='Slope')
rval[[3]]


windows = c(16,10,8,6,4,2)
rvals = list()
for(i in 1:length(windows)){
  rval = running.regression(ts.values, ts.temperature , windows[i], variable, mode = 2, maxy = 4, plot=FALSE)
  rvals[[i]] = rval
  
  par(mfrow=c(1,1))
  plot(rval[[4]], xlab='Temperature Class', ylab='RMSE of target set', main=windows[i])
  par(mfrow=c(2,1))
  plot(rval[[1]], ylim=c(0,3), xlab='Temperature Class', ylab='Y Intercept', main=windows[i])
  plot(rval[[2]], ylim=c(0,2), xlab='Tempearture Class', ylab='Slope', main=windows[i])
  print(rval[[3]])
}

par(mfrow=c(1,1))
plot(rvals[[1]][[4]], col=rainbow(6)[1], typ='l')
lines(rvals[[2]][[4]], col=rainbow(6)[2])
lines(rvals[[3]][[4]], col=rainbow(6)[3])
lines(rvals[[4]][[4]], col=rainbow(6)[4])
lines(rvals[[5]][[4]], col=rainbow(6)[5])
lines(rvals[[6]][[4]], col=rainbow(6)[6])
legend(x='topleft', legend=windows, col=rainbow(6), pch=1)

rmses = NULL
for(i in 1:length(windows)){
  rmses = rbind(rmses, rvals[[i]][[3]])
}
plot(rmses)





















# NDVI adventure
par(mfrow=c(1,1))
ts.temperature = ts.ndvi
ts.values = tscopy
varialbe = 'HPOA.mgl'
window = 8
rval = running.regression(ts.values, ts.temperature , window, variable, mode = 1, step=200, maxy = 4, plot=FALSE)
par(mfrow=c(1,1))
plot(rval[[4]], xlab='Temperature Class', ylab='RMSE of target set')



