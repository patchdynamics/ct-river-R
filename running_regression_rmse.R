library(reshape2)
library(ggplot2)
library(Hmisc)
source('multiplot.R')
source('tools.R')

#save(tscopy, file='tscopy.withflux.Rdata')
#tscopy.original = tscopy
tscopy = tscopy.original
#tscopy$flux.mgs[tscopy$flux.mgs > 6000000] = NA


# get the warming and cooling phase values
#ts.temperature = ts.stemp$SoilTemperature2
ts.temperature = tstemp

running.regression(ts.stemp$SoilTemperature2, 2)
running.regression(ts.stemp$SoilTemperature2, 2, mode = 2)

running.regression = function(ts.temperature, window, nbins = 20, mode = 1){
  
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
  while(next.t < stop.t) {
    #print(next.t)
    min = next.t
    max = min + window
    times = index(tshval[tshval$Temperature >= min & tshval$Temperature <= max & is.na(tslval.nomax$Temperature)])
    class.count = rbind(class.count, length(times))
    ts.window = tscopy[times]
    if(length(times) <= 1){
      next.t = next.t + 1
      next
    }
    
    avgs = timeseries.bin.and.average(ts.window, variable, bin.number=nbins)
    lm.avgs = lm( var.mean ~ poly(discharge.mean, 1), data=avgs)
    lms[[i]] <- lm.avgs
    #print(summary(lm.avgs)$r.squared)
    avgs$temperature.class = i
    avgs$temperature.class.base = next.t
    avgs$predictions = predict(lm.avgs)
    all.avgs = rbind(all.avgs, avgs)
    
    #regressions = rbind(regressions, data.frame(discharge.mean = new.data$discharge.mean, 
    #                                            predictions = predict(lm.avgs, new.data),
    #                                            temperature.class = i,
    #                                            temperature.target = min + window/2)) 
    
    df = as.data.frame(ts.window)
    #plot(df$Discharge, df$flux.mgs, xlab='Discharge (cfs)', ylab='HPOA Flux (mg/s)', 
    #     xlim=c(0,100000), 
    #   ylim=c(0, 8000000),
    #    main = min)
    #if(next.t == start) {
    #  lines(avgs$discharge.mean, avgs$predictions, typ='l',
    #        xlab='Discharge (cfs)', ylab='HPOA Flux (mg/s)', 
    #       xlim=c(0,100000), 
    #        ylim=c(0, 8000000),
    #        col = colors[i],
    #        lwd = 3
    #  )
    #} else {
    #  lines(avgs$discharge.mean, avgs$predictions, col = colors[i], lwd=3)
    #}
    
    # assign original / prediction list
    temperature.target = min + window/2
    times = index(tshval[tshval$Temperature >= (temperature.target-.5) & tshval$Temperature < (temperature.target + .5) & is.na(tslval.nomax$Temperature)])
    if(length(times) != 0){
      ts.window = tscopy[times]
      df = as.data.frame(ts.window)
      df = df[!is.na(df[,variable] & !is.na(df$Discharge)),]
      predictions = predict(lm.avgs, data.frame(discharge.mean = df$Discharge))
      lm.point.estimates = cbind(df$flux.mgs, predictions)
      point.estimates = rbind(point.estimates, lm.point.estimates)
    }
    
    next.t = next.t + 1
    i = i + 1
  }
  
  
  
  avgs.warming = all.avgs
  regression.warming = regressions
  warming.class.max = max(regression.warming$temperature.class)
  
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
    
    temperature.target = min + window/2
    times = index(tshval[tshval$Temperature >= (temperature.target-.5) & tshval$Temperature < (temperature.target + .5) & is.na(tslval.nomax$Temperature)])
    if(length(times) != 0){
      ts.window = tscopy[times]
      df = as.data.frame(ts.window)
      df = df[!is.na(df[,variable] & !is.na(df$Discharge)),]
      predictions = predict(lm.avgs, data.frame(discharge.mean = df$Discharge))
      lm.point.estimates = cbind(df[,variable], predictions)
      point.estimates = rbind(point.estimates, lm.point.estimates)
    }
    
    
    
    next.t = next.t - 1
    i = i + 1
    j = j + 1
  }
  all.avgs = rbind(all.avgs, avgs.cooling)
  regressions = rbind(regressions, regressions.cooling)
  }
  
  point.estimates = data.frame(actual = point.estimates[,1], predicted = point.estimates[,2])
  point.estimates = point.estimates[!is.na(point.estimates$actual),]
  print(nrow(point.estimates))
  rmserror = rmse(point.estimates$actual, point.estimates$predicted)
  return(rmserror)
}