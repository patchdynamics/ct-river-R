library(reshape2)
library(ggplot2)
library(Hmisc)
library(plyr)
library(Metrics)
library(nlme)
source('multiplot.R')
source('tools.R')

#save(tscopy, file='tscopy.withflux.Rdata')
#tscopy.original = tscopy
tscopy = tscopy.original
#tscopy$flux.mgs[tscopy$flux.mgs > 6000000] = NA


# get the warming and cooling phase values
#ts.temperature = ts.stemp$SoilTemperature2
ts.temperature = tstemp

running.regression(tscopy, ts.stemp$SoilTemperature2, 4, 'HPOA.mgl')
running.regression(tscopy, ts.stemp$SoilTemperature2, 2, 'flux.mgs', mode = 2)

running.regression(tscopy, ts.stemp$SoilTemperature2, 4, 'HPOA.mgl', mode = 2)
# , warming.stop.t = 282
# , warming.stop.t = 400

running.regression(tscopy, ts.ndvi, 1000, 'HPOA.mgl', step=500)


running.regression = function(ts.values, ts.temperature, window, variable, nbins = 20, mode = 1, step=1){
  
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
  while(next.t < stop.t) {
    min = next.t
    max = min + window
    times = index(tshval[tshval$Temperature >= min & tshval$Temperature <= max & is.na(tslval.nomax$Temperature)])
    class.count = rbind(class.count, length(times))

    ts.window = ts.values[times]
    
    df = as.data.frame(ts.window)
    df$t = .indexday(ts.window)
    df$group = .indexyear(ts.window)
    df = df[!is.na(df[,variable]) & !is.na(df$Discharge),]  
    
    if(nrow(df) <= 1){
      next.t = next.t + step
      next
    }
    
    #df$y = df[,variable]
    gls.model.ar1 = gls( as.formula(paste(variable,'~ Discharge')),
                         df,
                         correlation=corARMA(p=1, form= ~t | group))
    #gls.model.ar1 = gls( as.formula(paste(variable,'~ Discharge')),
    #                    df)
                         
    
    lms[[i]] <- gls.model.ar1
    intercepts = rbind(intercepts, gls.model.ar1$coefficients[1])
    slopes = rbind(slopes, gls.model.ar1$coefficients[2])
    #print(summary(lm.avgs)$r.squared)
    avgs$temperature.class = i
    avgs$temperature.class.base = next.t
    predictions = predict(gls.model.ar1)
    all.avgs = rbind(all.avgs, avgs) 
    
    plot(df$Discharge, as.vector(df[,variable]), xlab='Discharge (cfs)', ylab='HPOA Flux (mg/s)', 
             xlim=c(0,100000), 
             ylim=c(0, 3), #hmmm...
             main = paste0(min, ' to ', max),
           col=rainbow(6)[df$group-110])
    lines(df$Discharge, predictions, col = 'coral2', lwd=3)
    
    # assign original / prediction list
    temperature.target = min + window/2
    times = index(tshval[tshval$Temperature >= (temperature.target-.5) & tshval$Temperature < (temperature.target + .5) & is.na(tslval.nomax$Temperature)])
    if(length(times) != 0){
      ts.window = ts.values[times]
      df = as.data.frame(ts.window)
      df = df[!is.na(df[,variable]) & !is.na(df$Discharge),]
      predictions = predict(gls.model.ar1, df)
      lm.point.estimates = cbind(df[,variable], predictions)
      point.estimates = rbind(point.estimates, lm.point.estimates)
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
      
      times = index(tshval[!is.na(tslval.nomax$Temperature) & tslval$Temperature <= max & tslval$Temperature >= min ])
      if(length(times) <= 2){
        next.t = next.t - 1
        next
      }
      class.count = rbind(class.count, length(times))
      ts.window = ts.values[times]
      df = as.data.frame(ts.window)
      df$t = .indexday(ts.window)
      df$group = .indexyear(ts.window)
      df = df[!is.na(df[variable]) & !is.na(df$Discharge),]    
      
      gls.model.ar1 = gls( as.formula(paste(variable,'~ Discharge')),
                           df,
                           correlation=corARMA(value=c(.5), 
                                               p=1, 
                                               form= ~t | group,
                                               fixed=TRUE)
      )
      predictions = predict(gls.model.ar1)
      
      avgs$temperature.class = i
      avgs$temperature.class.base = next.t
      
      avgs.cooling = rbind(avgs.cooling, avgs)
      
      df2 = df[df$Limb == 0,]
      df2 = df
      plot(df2$Discharge, df2[,variable], xlab='Discharge (cfs)', ylab='HPOA Flux (mg/s)', 
           xlim=c(0,100000), 
           ylim=c(0, 3),
           col=rainbow(6)[df2$group-110],
           pch=(1:3)[df2$Limb + 2], 
           main = min
           )
      legend(x="bottomright",  legend=unique(df$group), col=rainbow(6), pch=1)
      lines(df$Discharge, predictions, col = colors[j], lwd=3)
      
      temperature.target = min + window/2
      times = index(tshval[tshval$Temperature >= (temperature.target-.5) & tshval$Temperature < (temperature.target + .5) & is.na(tslval.nomax$Temperature)])
      if(length(times) != 0){
        ts.window = ts.values[times]
        df = as.data.frame(ts.window)
        df = df[!is.na(df[,variable]) & !is.na(df$Discharge),]
        predictions = predict(gls.model.ar1, df)
        lm.point.estimates = cbind(df[,variable], predictions)
        point.estimates = rbind(point.estimates, lm.point.estimates)
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
  print(nrow(point.estimates))
  rmserror = rmse(point.estimates$actual, point.estimates$predicted)

  rval = list(rmserror, intercepts, slopes)
  names(rval) = c('rmse', 'intercepts', 'slopes')

  return(rval)
}

running.regression(tscopy, ts.stemp$SoilTemperature2, 4, 'HPOA.mgl')


running.regression(tscopy, ts.ndvi, 1000, 'HPOA.mgl', step=500)



ts.values = tscopy
ts.temperature = ts.stemp$SoilTemperature1 
window = 4
variable = 'HPOA.mgl' 
nbins = 20
mode = 2
out = running.regression(ts.values, ts.temperature, window, variable, mode)



