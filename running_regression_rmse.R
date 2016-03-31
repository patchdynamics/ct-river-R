library(reshape2)
library(ggplot2)
library(Hmisc)
library(plyr)
library(Metrics)
library(lmtest)
library(sandwich)
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
                          mode = 2, maxy = 3, step=1, target.window=1)

running.regression = function(ts.training, ts.testing, ts.temperature, window, variable, 
                              nbins = 20, mode = 1, step=1, target.window = 1,
                              method = 'all.points',
                              maxy=80000, plot=TRUE){
  
  ts.values = ts.training[!is.na(ts.training$Discharge) & !is.na(ts.training[,variable]),]
  
  tshval = rising.step(ts.temperature)
  tslval = falling.step(ts.temperature)
  
  # create nomax
  mg <- aggregate(as.vector(tslval$Temperature), by=list(.indexyear(tslval)), FUN=max, na.rm=TRUE)   
  tslval.nomax = tslval
  compare = mg[.indexyear(tslval.nomax)- 110,]
  tslval.nomax[tslval.nomax$Temperature >= (compare$x-1)] = NA
  
  # just the warming phase
  start = min(tshval, na.rm=TRUE) - window/2  # add window/2 to center window
  stop.t = max(tshval, na.rm=TRUE) + window/2 # on lowest/highest temperatures
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
  classes = NULL
  rmses = NULL
  ttests = NULL
  if(mode != 3) {
    while(next.t < stop.t) {
      #print(next.t)
      min = next.t
      max = min + window
      temperature.target = min + window/2
      classes = rbind(if(exists('classes')) classes else NULL, temperature.target)
      
      times = index(tshval[tshval$Temperature >= min & tshval$Temperature <= max & is.na(tslval.nomax$Temperature)])
      class.count = rbind(class.count, length(times))
      ts.window = ts.values[times]
      valid = FALSE
      if(nrow(ts.window) > 1){
      
        print(paste(min, max))
        if(method == 'bin1') {
          avgs = timeseries.bin.and.average(ts.window, variable, bin.number=nbins)
        } else if (method == 'all.points'){
          df.window = as.data.frame(ts.window)
          all.points = data.frame(var.mean = df.window[,variable], discharge.mean=df.window$Discharge)
          avgs = all.points
        }
        if(nrow(avgs) > 2){
        
          lm.avgs = lm(var.mean ~ poly(discharge.mean, 1), data=avgs)
          intercepts = rbind(intercepts, lm.avgs$coefficients[1])
          slopes = rbind(slopes, lm.avgs$coefficients[2])
          test = coeftest(lm.avgs, NeweyWest(lm.avgs, prewhite=FALSE))
          ttests = rbind(ttests, test[2,4])
          valid = TRUE
        }
      }
      if(!valid){
        print(paste0('Skip ', next.t))
        next.t = next.t + step
        rmses = rbind(rmses, NA)
        intercepts = rbind(intercepts, NA)
        slopes = rbind(slopes, NA)
        ttests = rbind(ttests, NA)
        next
      }
      
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
      times = index(tshval[tshval$Temperature >= (temperature.target-target.window/2) & tshval$Temperature < (temperature.target + target.window/2) & is.na(tslval.nomax$Temperature)])
      
      new.values = FALSE
      if(length(times) > 0 && nrow(ts.testing[times]) > 0) {
        ts.window = ts.testing[times,]
        df = as.data.frame(ts.window)
        df = df[!is.na(df[,variable]) & !is.na(df$Discharge),]
        if(nrow(df) != 0){
          predictions = predict(lm.avgs, data.frame(discharge.mean = df$Discharge))
          lm.point.estimates = cbind(df[,variable], predictions, i, temperature.target)
          point.estimates = rbind(point.estimates, lm.point.estimates)
          rmses = rbind(rmses, rmse(df[,variable], predictions))        
          new.values = TRUE
        }
      }
      if(new.values == FALSE) {
        rmses = rbind(rmses, NA)
      }
      
      next.t = next.t + step
      i = i + 1
    }
  }
  avgs.warming = all.avgs
  
  # add fall
  if(mode != 2) {
    avgs.cooling = NULL
    regressions.cooling = NULL
    
    stop.t = min(tshval, na.rm=TRUE) - window/2  # add window/2 to center window
    start = max(tshval, na.rm=TRUE) + window/2 # on lowest/highest temperatures
    plots = floor((stop.t - start) / window) + 1
    plots = floor(start - stop.t)
    colors = colorRampPalette(c("red", "blue"))( plots ) 
    
    next.t = start
    j = 1
    while(next.t > stop.t) {
      max = next.t
      min = max - window
      
      temperature.target = max - window/2
      classes = rbind(if(exists('classes')) classes else NULL, temperature.target)
      
      times = index(tshval[!is.na(tslval.nomax$Temperature) & tslval$Temperature <= max & tslval$Temperature >= min ])
      valid = FALSE
      if(length(times) > 2){

        class.count = rbind(class.count, length(times))
        ts.window = ts.values[times,]
        print(paste(min, max))
      
        if(method == 'bin1') {
          avgs = timeseries.bin.and.average(ts.window, variable, bin.number=nbins)
        } else if (method == 'all.points'){
          df.window = as.data.frame(ts.window)
          all.points = data.frame(var.mean = df.window[,variable], discharge.mean=df.window$Discharge)
          avgs = all.points
        }
        if(nrow(avgs) > 2){  
          lm.avgs = lm( var.mean ~ poly(discharge.mean, 1), data=avgs)
          intercepts = rbind(intercepts, lm.avgs$coefficients[1])
          slopes = rbind(slopes, lm.avgs$coefficients[2])
          test = coeftest(lm.avgs, NeweyWest(lm.avgs, prewhite=FALSE))
          ttests = rbind(ttests, test[2,4])
          valid = TRUE
        }
      }
      if(!valid){
        print(paste0('Skip ', next.t))
        next.t = next.t - step
        rmses = rbind(rmses, NA)
        intercepts = rbind(intercepts, NA)
        slopes = rbind(slopes, NA)
        ttests = rbind(ttests, NA)
        next
      }
      
      avgs$temperature.class = i
      avgs$temperature.class.base = next.t    
      avgs$predictions = predict(lm.avgs)
      avgs.cooling = rbind(avgs.cooling, avgs)
      
      df = as.data.frame(ts.window)
      if(plot) {
        plot(df$Discharge, df[,variable], xlab='Discharge (cfs)', ylab='HPOA Flux (mg/s)', 
             xlim=c(0,100000), 
             ylim=c(0, maxy),
             col = rainbow(6)[.indexyear(ts.window)-110],
             main = paste0(min, ' to ', max))
        lines(avgs$discharge.mean, avgs$predictions, col = colors[i], lwd=3)
      }
      
      times = index(tshval[tshval$Temperature >= (temperature.target-target.window/2) & tshval$Temperature < (temperature.target + target.window/2) & is.na(tslval.nomax$Temperature)])
      new.values = FALSE
      if(length(times) > 0 && nrow(ts.testing[times]) > 0) {
        ts.window = ts.testing[times,]
        df = as.data.frame(ts.window)
        df = df[!is.na(df[,variable]) & !is.na(df$Discharge),]
        if(nrow(df) != 0){
          predictions = predict(lm.avgs, data.frame(discharge.mean = df$Discharge))
          lm.point.estimates = cbind(df[,variable], predictions, i, temperature.target)
          point.estimates = rbind(point.estimates, lm.point.estimates)
          rmses = rbind(rmses, rmse(df[,variable], predictions))        
          new.values = TRUE
        }
      }
      if(new.values == FALSE) {
        rmses = rbind(rmses, NA)
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
  rval = list(intercepts, slopes, rmserror, rmses, classes,ttests)
  names(rval) = c('Intercepts', 'Slopes', 'RMSE', 'RMSES', 'Classes', 'TTests')
  return(rval)
}

# ad hov remove hurricane
tscopy.no.hurricane = tscopy[!(.indexyear(tscopy) == 111 & .indexyday(tscopy) > 230 & .indexyday(tscopy) < 260) ]

par(mfrow=c(1,1))
ts.temperature = ts.water.temp$WaterTemperature
ts.temperature = tstemp
ts.temperature = ts.stemp$SoilTemperature2
main = 'Soil Temperature 1'
ts.values = tscopy
variable = 'HPOA.mgl'
window = 8
rval = running.regression(ts.values, ts.values, ts.temperature, window, variable, mode = 1, maxy = 4, 
                          plot=FALSE, method='all.points')
rval[[3]]

par(mfrow=c(2,2))
plot(rval[[4]], xlab='Temperature Class', ylab='RMSE of target set', main=rval[[3]], ylim=c(0,2))
ttest.colors = as.numeric(rval$TTests < .05) + 1
ttest.colors[rval[[2]] < .2] = 3
plot(rval$TTests, ylim=c(0,1), main=main, col=c('red', 'blue', 'orange')[ttest.colors])

#par(mfrow=c(2,1))
plot(rval[[1]], ylim=c(0,3), xlab='Temperature Class', ylab='Y Intercept')
plot(rval[[2]], ylim=c(0,8), xlab='Temperature Class', ylab='Slope', 
     col=c('black', 'orange')[as.numeric(rval[[2]] < .2) + 1])





# yearly folding
tscopy$fold = .indexyear(tscopy) - min(.indexyear(tscopy)) + 1
ts.full.values = tscopy
ts.temperature = ts.stemp$SoilTemperature1  # window = 10
ts.temperature = ts.stemp$SoilTemperature2  # window = 4 (6)
ts.temperature = tstemp # window = 10
ts.temperature = ts.water.temp$WaterTemperature # window = 6 (8)
ts.temperature = ts.ndvi # window = 1250

main = 'NDVI'
variable = 'HPOA.mgl'
windows = c(30,20,18,16,14,12,10,8,6,4,2)
k = 5

#ndvi
windows = c(2750,2500,2250,2000,1750,1500,1250,1000,750,500,250) # ndvi
k = 4

errors.rmse = NULL
for(w in windows) {
  rvals = NULL
  for(i in 1:k) {
    ts.training = ts.full.values[ts.full.values$fold!=i,]
    ts.testing = ts.full.values[ts.full.values$fold==i,]
    rval = running.regression(ts.training, ts.testing,  ts.temperature, w, variable, mode = 2, 
                              step = 100, target.window = 250, #ndvi
                              plot=FALSE)
    rvals = rbind(rvals, rval)
    if(i==1){
      plot(rval[[2]], main=main)
    } else {
      points(rval[[2]], pch=i)
    }
  }
  errors.rmse = rbind(errors.rmse, mean(as.numeric(rvals[,3])))
}
plot(windows, errors.rmse)



# for a range of windows
windows = c(16,10,8,6,4,2)
rvals = list()
for(i in 1:length(windows)){
  rval = running.regression(ts.values, ts.temperature , windows[i], variable, mode = 3, maxy = 4, plot=FALSE)
  rvals[[i]] = rval
  
  par(mfrow=c(1,1))
  plot(rval[[4]], xlab='Temperature Class', ylab='RMSE of target set', main=windows[i])
  
  #par(mfrow=c(2,1))
  #plot(rval[[1]], ylim=c(0,3), xlab='Temperature Class', ylab='Y Intercept', main=windows[i])
  #plot(rval[[2]], ylim=c(0,2), xlab='Temperature Class', ylab='Slope', main=windows[i])
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
window = 1250
step=100
target.window = 250

rval = running.regression(ts.values, ts.values, ts.temperature , 
                          window, variable, mode = 2, 
                          step=step, target.window = 250,
                          maxy = 4, plot=FALSE)
rval[[3]]
par(mfrow=c(1,1))
plot(rval[[4]], xlab='Temperature Class', ylab='RMSE of target set')



windows = c(1000,750,500,250)
rvals = list()
ts.temperature = ts.ndvi
for(i in 1:length(windows)){
  rval = running.regression(ts.values, ts.temperature , 
                            windows[i], variable, mode = 3, 
                            step=100, target.window = 250,
                            maxy = 4, plot=FALSE)  
  rvals[[i]] = rval
  par(mfrow=c(1,1))
  plot(rval[[5]], rval[[4]], xlim=c(9000,3000), xlab='Temperature Class', ylab='RMSE of target set', main=windows[i])
  par(mfrow=c(2,1))
  plot(rval[[5]], rval[[1]], ylim=c(0,3), xlim=c(9000,3000), xlab='Temperature Class', ylab='Y Intercept', main=windows[i])
  plot(rval[[5]], rval[[2]], ylim=c(0,2), xlim=c(9000,3000), xlab='Tempearture Class', ylab='Slope', main=windows[i])
  print(rval[[3]])
}


