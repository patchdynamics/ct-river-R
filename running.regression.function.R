
running.regression = function(ts.training, ts.testing, ts.temperature, window, variable, 
                              nbins = 20, mode = 1, step=1, target.window = 1, include.transition.zone = TRUE,
                              method = 'all.points',
                              maxy=80000, plot=TRUE,
                              normalizing.discharges = c(200000,40000,60000,80000),
                              prewhite = TRUE, drop.insignificant = FALSE,
                              #speedup
                              tshval = NULL, tslval = NULL, tslval.nomax = NULL, tshval.nomax = NULL
){
  normalizing.discharges = data.frame(discharge.mean = normalizing.discharges)
  ts.values = ts.training[!is.na(ts.training$Discharge) & !is.na(ts.training[,variable]),]
  
  # this doesn't really need to happen each and every time.
  # these series could be stored in some kind of object and this would blazzzzze
  if(is.null(tshval) ){
    tshval = rising.step(ts.temperature)
  }
  if(is.null(tslval)){
    tslval = falling.step(ts.temperature)
  }
  if(is.null(tslval.nomax)) {
    tslval.nomax = temp = tslval
    temp$Temperature[is.na(temp$Temperature)] = min(temp$Temperature, na.rm=TRUE)
    tslval.nomax[rollmax(temp$Temperature,150,align='center',fill='extend') == tslval] = NA
  }
  if(is.null(tshval.nomax)) {
    tshval.nomax = temp = tshval
    temp$Temperature[is.na(temp$Temperature)] = min(temp$Temperature, na.rm=TRUE)
    tshval.nomax[rollmax(temp$Temperature,150,align='center',fill='extend') == tshval] = NA
  }
  
  # just the warming phase
  start = floor(min(tshval, na.rm=TRUE)) - window/2  # add window/2 to center window
  stop.t = floor(max(tshval, na.rm=TRUE)) + window/2 # on lowest/highest temperatures
  plots = floor((stop.t - start) / window) + 1
  plots = floor(stop.t - start)
  colors = colorRampPalette(c("blue", "red"))( plots ) 
  
  next.t = start
  i = 1
  all.avgs = class.count = regressions = lms = point.estimates = normalized.estimates = NULL
  slopes = intercepts = classes = rmses = ttests = NULL
  
  falling.next = floor(max(tslval, na.rm=TRUE))
  if(mode != 3) {
    # while(next.t + window <= max(tshval, na.rm=TRUE) ) {
    keep.going = TRUE
    while(keep.going) {
      #print(next.t)
      if(next.t + window < max(tshval, na.rm=TRUE)) {
        min = next.t
        max = min + window
        temperature.target = min + window/2
        times = index(tshval[tshval$Temperature >= min & tshval$Temperature <= max & is.na(tslval.nomax$Temperature)])
        display.min = min
        display.max = max
      } else if(next.t <= max(tshval, na.rm=TRUE)){  
        if(include.transition.zone == FALSE){
          keep.going = FALSE
          next
        }
        
        rising.min = next.t
        falling.min = falling.next
        if(rising.min + window/2 <= max(tshval, na.rm=TRUE)){
          temperature.target = rising.min + window/2
        } else {
          temperature.target = -falling.min + window/2    
        }
        times = index(tshval[(tshval$Temperature >= rising.min & is.na(tslval.nomax$Temperature)) | (tslval$Temperature >= falling.min &  is.na(tshval.nomax$Temperature))])
        display.min = rising.min
        display.max = falling.min
        falling.next = falling.next - step
      } else {
        keep.going=FALSE
        next
      }
     # print(paste(display.min, display.max))
      
      
      classes = rbind(classes, temperature.target)  
      class.count = rbind(class.count, length(times))
      ts.window = ts.values[times]
      valid = FALSE
      if(nrow(ts.window) > 1){
        
        if(method == 'bin1') {
          avgs = timeseries.bin.and.average(ts.window, variable, bin.number=nbins)
        } else if (method == 'all.points'){
          df.window = as.data.frame(ts.window)
          all.points = data.frame(var.mean = df.window[,variable], discharge.mean=df.window$Discharge)
          avgs = all.points
        }
        if(nrow(avgs) > 2){
          
          lm.avgs = lm(var.mean ~ poly(discharge.mean, 1, raw=TRUE), data=avgs)
          test = coeftest(lm.avgs, NeweyWest(lm.avgs, prewhite = prewhite))
          ttests = rbind(ttests, test[2,4])
          if(test[2,4] > .05 && drop.insignificant){
              lm.avgs = lm(var.mean ~ 1, data=avgs)
              slopes = rbind(slopes, 0)
              intercepts = rbind(intercepts, lm.avgs$coefficients[1])
          } else {
            slopes = rbind(slopes, lm.avgs$coefficients[2])
            intercepts = rbind(intercepts, lm.avgs$coefficients[1])        
          }
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
        normalized.estimates = rbind(normalized.estimates, NA)
        next
      }
      
      df = as.data.frame(ts.window)
      avg.discharge = mean(df$Discharge)
      min.discharge = min(df$Discharge)
      max.discharge = max(df$Discharge)
      q25.discharge = quantile(df$Discharge, na.rm=TRUE)[2]
      q75.discharge = quantile(df$Discharge, na.rm=TRUE)[4]
      q90.discharge = quantile(df$Discharge, probs=.9, na.rm=TRUE)[1]
      print(max.discharge)
      if(plot) {
        plot(df$Discharge, df[,variable], xlab='Discharge (m^3/s)', ylab='HPOA Flux (mg/s)', 
             xlim=c(0,4), 
             ylim=c(0, maxy),
             col = rainbow(6)[.indexyear(ts.window)-110],
             main = paste(display.min, 'to', display.max, i))
        lines(avgs$discharge.mean, predict(lm.avgs), col = colors[i], lwd=3)
      }
      # assign original / prediction list
      if(next.t + window <= max(tshval, na.rm=TRUE)) {
        times = index(tshval[tshval$Temperature >= (temperature.target-target.window/2) & tshval$Temperature < (temperature.target + target.window/2) & is.na(tslval.nomax$Temperature)])
      } else if(temperature.target > 0){
        times = index(tshval[tshval$Temperature >= (temperature.target-target.window/2) & tshval$Temperature < (temperature.target + target.window/2)  & is.na(tslval.nomax$Temperature)]) 
      } else {
        temperature.target = -temperature.target
        times = index(tshval[tslval$Temperature >= (temperature.target-target.window/2) & tslval$Temperature < (temperature.target + target.window/2) ]) 
      }
      
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
          if(plot == TRUE){
            points(df$Discharge, df[,variable], col='orange', pch=4, lwd='4')
          }
        }
      }
      if(new.values == FALSE) {
        rmses = rbind(rmses, NA)
      }
      normalizing.discharges.1 = rbind(normalizing.discharges, avg.discharge, min.discharge, max.discharge, q25.discharge, q75.discharge, q90.discharge)
      normalized.estimate = predict(lm.avgs, normalizing.discharges.1)
      normalized.estimates = rbind(normalized.estimates, normalized.estimate)
      
      next.t = next.t + step
      i = i + 1
    }
  }
  
  
  # add fall
  if(mode == 3){
    falling.next = falling.next - window/2
  }
  if(mode != 2) {
    avgs.cooling = NULL
    regressions.cooling = NULL
    
    stop.t = floor(min(tshval, na.rm=TRUE)) - window/2  # add window/2 to center window
    #start = floor(max(tshval, na.rm=TRUE)) + window/2 # on lowest/highest temperatures
    start = falling.next + window
    plots = floor((stop.t - start) / window) + 1
    plots = floor(start - stop.t)
    colors = colorRampPalette(c("red", "blue"))( plots ) 
    
    next.t = start
    j = 1
    rising.next = ceil(min(tshval, na.rm=TRUE))
    while(next.t > stop.t) {
      max = next.t
      min = max - window
      
      temperature.target = max - window/2
      classes = rbind(if(exists('classes')) classes else NULL, temperature.target)
      
      if(min > stop.t) {
        times = index(tshval[!is.na(tslval.nomax$Temperature) & tslval$Temperature <= max & tslval$Temperature >= min ])
      } else {
        #need to wrap
        times = index(tshval[tslval$Temperature <= max | tshval$Temperature <= rising.next])
        rising.next = rising.next + 1
      }
      valid = FALSE
      if(nrow(ts.values[times,])> 2){
        
        class.count = rbind(class.count, length(times))
        ts.window = ts.values[times,]
        #print(paste(min, max))
        
        if(method == 'bin1') {
          avgs = timeseries.bin.and.average(ts.window, variable, bin.number=nbins)
        } else if (method == 'all.points'){
          df.window = as.data.frame(ts.window)
          all.points = data.frame(var.mean = df.window[,variable], discharge.mean=df.window$Discharge)
          avgs = all.points
        }
        if(nrow(avgs) > 2){  
          lm.avgs = lm( var.mean ~ poly(discharge.mean, 1, raw=TRUE), data=avgs)
          test = coeftest(lm.avgs, NeweyWest(lm.avgs, prewhite=prewhite))
          ttests = rbind(ttests, test[2,4])
          
          if(test[2,4] > .05 && drop.insignificant){
              lm.avgs = lm(var.mean ~ 1, data=avgs)
              intercepts = rbind(intercepts, lm.avgs$coefficients[1])
              slopes = rbind(slopes, 0)
          } else {
            slopes = rbind(slopes, lm.avgs$coefficients[2])
            intercepts = rbind(intercepts, lm.avgs$coefficients[1])        
          }
          
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
        normalized.estimates = rbind(normalized.estimates, NA)
        next
      }
      
      df = as.data.frame(ts.window)
      avg.discharge = mean(df$Discharge)
      min.discharge = min(df$Discharge)
      max.discharge = max(df$Discharge)
      q25.discharge = quantile(df$Discharge, na.rm=TRUE)[2]
      q75.discharge = quantile(df$Discharge, na.rm=TRUE)[4]
      q90.discharge = quantile(df$Discharge, probs=.9, na.rm=TRUE)[1]
      
      print(max.discharge)
      
      if(plot) {
        plot(df$Discharge, df[,variable], xlab='Discharge (m^3/s)', ylab='HPOA Flux (mg/s)', 
             xlim=c(0,4), 
             ylim=c(0, maxy),
             col = rainbow(6)[.indexyear(ts.window)-110],
             main = paste(max, 'to', min, i))
        lines(avgs$discharge.mean, predict(lm.avgs), col = 'red', lwd=3)
      }
      
      times = index(tshval[tshval$Temperature >= (temperature.target-target.window/2) & tshval$Temperature < (temperature.target + target.window/2) & is.na(tslval.nomax$Temperature)])
      new.values = FALSE
      if(length(times) > 0 && nrow(ts.testing[times,]) > 0) {
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
      normalizing.discharges.1 = rbind(normalizing.discharges, avg.discharge, min.discharge, max.discharge, q25.discharge, q75.discharge, q90.discharge)
      normalized.estimate = predict(lm.avgs, normalizing.discharges.1)
      normalized.estimates = rbind(normalized.estimates, normalized.estimate)
      
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
  print(paste('Number of point estimates', nrow(point.estimates)))
  rmserror = rmse(point.estimates$actual, point.estimates$predicted)
  rval = list(intercepts, slopes, rmserror, rmses, classes,ttests, point.estimates, normalized.estimates)
  names(rval) = c('Intercepts', 'Slopes', 'RMSE', 'RMSES', 'Classes', 'TTests', 'Point.Estimates', 'Normalized.Estimates')
  return(rval)
}
