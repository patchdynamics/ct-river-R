timeseries.bin.and.average = function(ts.season, variable, bin.number = 100) {
  ts.ordered = as.data.frame(ts.season)[order(ts.season$Discharge),]
  ts.ordered = ts.ordered[!is.na(ts.ordered$Discharge) & !is.nan(ts.ordered$Discharge),]
  ts.ordered = ts.ordered[!is.na(ts.ordered[,variable]) & !is.nan(ts.ordered[,variable]),]
  ts.ordered = ts.ordered[ts.ordered$Discharge > 0,]
  
  df = data.frame(
    dates = as.Date(rownames(ts.ordered)),
    discharge = ts.ordered$Discharge, 
    var = ts.ordered[,variable])
  
  df$bin = cut2(df$discharge, g=bin.number)
  #df$bin = cut2(df$discharge,  1:bin.number  * 100000/bin.number)
  avgs = ddply(df, ~bin, summarise, discharge.mean=mean(discharge), var.mean=mean(var), n=length(var))
  return(avgs)
}

circular = function(timeseries){
  processed = timeseries[,1]
  max.timeseries = max(timeseries[,1])
  max.index = match(max(timeseries[1:365,1]), timeseries[,1])
  min.index = match(min(timeseries[1:365,1]), timeseries[,1])
  side.sign = sign(min.index-max.index)
  
  l = nrow(timeseries)
  for( i in 1:l){
    window.r = min(i+160, l)
    window.l = max(1, i-160)
  
    if(timeseries[i,1] == min(timeseries[window.l:window.r,1])){
      side.sign = 1 #  'rising'
    } else if(timeseries[i,1] == max(timeseries[window.l:window.r, 1])){
      side.sign = -1 # 'falling'
    }
    processed[i,1] = side.sign * ( timeseries[i,1] - max.timeseries )  
  }
  processed = processed + ( max.timeseries - min(timeseries[,1]))
  return(processed)
}

moving.window = function(circular.timeseries, window, step=1){
  l = nrow(circular.timeseries)
  max.timeseries = max(circular.timeseries[,1], na.rm=TRUE)
  loop2 = circular.timeseries
  loop2[,1] = loop2[,1] + max.timeseries
  looped.timeseries = rbind(circular.timeseries, loop2)
  
  par(mfrow=c(1,1))
  for(i in seq(0,max.timeseries, step)){
    min = i
    max = (min + window) #%% max.timeseries
    times = index(looped.timeseries[looped.timeseries[,1] >= min
                                      & looped.timeseries[,1] < max
                                      ])
    subset = circular.timeseries[times]
    plot(subset) 
  }
}

translate.classes = function(classes, original.timeseries){
  max.timeseries = max(original.timeseries[,1])
  min.timeseries = min(original.timeseries[,1])
  classes.centered = classes - ( max.timeseries - min.timeseries)
  classes.translated = classes.centered
  classes.translated[classes.centered <= 0] = max.timeseries + classes.centered[classes.centered <= 0] 
  classes.translated[classes.centered > 0]  = max.timeseries - classes.centered[classes.centered > 0]
  return(classes.translated)
}

rising.step = function(timeseries) {
  processed = timeseries[,1]
  minimum = min(timeseries[,1])
  highest = NA
  l = nrow(timeseries)
  for(i in 1:l){
    #if(.indexyday(timeseries[i]) == 0) {
    #  highest = minimum
    #}
    window.r = min(i+160, l)
    window.l = max(1, i-160)
    if(timeseries[i,1] == min(timeseries[window.l:window.r,1])){
      if(i+160 > l){
        highest = NA
      } else {
        highest = minimum
      }
    }
    if(!is.na(highest)){
      if(as.numeric(timeseries[i,1]) > as.numeric(highest)){
        highest = timeseries[i,1]
      }
      processed[i,1] = highest
    } else {
      processed[i,1] = NA
    }
  }
  names(processed) = 'Temperature'
  return(processed)
}


falling.step = function(timeseries){
  processed = timeseries[,1]
  minimum = min(timeseries[,1])
  highest = NA
  l = nrow(timeseries[,1])
  
  for(i in l:1){
#    if(.indexyday(timeseries[i]) == 0) {
#      highest = minimum
#    }
    window.r = min(i+160, l)
    window.l = max(1, i-160)
    if(timeseries[i,1] == min(timeseries[window.l:window.r,1])){
      highest = minimum
    }
    if(!is.na(highest)) {
      if(as.numeric(timeseries[i,1]) > as.numeric(highest)){
        highest = timeseries[i,1]
      }
      processed[i] = highest
    } else {
      processed[i] = NA
    }
  }
  names(processed) = 'Temperature'
  return(processed)
}


flux.avg = function(ts.season, bin.number = 100) {
  ts.ordered = as.data.frame(ts.season)[order(ts.season$Discharge),]
  ts.ordered = ts.ordered[!is.na(ts.ordered$Discharge) & !is.nan(ts.ordered$Discharge),]
  ts.ordered = ts.ordered[!is.na(ts.ordered$HPOA.mgl) & !is.nan(ts.ordered$HPOA.mgl),]
  ts.ordered = ts.ordered[ts.ordered$Discharge > 0,]
  #ts.ordered = ts.ordered[!is.na(ts.ordered$flux.mgs),]  #???
  
  lndf = data.frame(
    dates = as.Date(rownames(ts.ordered)),
    discharge = ts.ordered$Discharge, 
    flux = ts.ordered$flux.mgs,
    lndischarge = log(ts.ordered$Discharge), 
    lnflux = log(ts.ordered$flux.mgs))
  
  lndf$bin = cut2(lndf$discharge, g=bin.number)
  avgs = ddply(lndf, ~bin, summarise, discharge.mean=mean(discharge), flux.mean=mean(flux), n=length(flux))
  return(avgs)
}

