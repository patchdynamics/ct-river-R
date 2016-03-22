timeseries.bin.and.average = function(ts.season, variable, bin.number = 100) {
  ts.ordered = as.data.frame(ts.season)[order(ts.season$Discharge),]
  ts.ordered = ts.ordered[!is.na(ts.ordered$Discharge) & !is.nan(ts.ordered$Discharge),]
  ts.ordered = ts.ordered[!is.na(ts.ordered[,variable]) & !is.nan(ts.ordered[,variable]),]
  ts.ordered = ts.ordered[ts.ordered$Discharge > 0,]
  
  df = data.frame(
    dates = as.Date(rownames(ts.ordered)),
    discharge = ts.ordered$Discharge, 
    var = ts.ordered[,variable])
  
  #df$bin = cut2(df$discharge, g=bin.number)
  df$bin = cut2(df$discharge,  1:bin.number  * 100000/bin.number)
  avgs = ddply(df, ~bin, summarise, discharge.mean=mean(discharge), var.mean=mean(var), n=length(var))
  return(avgs)
}

rising.step = function(timeseries) {
  processed = timeseries[,1]
  minimum = min(timeseries[,1])
  highest = minimum
  for(i in 1:nrow(timeseries)){
    if(.indexyday(timeseries[i]) == 0) {
      highest = minimum
    }
    if(as.numeric(timeseries[i,1]) > as.numeric(highest)){
      highest = timeseries[i,1]
    }
    processed[i,1] = highest
  }
  return(processed)
}


falling.step = function(timeseries){
  processed = timeseries[,1]
  minimum = min(timeseries[,1])
  highest = minimum
  for(i in nrow(timeseries[,1]):1){
    if(.indexyday(timeseries[i]) == 0) {
      highest = minimum
    }
    if(as.numeric(timeseries[i,1]) > as.numeric(highest)){
      highest = timeseries[i,1]
    }
    processed[i] = highest
  }
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

