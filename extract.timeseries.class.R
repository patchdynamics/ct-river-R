
extract.timeseries.class = function(timeseries, class, window, tshval, tslval, tshval.nomax, tslval.nomax) {
  print(class)
  rising.next = ceil(min(tshval, na.rm=TRUE))
  falling.next = floor(max(tslval, na.rm=TRUE))
  rising.classes = (max(tshval$Temperature, na.rm=TRUE) - min(tshval$Temperature, na.rm=TRUE))
  falling.classes = (max(tslval$Temperature, na.rm=TRUE) - min(tslval$Temperature, na.rm=TRUE))
  if( class <= rising.classes){
    next.t = min(tshval$Temperature, na.rm=TRUE) + class    
    
    if( next.t + window < max(tshval, na.rm=TRUE) ) {
      min = next.t
      max = min + window
      temperature.target = min + window/2
      times = index(tshval[tshval$Temperature >= min & tshval$Temperature <= max & is.na(tslval.nomax$Temperature)])
      print(paste('1',min, max))
    } else if( next.t <= max(tshval, na.rm=TRUE) ){        
      rising.min = next.t
      falling.min = falling.next - (class + window - (max(tshval$Temperature, na.rm=TRUE) - min(tshval$Temperature, na.rm=TRUE)))
      if(rising.min + window/2 <= max(tshval, na.rm=TRUE)){
        temperature.target = rising.min + window/2
      } else {
        temperature.target = -falling.min + window/2    
      }
      times = index(tshval[(tshval$Temperature >= rising.min & is.na(tslval.nomax$Temperature)) | (tslval$Temperature >= falling.min &  is.na(tshval.nomax$Temperature))])    
      print(paste('2',rising.min, falling.min))
    }    
  } else if(class <= falling.classes + rising.classes) {
    max = max(tslval$Temperature, na.rm=TRUE) - (class - (max(tshval$Temperature, na.rm=TRUE) - min(tshval$Temperature, na.rm=TRUE)))
    min = max - window
    temperature.target = max - window/2
    if(min > min(tslval$Temperature, na.rm=TRUE)) {
      times = index(tshval[!is.na(tslval.nomax$Temperature) & tslval$Temperature <= max & tslval$Temperature >= min ])
      print(paste('3',max, min))
    } else {
      #need to wrap
      rising.max = rising.next + (class + window - (rising.classes + falling.classes) )
      times = index(tshval[tslval$Temperature <= max | tshval$Temperature <= rising.max])
      print(paste('4',max, rising.max))
    }
    
  } else {
    return(0)
  }
  
  rval = timeseries[times]
  return(rval)
}

ts.temperature = ts.stemp$SoilTemperature2
ts.temperature = ts.water.temp$WaterTemperature 
tshval = rising.step(ts.temperature)
tslval = falling.step(ts.temperature)
tslval.nomax = temp = tslval
temp$Temperature[is.na(temp$Temperature)] = min(temp$Temperature, na.rm=TRUE)
tslval.nomax[rollmax(temp$Temperature,150,align='center',fill='extend') == tslval] = NA
tshval.nomax = temp = tshval
temp$Temperature[is.na(temp$Temperature)] = min(temp$Temperature, na.rm=TRUE)
tshval.nomax[rollmax(temp$Temperature,150,align='center',fill='extend') == tshval] = NA

dev.new()
ts.prepare.extract = tscopy[!is.na(tscopy$Discharge) & !is.nan(tscopy$Discharge) ]
rising.classes = (max(tshval$Temperature, na.rm=TRUE) - min(tshval$Temperature, na.rm=TRUE))
falling.classes = (max(tslval$Temperature, na.rm=TRUE) - min(tslval$Temperature, na.rm=TRUE))
par(mfrow=c(4,4))
for(i in 1:(rising.classes+falling.classes)){
  ts.extracted = extract.timeseries.class(ts.prepare.extract, i, 36, tshval, tslval, tshval.nomax, tslval.nomax)
#  plot(ts.extracted$HPOA.mgl, main=i)
  df = as.data.frame(ts.extracted)
  plot(df$Discharge, df$HPOA.mgl, ylim=c(0,4), xlim=c(0,100000),         main=test)
  lm.avgs = lm(HPOA.mgl ~ poly(Discharge, 1, raw=TRUE), data=df)
  test = coeftest(lm.avgs, NeweyWest(lm.avgs, prewhite = TRUE))[2,4]
  lines(df$Discharge, predict(lm.avgs, newdata=df), col=c('red', 'blue')[as.numeric(test < .5) + 1]
)
}

df = as.data.frame(ts.extracted)
plot(df$Discharge, df$HPOA.mgl)
