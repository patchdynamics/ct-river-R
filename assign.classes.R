ts.temperature = ts.stemp$SoilTemperature1
tshval = rising.step(ts.temperature)
tslval = falling.step(ts.temperature)

tslval.nomax = temp = tslval
temp$Temperature[is.na(temp$Temperature)] = min(temp$Temperature, na.rm=TRUE)
tslval.nomax[rollmax(temp$Temperature,150,align='center',fill='extend') == tslval] = NA
tshval.nomax = temp = tshval
temp$Temperature[is.na(temp$Temperature)] = min(temp$Temperature, na.rm=TRUE)
tshval.nomax[rollmax(temp$Temperature,150,align='center',fill='extend') == tshval] = NA
  

window = 10
target.window = 1
start = floor(min(tshval, na.rm=TRUE)) - window/2  # add window/2 to center window
stop.t = max(tshval, na.rm=TRUE)

next.t = start
ts.assigning = tscopy
ts.assigning$Class = NA
while(next.t < stop.t){
  temperature.target = next.t
  
  if(next.t + window <= stop.t) { # The normal case
    times = index(tshval[tshval$Temperature >= (temperature.target - target.window/2) 
                         & tshval$Temperature < (temperature.target + target.window/2) 
                         & is.na(tslval.nomax$Temperature)])
  } #else if(temperature.target > 0){ # what is this supposed to capture ?
                                     # I believe it's redundant
                                     # this circumvents the wrapping code.
    #times = index(tshval[tshval$Temperature >= (temperature.target - target.window/2) 
    #                     & tshval$Temperature < (temperature.target + target.window/2)  
    #                     & is.na(tslval.nomax$Temperature)]) 
  } else {  # There is still some totally weird shit in here.
            # I do not believe this is wrapping.   This is indexing using tslval ???
    #times = index(tshval[tslval$Temperature >= (temperature.target - target.window/2) 
    #                     & tslval$Temperature < (temperature.target + target.window/2) ]) 
    
    # calculate how far we are from max
    to.max = max(tshval, na.rm=TRUE) - (temperature.target - target.window/2)
    falling.portion = target.window - to.max
    # clever but still not there, once target moves past max into the falling period it's messed up
    
    
    times = index(tshval[(tshval$Temperature >= (temperature.target - target.window/2)  
                            & is.na(tslval.nomax$Temperature)) 
                         | (tslval$Temperature >= max(tshval, na.rm=TRUE) - falling.portion 
                            & is.na(tshval.nomax$Temperature))])
    
  }
  if(length(times) > 0){  # this is one reason this method is a little weird, is there a simpler way ?
    ts.assigning[times,]$Class = temperature.target
  }
  next.t = next.t + 1
}

stop.t = min(tslval, na.rm=TRUE)
next.t = max(tslval, na.rm=TRUE)
while(next.t > stop.t){
  temperature.target = next.t
  
  if(next.t - window > stop.t) {
    times = index(tslval[tslval$Temperature >= (temperature.target-target.window/2) & tslval$Temperature < (temperature.target + target.window/2) & is.na(tshval.nomax$Temperature)])
  } else {
    #need to wrap
    # WHY IS THIS TSHVAL
    #times = index(tslval[tslval$Temperature <= max | tshval$Temperature <= rising.next])
    #rising.next = rising.next + 1
  }
  if(length(times) > 0){  
    ts.assigning[times,]$Class = temperature.target
  }
  next.t = next.t - 1
}



ts.assigning$Class = ts.assigning$Class - 273
plot(ts.assigning$Class)

df = as.data.frame(ts.assigning)
df.subset = df[df$Class < 9,]
plot(df.subset$Discharge, df.subset$HPOA.mgl, typ='p', ylim=c(1,3.5))

df.subset = df[df$Class > 8 & df$Class < 17,]
plot(df.subset$Discharge, df.subset$HPOA.mgl, typ='p', ylim=c(1,3.5))

df.subset = df[df$Class > 17,]
plot(df.subset$Discharge, df.subset$HPOA.mgl, typ='p', ylim=c(1,3.5))

