running.regression.circular = function(ts.training, 
                                       #ts.testing,  # do the testing elsehwere 
                                       ts.classifier, 
                                       training.window, 
                                       variable, 
                                       #mode = 1, 
                                       step=1, 
                                       target.window = 1
                                       #maxy=80000, plot=TRUE,
                                       #normalizing.discharges = c(200000,40000,60000,80000),
                                       #drop.insignificant = FALSE
                                       ){
  
  ts.values = ts.training[!is.na(ts.training$Discharge) & !is.na(ts.training[,variable]),]  # can be extracted
  
  l = nrow(circular.timeseries)
  max.timeseries = max(circular.timeseries[,1], na.rm=TRUE)
  loop2 = circular.timeseries
  loop2[,1] = loop2[,1] + max.timeseries
  looped.timeseries = rbind(circular.timeseries, loop2)
  
  lms = list()
  
  #par(mfrow=c(2,1))
  classes = seq(0, max.timeseries, step)
  for(i in 1:length(classes)){
    min = classes[i]
    max = (min + training.window)
    times = index(looped.timeseries[looped.timeseries[,1] >= min
                                    & looped.timeseries[,1] < max
                                    ])
    ts.window = ts.values[times,]
   # plot(ts.window, main=i)
    df.window = as.data.frame(ts.window)
    if(nrow(df.window) == 0) next
   # plot(df.window$Discharge, df.window$HPOA.mgl, ylim=c(0,3.5))
    subset = data.frame(dependent = df.window[,variable], discharge = df.window$Discharge)
    # plot(subset) 
    
    lm.subset = lm(dependent ~ poly(discharge, 1, raw=TRUE), data=subset)
    lms[[i]] = lm.subset
    # move the tests elsewhere
    # test = coeftest(lm.avgs, NeweyWest(lm.avgs, prewhite = prewhite))
    # ttests = rbind(ttests, test[2,4])
  }
  return(lms)
}

get.slopes = function(lms){  
  slopes = sapply(lms, function(lm){
    return(lm$coefficients[2])
  }, simplify='array')
  return(slopes)
}

get.pvalues = function(lms){
  pvalues = sapply(lms, function(lm){
    return(coeftest(lm, NeweyWest(lm))[2,4])   # this is returning p values that are way to big
  })
  return(pvalues)
}

get.classes = function(circular.timeseries, step){
  return(seq(0, max(circular.timeseries, na.rm=TRUE), step)) 
}

get.rmse = function(ts.testing, circular.timeseries, lms, training.window, prediction.window=1, variable, step=1){
  ts.testing.values = ts.testing[!is.na(ts.testing$Discharge) & !is.na(ts.testing[,variable]),]  # can be extracted
  
  l = nrow(circular.timeseries)
  max.timeseries = max(circular.timeseries[,1], na.rm=TRUE)
  loop2 = circular.timeseries
  loop2[,1] = loop2[,1] + max.timeseries
  looped.timeseries = rbind(circular.timeseries, loop2)
  classes = seq(0, max.timeseries, step)
  predicted.values = observed.values = NULL
  for(i in 1:length(classes)){
    min = classes[i] + training.window/2 - prediction.window/2
    max = min + prediction.window
    times = index(looped.timeseries[looped.timeseries[,1] >= min
                                    & looped.timeseries[,1] < max
                                    ])
    ts.window = ts.testing.values[times,]
    df.window = as.data.frame(ts.window)
    if(nrow(df.window)==0) next;
    predictions = predict(lms[[i]], data.frame(discharge = df.window$Discharge))
    predicted.values = append(predicted.values, predictions)
    observed.values = append(observed.values, df.window[,variable])                        
    #rmse.prediction = rmse(df.window[,variable], predictions)
    #rmses = rbind(rmses, rmse.prediction)        
  }
  rmse = rmse(observed.values, predicted.values)
  plot(observed.values)
  points(predicted.values, col='orange')
  return(rmse)
}

evaluate = function(ts.training, timeseries, training.window, prediction.window, variable){
  circular.timeseries = circular(timeseries)
  lms = running.regression.circular(ts.training, circular.timeseries, training.window, variable)
  plot(get.slopes(lms), col=c('red','blue')[ (get.pvalues(lms)<.05) + 1], ylim=c(0,1)) 
  get.rmse(ts.training, circular.timeseries, lms, training.window, prediction.window, variable)
}


#cross.validate = function(ts.training,
#                          ts.classifier,
#                          window, 
#                          variable, 
#                          #mode = 1, 
#                          step=1, 
#                          target.window = 1, 

circular.timeseries = circular(ts.stemp$SoilTemperature2)
lms = running.regression.circular(ts.training, circular.timeseries, 8, 'HPOA.mgl')
plot(get.slopes(lms), col=c('red','blue')[ (get.pvalues(lms)<.05) + 1], ylim=c(0,1)) 
get.rmse(ts.training, circular.timeseries, lms, 8, 1, 'HPOA.mgl')

classes.indices = get.classes(circular.timeseries, 1)
classes = translate.classes(classes.indices, ts.stemp$SoilTemperature2)




# Soil T 1
circular.timeseries = circular(ts.stemp$SoilTemperature1)
training.window = 16
prediction.window = 1
lms = running.regression.circular(ts.training, circular.timeseries, training.window, 'HPOA.mgl')
plot(get.slopes(lms), col=c('red','blue')[ (get.pvalues(lms)<.05) + 1], ylim=c(0,1)) 
get.rmse(ts.training, circular.timeseries, lms, training.window, prediction.window, 'HPOA.mgl')


classes.indices = get.classes(circular.timeseries, 1)

moving.window(circular.timeseries, 72)

# Air 
evaluate(ts.training, tstemp, 14, 1, 'HPOA.mgl')

# Water
evaluate(ts.training, ts.water.temp, 20, 1, 'HPOA.mgl')

circular.timeseries = circular(ts.water.temp)
plot(circular.timeseries)

#NDVI
#step = 100, target.window = 250
step = 100  # this actually needs to be passed through to everything, which is annoy.
training.window = 1500
prediction.window = 100
evaluate(ts.training, ts.ndvi, 250, 250, 'HPOA.mgl' )
circular.timeseries = circular(ts.ndvi)
moving.window(circular.timeseries, training.window, step=100)

lms = running.regression.circular(ts.training, circular.timeseries, training.window, 'HPOA.mgl', step=100)
plot(get.slopes(lms), col=c('red','blue')[ (get.pvalues(lms)<.05) + 1], ylim=c(-0.5,1.5)) 
get.rmse(ts.training, circular.timeseries, lms, training.window, prediction.window, 'HPOA.mgl', step=100)


