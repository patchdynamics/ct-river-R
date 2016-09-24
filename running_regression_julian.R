library(reshape2)
library(ggplot2)
library(parallel)
source('multiplot.R')

#save(tscopy, file='tscopy.withflux.Rdata')
#tscopy.original = tscopy
tscopy = tscopy.original
#tscopy$flux.mgs[tscopy$flux.mgs > 6000000] = NA

# fold are years
# leave a whole year out each fold

tscopy$julian = .indexyday(tscopy)
tscopy$fold = .indexyear(tscopy) - min(.indexyear(tscopy))
ts.prepare.1 = tscopy[!is.na(tscopy$flux.mgs) & !is.na(tscopy$Discharge)]
ts.prepare.1$Discharge = ts.prepare.1$Discharge  * 0.028316 # cubic meters per second
df.prepare.1 = as.data.frame(ts.prepare.1)
window = 30
nbins = 20


#k = 4
#df.prepare.1 = as.data.frame(ts.prepare.1)[sample(1:nrow(ts.prepare.1)),]
#folds = cut(seq(1,nrow(df.prepare.1)),breaks=k,labels=FALSE)

#plot(ts.prepare$flux.mgs, ylim=c(0,10000000))

k = 5
rvals = NULL
variable = 'HPOA.mgl'
for(i in 1:k) {
  ts.training = df.prepare.1[df.prepare.1$fold!=i,]
  ts.testing = df.prepare.1[df.prepare.1$fold==i,]
  rval = running.regression.julian(ts.training, ts.testing, variable, window=54, mode=1)
  rvals = rbind(rvals, rval)
  if(i==1){
    plot(rval[[2]])
  } else {
    points(rval[[2]], pch=i)
  }
}
mean(as.numeric(rvals[,3]))



# try range of windows
windows = c(90,80,70,60,50,40,30,26,20,16,10,8,6,4,2)
errors.rmse = NULL
for(w in windows){
  k = 5
  rvals = NULL
  variable = 'HPOA.mgl'
  for(i in 1:k) {
    ts.training = df.prepare.1[df.prepare.1$fold!=i,]
    ts.testing = df.prepare.1[df.prepare.1$fold==i,]
    rval = running.regression.julian(ts.training, ts.testing, variable, window=w, mode=1)
    rvals = rbind(rvals, rval)
    #if(i==1){
    #  plot(rval[[2]])
    #} else {
    #  points(rval[[2]], pch=i)
    #}
  }
  errors.rmse=rbind(errors.rmse, mean(as.numeric(rvals[,3])))
}
plot(windows, errors.rmse)
#plot(HPOA.mgl ~ julian, df, col=rainbow(5)[.indexyear(ts.prepare.1)-110])


ts.prepare.1$Discharge = ts.prepare.1$Discharge * 28.3168 / 1000 / 1000 # place in untils of Ml

rval = running.regression.julian(ts.prepare.1, ts.prepare.1, variable, window=54, mode=3)


rval = running.regression.julian(ts.prepare.1, ts.prepare.1, variable, window=96, mode=2)


plot(rval$Slopes* 10000,col=c('red', 'blue')[as.numeric(rval$Ttests < .05) + 1])
#plot(rval[[2]]*10000)


par(mfrow=c(3,3))
for(w in 1:180){
  rval = running.regression.julian(ts.prepare.1, ts.prepare.1, variable, window=w, mode=1)  
  plot(rval$Classes, rval$Slopes* 10000,
       col=c('red', 'blue')[as.numeric(rval$Ttests < .05) + 1],
       main=paste("Window: ", w)
  )
}

rvals = lapply((1:90)*2, FUN=function(w){
  rval = running.regression.julian(ts.prepare.1, ts.prepare.1, variable, window=w, mode=1)  
  return(rval)  
})

library(parallel)
cores = 3
cluster <- makeCluster(cores, type="FORK")
rvals = parLapply(cluster, (1:90)*2, function(w){
  rval = running.regression.julian(ts.prepare.1, ts.prepare.1, variable, window=w, mode=1)  
  return(rval)  
})
stopCluster(cluster)
par(mfrow=c(4,4))
for(i in 1:90){
  rval = rvals[[i]]
  plot(rval$Classes, rval$Slopes* 10000,
       col=c('red', 'blue')[as.numeric(rval$Ttests < .05) + 1],
       main=paste("Window:", i * 2, 'RMSE:', rval$RMSE))
}

windows = (1:80)*2
windows = (1:40)*4
errors.rmse = NULL
for(w in 1:length(windows)) {
  print(windows[w])
  combinations = combn(5,4)
  cluster <- makeCluster(3, type="FORK")
  rmses = parLapply(cluster, 1:ncol(combinations), function(i){
    folds = combinations[,i]
    ts.training = ts.prepare.1[ts.prepare.1$fold %in% folds]
    ts.testing = ts.prepare.1[!(ts.prepare.1$fold %in% folds)]
    rval = running.regression.julian(ts.training, ts.testing, variable, windows[w], mode = 1)
    return(rval[[3]])  
  }) 
  stopCluster(cluster)  
  errors.rmse = rbind(errors.rmse, mean(as.numeric(rmses)))
}
plot(windows, errors.rmse, typ='l')
points(windows, errors.rmse)





running.regression.julian = function(ts.training, ts.testing, variable, window = 30, mode=1, 
                                     plot = FALSE, prewhite = TRUE) {
  
  ts.prepare = ts.training
  names(ts.prepare)[which(names(ts.prepare) == variable)] = 'variable'
  
  normalized.julian = point.estimates = intercepts = slopes = classes = ttests = NULL
  if(mode == 1){
    i.start = 1
    i.stop = 365
  } else if(mode == 2){
    i.start = 1
    i.stop = 180
  } else if(mode == 3){
    i.start = 181
    i.stop = 365
  }
  for( i in i.start:i.stop){
    low = i
    high = i + window
    if(high > 365){
      high = high - 365
      ts.process = ts.prepare[ts.prepare$julian >= low | ts.prepare$julian <= high,]
    } else {
      ts.process = ts.prepare[ts.prepare$julian >= low & ts.prepare$julian <= high,]
    }
    df.process = as.data.frame(ts.process)
    #ts.process$bin = cut2(ts.process$Discharge, g=nbins)
    #avgs = ddply(as.data.frame(ts.process), ~bin, summarise, discharge.mean=mean(Discharge), var.mean=mean(variable), n=length(variable))
    #lm.avgs = lm( var.mean ~ poly(discharge.mean, 1), data=avgs)
    lm.avgs = lm(variable ~ Discharge, as.data.frame(ts.process))
    test = coeftest(lm.avgs, NeweyWest(lm.avgs, prewhite=prewhite))
    intercepts = rbind(intercepts, lm.avgs$coefficients[1])
    slopes = rbind(slopes, lm.avgs$coefficients[2])
    ttests = rbind(ttests, test[2,4])
    
    
    julian.target = i + window/2
    if(julian.target > 365) {
      julian.target = julian.target - 365
    }
    classes = rbind(classes, julian.target)
    ts.target = ts.testing[ts.testing$julian == julian.target,] 
    if(nrow(ts.target) == 0){
      next
    }
    df = as.data.frame(ts.target)                                 # and i guess for k fold you just skip when there is no target value
    
    df.process = as.data.frame(ts.process)
    #lot(df.process$Discharge, df.process$flux.mgs,
    #     xlab='Discharge (cfs)', ylab='HPOA Flux (mg/s)', 
    #     xlim=c(0,100000), 
    #     ylim=c(0, 8000000)
    #)
    # p = predict(lm.avgs)
    #points(df$Discharge, df$flux.mgs, col='blue', pch=15)
    #lines(avgs$discharge.mean, p, col='red')

    
    #predicted = predict(lm.avgs, newdata = data.frame(discharge.mean = df$Discharge))
    
    predicted = predict(lm.avgs, newdata = df)
    p = predict(lm.avgs)
    
    if(plot) {
      plot(df.process$Discharge, df.process$variable, xlim=c(0,100000), ylim=c(0,4))
      lines(df.process$Discharge, p, col='red')
    }
    #points(df$Discharge, predicted, col='coral2')
    
    point.estimates.target = cbind(df[,variable], predicted)
    point.estimates  = rbind(point.estimates, point.estimates.target)
  }
  
  
  point.estimates = data.frame(actual = point.estimates[,1], predicted = point.estimates[,2])
  point.estimates = point.estimates[!is.na(point.estimates$actual),]
  rmserror = rmse(point.estimates$actual, point.estimates$predicted)
  #print(paste0('RMSE: ', sprintf("%.1f",rmserror / 1000),' g/s'))
  print(paste0('RMSE: ', rmserror ))
  
  # par(mfrow=c(1,1))
  # normalized ideas
  # normalized.data = data.frame(discharge.mean = c(50000))
  #normalized.data = data.frame(Discharge = c(50000))
  #normalized.julian[i] = predict(lm.avgs, newdata = normalized.data)[1]  
  #plot(normalized.julian/1000, ylab='g/s')#, ylim=c(0,6000000))
  #dff = as.data.frame(ts.prepare)
  #points(dff$julian, dff$variable, col='coral')
  
  rval = list(intercepts, slopes, rmserror, rmses, classes, ttests)
  names(rval) = c('Intercepts', 'Slopes', 'RMSE', 'RMSErrors', 'Classes', 'Ttests')
  return(rval)
}


