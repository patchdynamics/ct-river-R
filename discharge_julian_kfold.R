library(reshape2)
library(ggplot2)
source('multiplot.R')

#save(tscopy, file='tscopy.withflux.Rdata')
#tscopy.original = tscopy
tscopy = tscopy.original
#tscopy$flux.mgs[tscopy$flux.mgs > 6000000] = NA

tscopy$julian = .indexyday(tscopy)
ts.prepare.1 = tscopy[!is.na(tscopy$flux.mgs) & !is.na(tscopy$Discharge)]
window = 30
nbins = 20



k = 4
df.prepare.1 = as.data.frame(ts.prepare.1)[sample(1:nrow(ts.prepare.1)),]
folds = cut(seq(1,nrow(df.prepare.1)),breaks=k,labels=FALSE)

#plot(ts.prepare$flux.mgs, ylim=c(0,10000000))

rmserrors = NULL
for(i in 1:k) {
  ts.training = df.prepare.1[folds!=i,]
  ts.testing = df.prepare.1[folds==i,]
  rmserrors = rbind(rmserrors, running.regression.julian(ts.training, ts.testing, window=4))
}
mean(rmserrors / 1000)

running.regression.julian = function(ts.training, ts.testing, window = 30) {

  ts.prepare = ts.training
  
normalized.julian = NULL
point.estimates = NULL
par(mfrow=c(2,2))
for( i in 1:180){
  low = i
  high = i + window
  if(high > 365){
    high = high - 365
    ts.process = ts.prepare[ts.prepare$julian >= low | ts.prepare$julian <= high,]
  } else {
    ts.process = ts.prepare[ts.prepare$julian >= low & ts.prepare$julian <= high,]
  }
  ts.process$bin = cut2(ts.process$Discharge, g=nbins)
  avgs = ddply(as.data.frame(ts.process), ~bin, summarise, discharge.mean=mean(Discharge), flux.mean=mean(flux.mgs), n=length(flux))
  lm.avgs = lm( flux.mean ~ poly(discharge.mean, 1), data=avgs)
  p = predict(lm.avgs)
  df.process = as.data.frame(ts.process)
  
  
  normalized.data = data.frame(discharge.mean = c(50000))
  normalized.julian[i] = predict(lm.avgs, newdata = normalized.data)[1]  
  
  julian.target = i + window/2
  if(julian.target > 365) {
    julian.target = julian.target - 365
  }
  ts.target = ts.testing[ts.testing$julian == julian.target,]  # and actually for k fold it would be the other data, not inclusive
  if(nrow(ts.target) == 0){
    next
  }
  df = as.data.frame(ts.target)                                 # and i guess for k fold you just skip when there is no target value
  
  #plot(df.process$Discharge, df.process$flux.mgs,
  #     xlab='Discharge (cfs)', ylab='HPOA Flux (mg/s)', 
  #     xlim=c(0,100000), 
  #     ylim=c(0, 8000000)
  #)
  #points(df$Discharge, df$flux.mgs, col='blue', pch=15)
  #lines(avgs$discharge.mean, p, col='red')
  
  
  predicted = predict(lm.avgs, newdata = data.frame(discharge.mean = df$Discharge))
  point.estimates.target = cbind(df$flux.mgs, predicted)
  point.estimates  = rbind(point.estimates, point.estimates.target)
}


point.estimates = data.frame(actual = point.estimates[,1], predicted = point.estimates[,2])
point.estimates = point.estimates[!is.na(point.estimates$actual),]
rmserror = rmse(point.estimates$actual, point.estimates$predicted)
print(paste0('RMSE: ', sprintf("%.1f",rmserror / 1000),' g/s'))

par(mfrow=c(1,1))
plot(normalized.julian/1000, ylab='g/s')#, ylim=c(0,6000000))
dff = as.data.frame(ts.prepare)
points(dff$julian, dff$flux.mgs, col='coral')

return(rmserror)
}


