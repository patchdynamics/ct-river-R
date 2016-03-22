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


# k fold checks for overfitting.
#kfold.10 = nrow(ts.prepare.1) - nrow(ts.prepare.1)/1.1
#ts.prepare = ts.prepare.1[sort(sample(1:nrow(ts.prepare.1), 100)),]
#plot(ts.prepare$flux.mgs, ylim=c(0,10000000))

ts.prepare = ts.prepare.1

normalized.julian = NULL
point.estimates = NULL
par(mfrow=c(2,2))
for( i in 1:180){
  low = i
  high = i + window
  if(high > 365){
    high = high - 365
    ts.window = ts.prepare[ts.prepare$julian >= low | ts.prepare$julian <= high]
  } else {
    ts.window = ts.prepare[ts.prepare$julian >= low & ts.prepare$julian <= high]
  }
  #ts.process$bin = cut2(ts.process$Discharge, g=nbins)
  #avgs = ddply(as.data.frame(ts.process), ~bin, summarise, discharge.mean=mean(Discharge), flux.mean=mean(flux.mgs), n=length(flux))
  #lm.avgs = lm( flux.mean ~ poly(discharge.mean, 1), data=avgs)
  
  df.process = as.data.frame(ts.window)
  df.process$t = .indexday(ts.window)
  df.process$group = .indexyear(ts.window)
  df.process = df.process[!is.na(df.process[variable]) & !is.na(df.process$Discharge),]    
  gls.model.ar1 = gls( as.formula(paste(variable,'~ Discharge')),
                       df.process,
                       correlation=corARMA(p=1, form= ~t | group))
  
  p = predict(gls.model.ar1)
  
  
  normalized.data = data.frame(Discharge = c(50000))
  normalized.julian[i] = predict(gls.model.ar1, newdata = normalized.data)[1]  
  
  julian.target = i + window/2
  if(julian.target > 365) {
    julian.target = julian.target - 365
  }
  ts.target = ts.prepare.1[ts.prepare.1$julian == julian.target]  # and actually for k fold it would be the other data, not inclusive
  df = as.data.frame(ts.target)                                 # and i guess for k fold you just skip when there is no target value
  
  if(0) {
  plot(df.process$Discharge, df.process$flux.mgs,
       xlab='Discharge (cfs)', ylab='HPOA Flux (mg/s)', 
       xlim=c(0,100000), 
       ylim=c(0, 8000000),
       main = i
  )
  points(df$Discharge, df$flux.mgs, col='blue', pch=15)
  lines(df.process$Discharge, p, col='red')
  }
  
  point.estimates.target = cbind(df$flux.mgs, predict(gls.model.ar1, newdata=df))
  point.estimates  = rbind(point.estimates, point.estimates.target)
}


point.estimates = data.frame(actual = point.estimates[,1], predicted = point.estimates[,2])
point.estimates = point.estimates[!is.na(point.estimates$actual),]
rmserror = rmse(point.estimates$actual, point.estimates$predicted)
rmserror

par(mfrow=c(1,1))
plot(normalized.julian)#, ylim=c(0,6000000))
lines(normalized.julian, col='green')
dff = as.data.frame(ts.prepare)
points(dff$julian, dff$flux.mgs, col='coral')


