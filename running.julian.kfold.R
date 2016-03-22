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
k = 2
ts.prepare.1$t = .indexday(ts.prepare.1)
ts.prepare.1$group = .indexyear(ts.prepare.1)
df.prepare.1 = as.data.frame(ts.prepare.1)[sample(1:nrow(ts.prepare.1)),]
folds = cut(seq(1,nrow(df.prepare.1)),breaks=k,labels=FALSE)  #THIS IS NOT RANDOM NO GOOD!!!
rmserrors = NULL
for(i in 1:k) {
  df.training = df.prepare.1[folds!=i,]
  df.testing = df.prepare.1[folds==i,]
  rmserrors = rbind(rmserrors, running.regression.julian2(df.training, df.testing, window=30))
}
mean(rmserrors / 1000)

running.regression.julian2 = function(df.training, df.testing, window = 30) {
  variable = "flux.mgs"  
  
  normalized.julian = NULL
  point.estimates = NULL
  par(mfrow=c(2,2))
  for( i in 1:180){
    low = i
    high = i + window
    if(high > 365){
      high = high - 365
      df.training.window = df.training[df.training$julian >= low | df.training$julian <= high,]
      df.testing.window = df.testing[df.testing$julian >= low | df.testing$julian <= high,]
    } else {
      df.training.window = df.training[df.training$julian >= low & df.training$julian <= high,]
      df.testing.window = df.testing[df.testing$julian >= low & df.testing$julian <= high,]
    }
    #ts.process$bin = cut2(ts.process$Discharge, g=nbins)
    #avgs = ddply(as.data.frame(ts.process), ~bin, summarise, discharge.mean=mean(Discharge), flux.mean=mean(flux.mgs), n=length(flux))
    #lm.avgs = lm( flux.mean ~ poly(discharge.mean, 1), data=avgs)
     
    df.training.window = df.training.window[!is.na(df.training.window[variable]) & !is.na(df.training.window$Discharge),]    
    gls.model.ar1 = gls( as.formula(paste(variable,'~ Discharge')),
                         df.training.window,
                         correlation=corARMA(p=1, form= ~t | group))
    
    p = predict(gls.model.ar1)
    
    
    normalized.data = data.frame(Discharge = c(50000))
    normalized.julian[i] = predict(gls.model.ar1, newdata = normalized.data)[1]  
    
    julian.target = i + window/2
    if(julian.target > 365) {
      julian.target = julian.target - 365
    }
    
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
    
    point.estimates.target = cbind(df.testing.window$flux.mgs, predict(gls.model.ar1, newdata=df.testing.window))
    point.estimates  = rbind(point.estimates, point.estimates.target)
  }
  
  par(mfrow=c(1,1))
  plot(normalized.julian)#, ylim=c(0,6000000))
  lines(normalized.julian, col='green')
  
  
  point.estimates = data.frame(actual = point.estimates[,1], predicted = point.estimates[,2])
  point.estimates = point.estimates[!is.na(point.estimates$actual),]
  rmserror = rmse(point.estimates$actual, point.estimates$predicted)
  return(rmserror)
  
}



