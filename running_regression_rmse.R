library(reshape2)
library(ggplot2)
library(Hmisc)
library(plyr)
library(Metrics)
library(lmtest)
library(sandwich)
source('multiplot.R')
source('tools.R')

#save(tscopy, file='tscopy.withflux.Rdata')
#tscopy.original = tscopy
tscopy = tscopy.original
#tscopy$flux.mgs[tscopy$flux.mgs > 6000000] = NA


# ad hov remove hurricane
tscopy.no.hurricane = tscopy[!(.indexyear(tscopy) == 111 & .indexyday(tscopy) > 230 & .indexyday(tscopy) < 260) ]

ts.temperature = ts.water.temp$WaterTemperature
ts.temperature = tstemp
ts.temperature = ts.stemp$SoilTemperature1
main = 'Water Temp'
ts.values = tscopy
variable = 'HPOA.mgl'
window = 8

par(mfrow=c(2,1))
rval = running.regression(ts.values, ts.values, ts.temperature, window, variable, mode = 3, maxy = 4, 
                          plot=TRUE, method='all.points')
rval[[3]]

par(mfrow=c(2,2))
plot(rval[[4]], xlab='Temperature Class', ylab='RMSE of target set', main=rval[[3]], ylim=c(0,2))
ttest.colors = as.numeric(rval$TTests < .05) + 1
ttest.colors[rval[[2]] < .2] = 3
plot(rval$TTests, ylim=c(0,1), main=main, col=c('red', 'blue', 'orange')[ttest.colors])

#par(mfrow=c(2,1))
plot(rval[[1]], ylim=c(0,3), xlab='Temperature Class', ylab='Y Intercept')
ticks = c(20,40,60)
plot(rval[[2]], ylim=c(0,8), xlab='Temperature Class', ylab='Slope', 
     col=c('black', 'orange')[as.numeric(rval[[2]] < .2) + 1])
#     at=ticks,
#    labels=rval$Classes[ticks])


# all at once
timeseries = list(ts.stemp$SoilTemperature1, ts.stemp$SoilTemperature2, tstemp, ts.water.temp$WaterTemperature)
names = c('Soil Temp 1', 'Soil Temp 2', 'Air Temp', 'Water Temp')
windows = c(10,10,10,10)
ts.training = tscopy.no.hurricane  #### tscopy - HURRICANE IN tscopy.no.hurricane
ts.training$Discharge = ts.training$Discharge * 28.3168 / 1000 / 1000 # place in untils of Ml
for(j in 1:4){
  
  variable = 'HPOA.mgl'
  window = windows[j]
  par(mfrow=c(2,1))
  rval = running.regression(ts.training, ts.training, timeseries[[j]], window, variable, 
                            mode = 1, maxy = 4, 
                            plot=FALSE, method='all.points',
                            normalizing.discharges = c(20000,40000,60000,80000) * 28.3168 / 1000 / 1000 # place in untils of Ml
  )
  rval[[3]]
  
  par(mfrow=c(2,2))
  plot(rval[[4]], xlab='Temperature Class', ylab='RMSE of target set', main=rval[[3]], ylim=c(0,2))
  
  normalized = rval$Normalized.Estimates
  normalized[is.na(rval$RMSES),] = NA
  plot(normalized[,1], typ='l', col=rainbow(5)[1], main=names[j], 
       ylab= '[HPOA] (mg/l)', xaxt = "n", xlab='Temperature Class (K)',
       ylim=c(1,4), lwd=2)
  lines(normalized[,2], col=rainbow(5)[2], lwd=2)
  lines(normalized[,3], col=rainbow(5)[3], lwd=2)
  lines(normalized[,4], col=rainbow(5)[4], lwd=2)
  lines(normalized[,5], col=rainbow(5)[5], lwd=2)
  axis(1, at=10*(0:floor(nrow(normalized) / 10)),  labels=rval$Classes[1+10*(0:floor(nrow(normalized) / 10))])
  legend('bottom', legend=c('20000 cfs', '40000 cfs', '60000 cfs', '80000 cfs'),
         lwd=2, col=rainbow(4), cex=.7, ncol=2, bty = "n")
  
  #ttest.colors = as.numeric(rval$TTests < .05) + 1
  #ttest.colors[rval[[2]] < .2] = 3
  #plot(rval$TTests, ylim=c(0,1), main=names[[j]], col=c('red', 'blue', 'orange')[ttest.colors],
  #     ylab='t test p-value')
  
  #par(mfrow=c(2,1))
  plot(rval[[1]], ylim=c(0,3), xlab='Temperature Class', ylab='Y Intercept', col='blue') # always significant
  ticks = c(20,40,60)
  ttest.colors = cut(as.numeric(rval$TTests),  breaks=c(0,.001,.01,.05,.1,1))
  plot(rval[[2]], 
       ylim=c(0,2),
       xlab='Temperature Class', ylab='Slope', 
       #col=c('red', 'blue')[ttest.colors]
       col=c('blue', 'green', 'yellow', 'orange', 'red')[as.numeric(ttest.colors)]
  )
  #legend('topleft', 
  #       legend=c('significant', 'not significant', 'flat slope'),
  #       col=c('blue', 'red', 'orange'), pch=1
  #)
  
  par(mfrow=c(1,1))
  plot(rval$Point.Estimates[,1], typ='l', ylim=c(1,3.5))
  points(rval$Point.Estimates[,1])
  points(rval$Point.Estimates[,2], typ='l', col='blue')
  points(rval$Point.Estimates[,2], col='blue')
}


# get the warming and cooling phase values
#ts.temperature = ts.stemp$SoilTemperature2
ts.temperature = tstemp

running.regression(ts.stemp$SoilTemperature2, 2, 'flux.mgs')
running.regression(ts.stemp$SoilTemperature2, 2, 'flux.mgs', mode = 2)

rval = running.regression(ts.stemp$SoilTemperature2, 4, 'HPOA.mgl', 
                          mode = 2, maxy = 3, step=1, target.window=1)


# normalized
plot(rval$Intercepts + 20000 * rval$Slopes)

par(mfrow=c(1,1))
plot(rval$Point.Estimates[,1], typ='l')
points(rval$Point.Estimates[,1])
points(rval$Point.Estimates[,2], typ='l', col='blue')
points(rval$Point.Estimates[,2], col='blue')
rval[[3]]

# yearly folding
tscopy$fold = .indexyear(tscopy) - min(.indexyear(tscopy)) + 1
ts.full.values = tscopy
ts.temperature = ts.stemp$SoilTemperature1  # window = 10         10 +
ts.temperature = ts.stemp$SoilTemperature2  # window = 4 (6)      6+
ts.temperature = tstemp # window = 10                             4+ ( not stable, maybe b/c pred. are never significant ?)
ts.temperature = ts.water.temp$WaterTemperature # window = 6 (8)  10+ (12 or 14)
ts.temperature = ts.ndvi # window = 1250

main = 'Air Temp'
variable = 'HPOA.mgl'
windows = c(14,12,10,8,6,4,2)

windows = c(60,56,54,50,46,44,40,38,34,32,30,26,24,20,18,16,14,12,10,8,6,4,2)

windows = c(60,50,40,30,20,10)

k = 5
k.training = 4


#ndvi

windows = c(3000,2750,2500,2250,2000,1750,1500,1250,1000,750,500,250) # ndvi
windows = c(4000,3500,3000,2750,2500,2250,2000,1750)
k = 4
k.training = 2

windows = c(10,8,6,4,2)

ts.values.without.winter = ts.full.values[.indexmon(ts.full.values) > 1 &  .indexmon(ts.full.values)  < 11]

#
# Cross Validation Code
#
for(ts.temperature in list(ts.stemp$SoilTemperature1, ts.stemp$SoilTemperature2, tstemp, ts.water.temp$WaterTemperature )) {
print('nexttemp')
  tshval = rising.step(ts.temperature)
  tslval = falling.step(ts.temperature)
  tslval.nomax = temp = tslval
  temp$Temperature[is.na(temp$Temperature)] = min(temp$Temperature, na.rm=TRUE)
  tslval.nomax[rollmax(temp$Temperature,150,align='center',fill='extend') == tslval] = NA
  tshval.nomax = temp = tshval
  temp$Temperature[is.na(temp$Temperature)] = min(temp$Temperature, na.rm=TRUE)
  tshval.nomax[rollmax(temp$Temperature,150,align='center',fill='extend') == tshval] = NA

errors.rmse = NULL
for(w in 1:length(windows)) {
  print(w)
  combinations = combn(5,k.training)
  cluster <- makeCluster(3, type="FORK")
  rmses = parLapply(cluster, 1:ncol(combinations), function(i){
  #rmses = lapply(1:ncol(combinations), function(i){
      
    #folds = (c(-1,0,1) + i) %% 5 + 1
    folds = combinations[,i]
    print(i)
    ts.training = ts.full.values[ts.full.values$fold %in% folds]
    ts.testing = ts.full.values[!(ts.full.values$fold %in% folds)]
    rval = running.regression(ts.training, ts.testing,  ts.temperature, windows[w], variable, mode = 1, 
                              step = 100, target.window = 250, #ndvi
                              maxy=4,plot=FALSE, prewhite=FALSE,
                             tshval = tshval, tslval=tslval, tshval.nomax=tshval.nomax, tslval.nomax=tslval.nomax)
    return(rval[[3]])  
  }) 
  stopCluster(cluster)  
  errors.rmse = rbind(errors.rmse, mean(as.numeric(rmses)))
}
par(mfrow=c(1,1))
plot(windows, errors.rmse, typ='l')
points(windows, errors.rmse)

}

# check for significant bandwidth, run regression with all windows
rvals.all = list()
for(ts.temperature in list(ts.stemp$SoilTemperature1, ts.stemp$SoilTemperature2, tstemp, ts.water.temp$WaterTemperature )) {
  print('nexttemp')
  tshval = rising.step(ts.temperature)
  tslval = falling.step(ts.temperature)
  tslval.nomax = temp = tslval
  temp$Temperature[is.na(temp$Temperature)] = min(temp$Temperature, na.rm=TRUE)
  tslval.nomax[rollmax(temp$Temperature,150,align='center',fill='extend') == tslval] = NA
  tshval.nomax = temp = tshval
  temp$Temperature[is.na(temp$Temperature)] = min(temp$Temperature, na.rm=TRUE)
  tshval.nomax[rollmax(temp$Temperature,150,align='center',fill='extend') == tshval] = NA
  
cores = 3
cluster <- makeCluster(cores, type="FORK")
rvals = parLapply(cluster, 
                  #(1:50)*2,
                  windows,
                  function(window){
#rvals = lapply((1:15)*2, function(window){
  rval = running.regression(ts.full.values, ts.full.values, ts.temperature, window, variable, 
                            mode = 3, 
                            step = 100, target.window = 250, #ndvi
                            maxy=4,plot=FALSE,
                            tshval = tshval, tslval=tslval, tshval.nomax=tshval.nomax, tslval.nomax=tslval.nomax,
                            prewhite = FALSE, drop.insignificant=TRUE)
  
  return(rval)  
})
stopCluster(cluster)

rvals.all[[length(rvals.all)+1]] = rvals

}

for(rvals  in rvals.all){
  par(mfrow=c(4,4))
  for(i in 1:length(rvals)){
    rval = rvals[[i]]
    #twoord.plot(lx = 1:length(rval$Slopes), ly = rval$Slopes* 10000,
    #            rx = 1:length(rval$Slopes), ry = rval$Intercepts,
    #            lcol=c('red', 'blue')[as.numeric(rval$TTests < .05) + 1],
    #            main=paste("Window:", i * 2, 'RMSE:', rval$RMSE))
    plot(rval$Slopes* 10000,
         col=c('red', 'blue')[as.numeric(rval$TTests < .05) + 1],
         main=paste("Window:", 
                    #i * 2,
                    windows[i],
                    'RMSE:', rval$RMSE))  
  }
  
}

# Sensitivity Analysis
par(mfrow=c(1,1))
#plot(1, type="n", xlab="", ylab="", xlim=c(0, 100), ylim=c(.2, .6))
plot(1, type="n", xlab="", ylab="", xlim=c(0, 100), ylim=c(0, 1000000))

ranges = c(36, 24, 49, 30) * 2  # Air T is actually 49 degrees wide
for(i in 1:length(rvals.all)){
  num.windows = (ranges[i]/2)
  num.windows = 50
  rmses = NULL
  for(j in 1:num.windows){
    rmses = rbind(rmses, rvals.all[[i]][[j]]$RMSE)
  }
  
  points((1:num.windows)*2, rmses, col=rainbow(4)[i])
  lines((1:num.windows)*2, rmses, col=rainbow(4)[i])
}
legend('topright', legend=c('SoilT1', 'SoilT2', 'AirT', 'WaterT'), col=rainbow(4), cex=.8, lwd=1)


#ndvi sensitivity
rmses = NULL
par(mfrow=c(1,1))
for(j in 1:length(windows)){
  rmses = rbind(rmses, rvals[[j]]$RMSE)
}
plot(windows, rmses)
lines(windows, rmses)


##### OLD

errors.rmse = NULL
rmse.matrix = matrix(nrow=length(windows), ncol=k)
for(w in 1:length(windows)) {
  cluster <- makeCluster(3, type="FORK")
  rmses = parLapply(cluster, 1:k, function(i){
    folds = (c(-1,0,1) + i) %% 5 + 1
    ts.training = ts.full.values[ts.full.values$fold %in% folds]
    ts.testing = ts.full.values[!(ts.full.values$fold %in% folds)]
    rval = running.regression(ts.training, ts.testing,  ts.temperature, windows[w], variable, mode =2, 
                              # step = 100, target.window = 250, #ndvi
                              maxy=4,plot=FALSE)
    return(rval[[3]])  
  }) 
  stopCluster(cluster)  
  errors.rmse = rbind(errors.rmse, mean(as.numeric(rmses)))
  
#  for(i in 1:k) {
  #  folds = (c(-1,0,1) + i) %% 5 + 1
    #ts.training = ts.full.values[ts.full.values$fold!=i,]
    #%ts.testing = ts.full.values[ts.full.values$fold==i,]
   # ts.training = ts.full.values[ts.full.values$fold %in% folds]
  #  ts.testing = ts.full.values[!(ts.full.values$fold %in% folds)]
   # rval = running.regression(ts.training, ts.testing,  ts.temperature, windows[w], variable, mode = 2, 
                             # step = 100, target.window = 250, #ndvi
#                              maxy=4,plot=FALSE)
  #  print(rval[[3]])
  #  rmse.matrix[w,i] = rval[[3]]
  #  rvals = rbind(rvals, rval)
    if(i==1){
      #plot(rval[[2]], main=main)
    } else {
      #points(rval[[2]], pch=i)
    }
 # }
  #errors.rmse = rbind(errors.rmse, mean(as.numeric(rvals[,3])))
}

plot(windows, errors.rmse, typ='l')
points(windows, errors.rmse)

### END OLD ?


# for a range of windows
windows = c(16,10,8,6,4,2)
rvals = list()
for(i in 1:length(windows)){
  rval = running.regression(ts.values, ts.temperature , windows[i], variable, mode = 3, maxy = 4, plot=FALSE)
  rvals[[i]] = rval
  
  par(mfrow=c(1,1))
  plot(rval[[4]], xlab='Temperature Class', ylab='RMSE of target set', main=windows[i])
  
  #par(mfrow=c(2,1))
  #plot(rval[[1]], ylim=c(0,3), xlab='Temperature Class', ylab='Y Intercept', main=windows[i])
  #plot(rval[[2]], ylim=c(0,2), xlab='Temperature Class', ylab='Slope', main=windows[i])
  print(rval[[3]])
}

par(mfrow=c(1,1))
plot(rvals[[1]][[4]], col=rainbow(6)[1], typ='l')
lines(rvals[[2]][[4]], col=rainbow(6)[2])
lines(rvals[[3]][[4]], col=rainbow(6)[3])
lines(rvals[[4]][[4]], col=rainbow(6)[4])
lines(rvals[[5]][[4]], col=rainbow(6)[5])
lines(rvals[[6]][[4]], col=rainbow(6)[6])
legend(x='topleft', legend=windows, col=rainbow(6), pch=1)

rmses = NULL
for(i in 1:length(windows)){
  rmses = rbind(rmses, rvals[[i]][[3]])
}
plot(rmses)





















# NDVI adventure
par(mfrow=c(1,1))
ts.temperature = ts.ndvi
ts.values = tscopy
ts.values$Discharge = ts.values$Discharge * 28.3168 / 1000 / 1000 # place in untils of Ml
variable = 'HPOA.mgl'
window = 2750
step=150
target.window = 250

par(mfrow=c(4,4))
rval = running.regression(ts.values, ts.values, ts.temperature , 
                          window, variable, mode = 3, 
                          step=step, target.window = 250,
                          maxy = 4, plot=FALSE,
                          prewhite=FALSE, drop.insignificant=TRUE)
rval[[3]]
par(mfrow=c(1,1))
plot(rval$Slopes* 10000,
     col=c('red', 'blue')[as.numeric(rval$TTests < .05) + 1],
     main=paste("Window:", window, 'RMSE:', rval$RMSE))  

par(mfrow=c(2,2))
plot(rval[[4]], xlab='Temperature Class', ylab='RMSE of target set', main=rval[[3]], ylim=c(0,2))
ttest.colors = as.numeric(rval$TTests < .05) + 1
ttest.colors[rval[[2]] < .2] = 3
plot(rval$TTests, ylim=c(0,1), main=main, col=c('red', 'blue', 'orange')[ttest.colors])

#par(mfrow=c(2,1))
plot(rval[[1]], ylim=c(0,3), xlab='Temperature Class', ylab='Y Intercept')
ticks = c(20,40,60)
plot(rval[[2]], ylim=c(0,2), xlab='Temperature Class', ylab='Slope', 
     col=c('black', 'orange')[as.numeric(rval[[2]] < .2) + 1])


par(mfrow=c(1,1))
plot(rval[[4]], xlab='Temperature Class', ylab='RMSE of target set')



windows = c(3000,2500,2000,1500,1000,750,500,250,100,50)
rvals = list()
ts.temperature = ts.ndvi
ts.values = tscopy
ts.values$Discharge = ts.values$Discharge * 28.3168 / 1000 / 1000 # place in untils of Ml
rvals = list()
par(mfrow=c(4,4))
for(i in 1:length(windows)){
  rval = running.regression(ts.values, ts.values, ts.temperature , 
                            windows[i], variable, mode = 1, 
                            step=100, target.window = 250,
                            maxy = 4, plot=FALSE,
                            prewhite=FALSE, drop.insignificant=TRUE)
  rvals[[i]] = rval
  
  par(mfrow=c(2,1))
  ttest.colors = as.numeric(rval$TTests < .05) + 1
  ttest.colors[rval[[2]] < .2] = 3
  plot(rval[[1]], ylim=c(0,-400000),  xlab='Temperature Class', ylab='Y Intercept', main=windows[i])
  plot(rval[[2]], ylim=c(0,4000000),  xlab='Temperture Class', ylab='Slope', main=windows[i],  col=c('red', 'blue', 'orange')[ttest.colors])
  print(rval[[3]])
}

#  par(mfrow=c(1,1))
# plot(rval[[5]], rval[[4]], xlim=c(9000,3000), xlab='Temperature Class', ylab='RMSE of target set', main=windows[i])
par(mfrow=c(2,1))
ttest.colors = as.numeric(rval$TTests < .05) + 1
ttest.colors[rval[[2]] < .2] = 3
plot(rval[[1]], ylim=c(0,3),  xlab='Temperature Class', ylab='Y Intercept', main=windows[i])
plot(rval[[2]], ylim=c(0,2),  xlab='Temperture Class', ylab='Slope', main=windows[i],  col=c('red', 'blue', 'orange')[ttest.colors])
print(rval[[3]])



