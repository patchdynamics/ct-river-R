#source('running.regression.function.R')

par(mfrow=c(2,1))

timeseries = list(ts.stemp$SoilTemperature1, ts.stemp$SoilTemperature2, tstemp, ts.water.temp$WaterTemperature)
names = c('Soil Temp 1', 'Soil Temp 2', 'Air Temp', 'Water Temp')
windows = c(10,10,10,10)
ts.training = tscopy  #### tscopy - HURRICANE IN tscopy.no.hurricane
ts.training$Discharge = ts.training$Discharge * 28.3168 / 1000 / 1000 # place in untils of Ml

j=2
  normalizing.discharges = c(20000,40000,60000,80000)
  normalizing.discharges = c(8490.0, 13600.0, 22600.0, 38900.0, 99000.0, 119992.3, 137992.3 )

  variable = 'HPOA.mgl'
  window = windows[j]
  #par(mfrow=c(2,1))
  rval = running.regression(ts.training, ts.training, timeseries[[j]], window, variable, 
                            mode = 1, maxy = 4, 
                            plot=FALSE, method='all.points',
                            prewhite = FALSE, drop.insignificant = FALSE,
                            normalizing.discharges = normalizing.discharges  * 28.3168 / 1000 / 1000 # place in untils of Ml
  )
  rval[[3]]
  plot(rval$Slopes, xaxt='n')
  axis(1, at=10*(0:floor(nrow(normalized) / 10)),  labels=rval$Classes[1+10*(0:floor(nrow(normalized) / 10))])

  
  par(mfrow=c(1,1))
  normalized.len = length(normalizing.discharges)
  ramp.colors = colorRampPalette(c("lightblue", "darkblue"))( length(normalized.len) )
  normalized = rval$Normalized.Estimates
  normalized[is.na(rval$RMSES),] = NA  # the predictions where we have no actual value for RMSE to compare are not informative, and not really part of the model
  plot(normalized[,1], typ='l', col=ramp.colors[1], main='[HPOA] Response', 
       ylab= '[HPOA] (mg/l)', xaxt = "n", xlab='Soil Temperature Class (K)',
       ylim=c(1,4.5), lwd=2)
  for(i in 2:normalized.len){
    lines(normalized[,i], col=ramp.colors[i], lwd=2)
  }
  lines(normalized[,normalized.len], col=rainbow(5)[5], lwd=3, lty=2)
  #lines(normalized[,6], col=rainbow(5)[5], lwd=3, lty=3)
  lines(normalized[,7], col=rainbow(5)[5], lwd=2, lty=3)
  lines(normalized[,8], col=rainbow(5)[5], lwd=2, lty=4)
  lines(normalized[,9], col=rainbow(5)[5], lwd=2, lty=4)
  lines(normalized[,10], col=rainbow(5)[5], lwd=1, lty=5)


  axis(1, at=10*(0:floor(nrow(normalized) / 10)),  labels=rval$Classes[1+10*(0:floor(nrow(normalized) / 10))])
  legend('bottom', legend=c('20000 cfs', '40000 cfs', '60000 cfs', '80000 cfs',
                            'Mean Discharge', 'Max Discharge', '25% & 75% quantile', '90% quantile'),
         lwd=c(2,2,2,2,3,2,2,2,1), lty=c(1,1,1,1,2,3,3,4,4,5),
         col=cbind(rainbow(5), rainbow(5)[5], rainbow(5)[5]), cex=.7, ncol=3, bty = "n")
  


#Flux

timeseries = list(ts.stemp$SoilTemperature1, ts.stemp$SoilTemperature2, tstemp, ts.water.temp$WaterTemperature)
names = c('Soil Temp 1', 'Soil Temp 2', 'Air Temp', 'Water Temp')
windows = c(10,10,10,10)
ts.training = tscopy  #### tscopy - HURRICANE IN tscopy.no.hurricane
ts.training$Discharge = ts.training$Discharge * 28.3168 / 1000 / 1000 # place in untils of Ml

j=2

variable = 'flux.mgs'
window = windows[j]
#par(mfrow=c(2,1))
rval = running.regression(ts.training, ts.training, timeseries[[j]], window, variable, 
                          mode = 1, maxy = 4, 
                          plot=FALSE, method='all.points',
                          normalizing.discharges = c(20000,40000,60000,80000) * 28.3168 / 1000 / 1000 # place in untils of Ml
)
rval[[3]]

scale = .028 / (1000 * .001) / 1000

par(mfrow=c(1,1))
normalized = rval$Normalized.Estimates
normalized[is.na(rval$RMSES),] = NA
plot(normalized[,1]*scale, typ='l', col=rainbow(5)[1], main='HPOA Flux Response', 
     ylab= 'HPOA Flux (kg/s)', xaxt = "n", xlab='Soil Temperature Class (K)',
     ylim=c(-50,300), lwd=2)
lines(normalized[,2]*scale, col=rainbow(5)[2], lwd=2)
lines(normalized[,3]*scale, col=rainbow(5)[3], lwd=2)
lines(normalized[,4]*scale, col=rainbow(5)[4], lwd=2)
lines(normalized[,5]*scale, col=rainbow(5)[5], lwd=3, lty=2)
#lines(normalized[,6], col=rainbow(5)[5], lwd=3, lty=3)
lines(normalized[,7]*scale, col=rainbow(5)[5], lwd=2, lty=3)
lines(normalized[,8]*scale, col=rainbow(5)[5], lwd=2, lty=4)
lines(normalized[,9]*scale, col=rainbow(5)[5], lwd=2, lty=4)
lines(normalized[,10]*scale, col=rainbow(5)[5], lwd=1, lty=5)


axis(1, at=10*(0:floor(nrow(normalized) / 10)),  labels=rval$Classes[1+10*(0:floor(nrow(normalized) / 10))])
legend('bottom', legend=c('20000 cfs', '40000 cfs', '60000 cfs', '80000 cfs',
                          'Mean Discharge', 'Max Discharge', '25% & 75% quantile', '90% quantile'),
       lwd=c(2,2,2,2,3,2,2,2,1), lty=c(1,1,1,1,2,3,4,4,5),
       col=cbind(rainbow(5), rainbow(5)[5], rainbow(5)[5]), cex=.7, ncol=3, bty = "n")
