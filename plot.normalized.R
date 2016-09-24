#source('running.regression.function.R')

par(mfrow=c(2,1))

timeseries = list(ts.stemp$SoilTemperature1, ts.stemp$SoilTemperature2, tstemp, ts.water.temp$WaterTemperature)
names = c('Soil Temp 1', 'Soil Temp 2', 'Air Temp', 'Water Temp')
windows = c(10,10,10,10)
ts.training = tscopy  #### tscopy - HURRICANE IN tscopy.no.hurricane
ts.training$Discharge = ts.training$Discharge * 28.3168 / 1000 / 1000 # place in untils of Ml

j=2
  normalizing.discharges = c(20000,40000,60000,80000)  # cfs
  normalizing.discharges = c(8490.0, 13600.0, 22600.0, 38900.0, 99000.0, 119992.3, 137992.3 ) # cfs
  probs = c(.25, .5, .75, .9, 1-1/365, 1-1/(5*365), 1-1/(10*365))

  normalizing.discharges = c(8490.0, 22600.0, 38900.0, 70000, 99000.0) #, 119992.3 )
  probs = c(.25, .75, .9, .98, 1-1/365) #, 1-1/(5*365))


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
  ramp.colors = colorRampPalette(c("lemonchiffon3", "turquoise3"))( normalized.len )
  normalized = rval$Normalized.Estimates
  normalized[is.na(rval$RMSES),] = NA  # the predictions where we have no actual value for RMSE to compare are not informative, and not really part of the model
  plot(normalized[,1], typ='l', col=ramp.colors[1], main='[HPOA] Response', 
       ylab= '[HPOA] (mg/l)', xaxt = "n", xlab='Soil Temperature Class (K)',
       ylim=c(1,4.5), lwd=2)
  for(i in 2:normalized.len){
    lines(normalized[,i], col=ramp.colors[i], lwd=2)
  }
  stats.color = 'black'
  lines(normalized[,normalized.len + 1], col=stats.color, lwd=1, lty=2)
  #lines(normalized[,6], col=rainbow(5)[5], lwd=3, lty=3)
  lines(normalized[,normalized.len + 3], col=stats.color, lwd=2, lty=3)
  lines(normalized[,normalized.len + 4], col=stats.color, lwd=3, lty=4)
  lines(normalized[,normalized.len + 5], col=stats.color, lwd=3, lty=4)
  lines(normalized[,normalized.len + 6], col=stats.color, lwd=1, lty=5)

  axis(1, at=10*(0:floor(nrow(normalized) / 10)),  labels=rval$Classes[1+10*(0:floor(nrow(normalized) / 10))])
  middle = max(rval$Classes)
  middle.at = which(rval$Classes == middle)
  axis(1, at=middle.at, labels=middle, lwd=3)

  legend('bottom', legend=c('25% Exceedence', '75% Exceedence', '90% Exceedence', '95% Exceedence', 
                            '1 year flood', '5 year flood',
                            'Mean Discharge', 'Max Discharge', '25% & 75% quantile', '90% quantile'),
         lwd=c(2,2,2,2,3,2,2,2,1), lty=c(1,1,1,1,2,3,3,4,4,5),
         col=cbind(rainbow(5), rainbow(5)[5], rainbow(5)[5]), cex=.7, ncol=3, bty = "n")
 
plot(1, typ='n',  ylab= '[HPOA] (mg/l)', xaxt = "n", xlab='Soil Temperature Class (K)',
     ylim=c(1,4.5),xlim=c(0,length(normalized[,normalized.len + 1])-8))
lines(normalized[,normalized.len + 1], col=stats.color, lwd=1, lty=2)
#lines(normalized[,6], col=rainbow(5)[5], lwd=3, lty=3)
lines(normalized[,normalized.len + 3], col=stats.color, lwd=2, lty=3)
lines(normalized[,normalized.len + 4], col=stats.color, lwd=3, lty=4)
lines(normalized[,normalized.len + 5], col=stats.color, lwd=3, lty=4)
lines(normalized[,normalized.len + 6], col=stats.color, lwd=1, lty=5)



# Just the exceedences
par(mfrow=c(1,1))
normalized.len = length(normalizing.discharges)
ramp.colors = colorRampPalette(c("lemonchiffon3", "turquoise3"))( normalized.len )
normalized = rval$Normalized.Estimates
normalized[is.na(rval$RMSES),] = NA  # the predictions where we have no actual value for RMSE to compare are not informative, and not really part of the model
plot(normalized[,1], typ='l', col=ramp.colors[1], main='[HPOA] Response', 
     ylab= '[HPOA] (mg/l)', xaxt = "n", xlab='Soil Temperature Class (K)',
     ylim=c(1,4.5), lwd=2)
for(i in 2:normalized.len){
  lines(normalized[,i], col=ramp.colors[i], lwd=2)
}
axis(1, at=10*(0:floor(nrow(normalized) / 10)),  labels=rval$Classes[1+10*(0:floor(nrow(normalized) / 10))])
middle = max(rval$Classes)
middle.at = which(rval$Classes == middle)
axis(1, at=middle.at, labels=middle, lwd=3)
  legend('bottom', legend=c('25% Exceedence', '75% Exceedence', '90% Exceedence', '95% Exceedence', 
                          '1 year flood', '5 year flood'),
         lwd=c(2,2,2,2,2,2), lty=c(1,1,1,1,1,1),
         col=ramp.colors, cex=.7, ncol=3, bty = "n",
         title='Discharge Levels')

# stats on contours

legend('bottom', legend=c(
                          'Mean Discharge', 'Max Discharge', '25% & 75% quantile', '90% quantile'),
       lwd=c(1,2,2,1), lty=c(2,3,4,5),
       col=stats.color, cex=1, ncol=2, bty = "n")

#Flux

timeseries = list(ts.stemp$SoilTemperature1, ts.stemp$SoilTemperature2, tstemp, ts.water.temp$WaterTemperature)
names = c('Soil Temp 1', 'Soil Temp 2', 'Air Temp', 'Water Temp')
windows = c(10,8,10,10)
ts.training = tscopy  #### tscopy - HURRICANE IN tscopy.no.hurricane
ts.training$Discharge = ts.training$Discharge * 0.0283168  # place in units of m^3
plot(as.numeric(ts.training$Discharge), as.numeric(ts.training$flux.mgs)/1000)

j=2

variable = 'flux.mgs'
window = windows[j]
#par(mfrow=c(2,1))
rval = running.regression(ts.training, ts.training, timeseries[[j]], window, variable, 
                          mode = 1, maxy = 4, 
                          plot=FALSE, method='all.points',
                          normalizing.discharges = normalizing.discharges * 0.0283168 # place in untils of Ml
)
rval[[3]]

#scale = .028 / (1000 * .001) / 1000  # actually discharge has already been scaled
scale = 1 / 1000 / 1000 # should scale to kilograms

par(mfrow=c(1,1))
normalized = rval$Normalized.Estimates
normalized[is.na(rval$RMSES),] = NA
plot(normalized[,1]*scale, typ='l', col=ramp.colors[1], main='HPOA Flux Response', 
     ylab= 'HPOA Flux (kg/s)', xaxt = "n", xlab='Soil Temperature Class (K)',
     ylim=c(-50,10000)*scale* 1000, 
     lwd=2)
for(i in 2:length(normalizing.discharges)){
  lines(normalized[,i]*scale, col=ramp.colors[i], lwd=2)
}
stats.color = 'black'
plot(normalized[,normalized.len + 1]*scale, col=stats.color, lwd=1, lty=2, typ='l',
     ylim=c(-50,10000)*scale*1000, main='HPOA Flux Response', 
     ylab= 'HPOA Flux (kg/s)', xaxt = "n", xlab='Soil Temperature Class (K)',)
#lines(normalized[,normalized.len + 1]*scale, col=stats.color, lwd=1, lty=2)

#lines(normalized[,6], col=rainbow(5)[5], lwd=3, lty=3)
lines(normalized[,normalized.len + 3]*scale, col=stats.color, lwd=2, lty=3)
lines(normalized[,normalized.len + 4]*scale, col=stats.color, lwd=3, lty=4)
lines(normalized[,normalized.len + 5]*scale, col=stats.color, lwd=3, lty=4)
lines(normalized[,normalized.len + 6]*scale, col=stats.color, lwd=1, lty=5)

middle = max(rval$Classes)
middle.at = which(rval$Classes == middle)
at = 10*(0:floor(nrow(normalized) / 10))
labels = rval$Classes[1+10*(0:floor(nrow(normalized) / 10))]
axis(1, at=at, labels=labels)
axis(1, at=middle.at, labels=middle, lwd=3)
legend('bottom', legend=c('20000 cfs', '40000 cfs', '60000 cfs', '80000 cfs',
                          'Mean Discharge', 'Max Discharge', '25% & 75% quantile', '90% quantile'),
       lwd=c(2,2,2,2,3,2,2,2,1), lty=c(1,1,1,1,2,3,4,4,5),
       col=cbind(rainbow(5), rainbow(5)[5], rainbow(5)[5]), cex=.7, ncol=3, bty = "n")


#Just the Exceedences
par(mfrow=c(1,1))
normalized.len = length(normalizing.discharges)
ramp.colors = colorRampPalette(c("lemonchiffon3", "turquoise3"))( normalized.len )
normalized = rval$Normalized.Estimates
normalized[is.na(rval$RMSES),] = NA  # the predictions where we have no actual value for RMSE to compare are not informative, and not really part of the model
plot(normalized[,1]*scale, typ='l', col=ramp.colors[1],  main='HPOA Flux Response', 
     ylab= 'HPOA Flux (kg/s)', xaxt = "n", xlab='Soil Temperature Class (K)',
     ylim=c(-50,300), lwd=2)
for(i in 2:normalized.len){
  lines(normalized[,i]*scale, col=ramp.colors[i], lwd=2)
}
axis(1, at=10*(0:floor(nrow(normalized) / 10)),  labels=rval$Classes[1+10*(0:floor(nrow(normalized) / 10))])
middle = max(rval$Classes)
middle.at = which(rval$Classes == middle)
axis(1, at=middle.at, labels=middle, lwd=3)
legend('bottom', legend=c('25% Exceedence', '75% Exceedence', '90% Exceedence', '95% Exceedence', 
                          '1 year flood', '5 year flood'),
       lwd=c(2,2,2,2,2,2), lty=c(1,1,1,1,1,1),
       col=ramp.colors, cex=.7, ncol=3, bty = "n",
       title='Discharge Levels')


# Stats
legend('bottom', legend=c(
  'Mean Discharge', 'Max Discharge', '25% & 75% quantile', '90% quantile'),
  lwd=c(1,2,2,1), lty=c(2,3,4,5),
  col=stats.color, cex=1, ncol=2, bty = "n")





# Annotated

timeseries = list(ts.stemp$SoilTemperature1, ts.stemp$SoilTemperature2, tstemp, ts.water.temp$WaterTemperature)
ts.training = tscopy  #### tscopy - HURRICANE IN tscopy.no.hurricane
ts.training$Discharge = ts.training$Discharge * 28.3168 / 1000 / 1000 # place in untils of Ml
variable = 'flux.mgs'
window = 10
normalizing.discharges = c(10,30)
#par(mfrow=c(2,1))
rval = running.regression(ts.training, ts.training, timeseries[[2]], window, variable, 
                          mode = 1, maxy = 4, 
                          plot=FALSE, method='all.points',
                          normalizing.discharges = normalizing.discharges * 28.3168 / 1000 / 1000 # place in untils of Ml
)
rval[[3]]
length(normalizing.discharges)
normalized.len = length(normalizing.discharges)

#scale = .028 / (1000 * .001) / 1000
scale = 1/1000/1000

par(mfrow=c(1,1))
normalized = rval$Normalized.Estimates
normalized[is.na(rval$RMSES),] = NA


# 1 avg
# 2 min
# 3 max
# 4 25
# 5 75
# 6 95
# 7 99

stats.color = 'black'
plot(normalized[,normalized.len + 1]*scale, col=stats.color, lwd=1, lty=2, typ='l',
     ylim=c(-50,10000)*scale* 1000, main='HPOA Flux Response', 
     ylab= 'HPOA Flux (kg/s)', xaxt = "n", xlab='Soil Temperature Class (K)',)


png(filename="Fig7.png", width=764, height=600)
plot(1, type="n", ylim=c(-1500,10000)*scale* 1000, main='HPOA Flux Response', 
     ylab= 'HPOA Flux (kg/s)', xlim=c(0,length(normalized[,normalized.len + 1])-8), 
     xaxt = "n", xlab='Soil Temperature Class (°C)' )

#lines(normalized[,6], col=rainbow(5)[5], lwd=3, lty=3)
#lines(normalized[,normalized.len + 3]*scale, col=stats.color, lwd=2, lty=3)
lines(normalized[,normalized.len + 4]*scale, col=stats.color, lwd=3, lty=4)
lines(normalized[,normalized.len + 5]*scale, col=stats.color, lwd=3, lty=4)
lines(normalized[,normalized.len + 7]*scale, col=stats.color, lwd=1, lty=5)
# max
#lines(normalized[,normalized.len +3]*scale, col='blue', lwd=1, lty=5)

middle = max(rval$Classes)
middle.at = which(rval$Classes == middle)

at = 10*(0:ceiling(nrow(normalized) / 10))
#at = which(rval$Classes %in% c( middle - 2))
#at = which(rval$Classes %in% c(middle - 2, middle + 7, middle - 7,
#                               middle + 12, middle - 12, middle + 17, middle - 17))
at = c(middle.at-7, middle.at+8, middle.at-12, middle.at+13,
       middle.at-17, middle.at+18, middle.at-22, middle.at+23, middle)
labels = rval$Classes[at] - 273
#axis(1, at=1:nrow(rval$Classes), labels=FALSE)
axis(1, at=at, labels=labels)
axis(1, at=middle.at+.5, labels=middle - 273, lwd=3)

# Stats
legend('bottom', legend=c(
  #'Mean Discharge', 
  #'Max Discharge', 
  '25% & 75% quantile', '99% quantile'),
  lwd=c(#1,
        #2,
        2,1), lty=c(#2,
                    #3,
                   # 1,1)
                    4,5),
  col=stats.color, cex=1, ncol=2, bty = "n")
dev.off()

