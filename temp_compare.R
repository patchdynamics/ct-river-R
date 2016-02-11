#  5 levels:
#  0 , 10 , 40 , 100 cm for all variables plus 800cm for soil temperature.

par(mfrow=c(1,1))
plot(as.data.frame(tstemp)$Temperature)
points(stemp$V4, col='red')
points(stemp$V5, col='blue')

min(stemp$V4[!is.na(stemp$V4)])
min(stemp$V5[!is.na(stemp$V5)])
min(stemp$V6[!is.na(stemp$V6)])
min(stemp$V7[!is.na(stemp$V7)])

ground.frozen = stemp[!is.na(stemp$V4),]
ground.frozen$level1frozen = 0
ground.frozen[ground.frozen$V4 < 273.15,]$level1frozen = 1
ground.frozen$level2frozen = 0
ground.frozen[ground.frozen$V5 < 273.15,]$level2frozen = 1
ground.frozen$level3frozen = 0
ground.frozen[ground.frozen$V5 < 273.15,]$level3frozen = 1
air.freezing = as.data.frame(tstemp)
air.freezing$belowzero = 0
air.freezing$belowzero[air.freezing$Temperature<273.15] = 1

par(mfrow=c(2,2))
plot(ground.frozen$level1frozen, type='l')
plot(ground.frozen$level2frozen, type='l')
plot(ground.frozen$level3frozen, type='l')
plot(air.freezing$belowzero, type='l')


par(mfrow=c(1,1))
plot(air.freezing$belowzero*.5, type='l')
lines(ground.frozen$level1frozen, type='l', col='red')
#lines(ground.frozen$level2frozen, type='l', col='blue')
#lines(ground.frozen$level3frozen, type='l', col='green')


