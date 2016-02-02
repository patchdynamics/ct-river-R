

par(mfrow=c(3,4))

above = 7 - 1  # not indexing starts with 0
below = 9 - 1
subsetseries = fullseries[.indexmon(fullseries) > above & .indexmon(fullseries) < below]
discharge = na.approx(subsetseries$Discharge, na.rm=FALSE)
discharge = as.data.frame(discharge)
subset = as.data.frame(subsetseries)
head(subset)

par(mfrow=c(2,2))
rising = subset[summer$Limb == 1,]
plot(rising$Discharge/1000, rising$HPOA.mgl, ylim=c(1,3.5), xlim=c(0,100000)/1000)
falling = subset[summer$Limb == -1,]
plot(falling$Discharge/1000, falling$HPOA.mgl, ylim=c(1,3.5), xlim=c(0,100000)/1000)
baseflow = subset[summer$Limb == 0,]
plot(baseflow$Discharge/1000, baseflow$HPOA.mgl, ylim=c(1,3.5), xlim=c(0,100000)/1000)
storm = rbind(rising, falling)
plot(storm$Discharge/1000, storm$HPOA.mgl, ylim=c(1,3.5), xlim=c(0,100000)/1000)