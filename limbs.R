library(DVstats) # usgs hysep code

tsfilled =  na.approx(tscopy$Discharge)
timestamps = index(tsfilled)
df1.zoo<-zoo(, seq(timestamps[1],timestamps[length(timestamps)],by="day")) #set date to Index
merge(tsfilled, df1.zoo)
tsfilled =  na.approx(tsfilled$Discharge)
hysep.out = hysep(as.vector(tsfilled$Discharge), as.Date(index(tsfilled)), da=10870.706, STAID='01193050')

plot(hysep.out$Flow, typ='l')
lines(hysep.out$Sliding, col='red')
lines(hysep.out$Sliding + 5000, col='coral1')


#range = '2011-01-01/2016-01-01'
#discharge = tsfilled['2011-01-01/2016-01-01']
discharge = tsfilled
discharge.diff = diff.xts(discharge)
indexTZ(discharge.diff) = Sys.getenv("TZ")

#discharge.diff[abs(discharge.diff) < 2000] = 0
discharge.diff[ tsfilled$Discharge <  hysep.out$Sliding + 2000] = 0
discharge.diff[is.na(discharge.diff)] = -9990
discharge.diff[is.nan(discharge.diff)] = -9990
discharge.diff[discharge.diff < 0] = -1
discharge.diff[discharge.diff > 0] = 1
discharge.diff[discharge.diff == 0] = 0
names(discharge.diff) = 'Limb'
indexTZ(discharge.diff) = Sys.getenv("TZ")
tscopy2 = merge(tscopy, discharge.diff)


fullseries = tscombined['2011-01-01/2015-12-31']
nrow(fullseries)
nrow(discharge.diff)

fullseries = merge(fullseries, discharge.diff)
  
colors <- function(series) {
  
  year.color = function(year) { 
    color = switch(year,
           'blue', 'red', 'orange', 'purple', 'green'
    )
  }
  return(apply(as.data.frame(.indexyear(series)-110), 1, month.color))
  
}

par(mfrow=c(1,1))

par(mfrow=c(3,4))

above = 6 - 1  # not indexing starts with 0
below = 9 - 1
subsetseries = tscopy2[.indexmon(tscopy2) > above & .indexmon(tscopy2) < below]
discharge = na.approx(subsetseries$Discharge, na.rm=FALSE)
discharge = as.data.frame(discharge)
subset = as.data.frame(subsetseries)
head(subset)
plot(subset$Discharge, subset$HPOA.mgl, ylim=c(1,3.5), col=colors(subsetseries))

#x = subset$HPOA.mgl
#y = discharge$Discharge
#df = data.frame(x=x, y=y)
#lm.HPOA = lm(x~y, df)
#summary(lm.HPOA)
#discharge$Discharge[is.na(discharge$Discharge)] = mean(discharge$Discharge)
#newdata = data.frame(x=seq(min(discharge$Discharge),max(discharge$Discharge),100))
#lines(newdata$x, lm.HPOA$coefficients[1] + lm.HPOA$coefficients[2] * newdata$x,
#      #predict(lm.HPOA, newdata),
#      col='red')

#lines(x, predict(lm.HPOA), col='red')

subset = as.data.frame(tscopy2)
par(mfrow=c(2,2))
rising = subset[subset$Limb == 1,]
plot(rising$Discharge/1000, rising$HPOA.mgl, ylim=c(1,3.5), xlim=c(0,100000)/1000,
     ylab = 'mg/l', xlab='Discharge (cfs/1000)', main='Rising Limb', col='coral4')
falling = subset[subset$Limb == -1,]
plot(falling$Discharge/1000, falling$HPOA.mgl, ylim=c(1,3.5), xlim=c(0,100000)/1000,
     ylab = 'mg/l', xlab='Discharge (cfs/1000)', main='Falling Limb', col='coral4')
lowflow = subset[subset$Limb == 0,]
plot(lowflow$Discharge/1000, lowflow$HPOA.mgl, ylim=c(1,3.5), xlim=c(0,100000)/1000,
     ylab = 'mg/l', xlab='Discharge (cfs/1000)', , main='Low Flow', col='coral4')
storm = rbind(rising, falling)
plot(storm$Discharge/1000, storm$HPOA.mgl, ylim=c(1,3.5), xlim=c(0,100000)/1000,
     ylab = 'mg/l', xlab='Discharge (cfs/1000)', , main='Storm', col='coral4')


#collection.dates = strptime(as.character(fractionation$Collection.Date), '%m/%d/%y')
#fractionation_series = xts(x=fractionation, order.by=collection.dates)
#series = fractionation_series[!is.na(index(fractionation_series))]
#head(series)
#series = merge(discharge.diff, series)
