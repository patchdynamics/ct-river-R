
tsfilled =  na.approx(tscopy$Discharge)

range = '2011-01-01/2016-01-01'
discharge = tsfilled['2011-01-01/2016-01-01']
discharge = tsfilled
discharge.diff = diff.xts(discharge)
discharge.diff[abs(discharge.diff) < 2000] = 0
discharge.diff[is.na(discharge.diff)] = -9990
discharge.diff[is.nan(discharge.diff)] = -9990


par(mfrow=c(2,1))
plot(discharge)
plot(discharge.diff)

discharge.diff[discharge.diff < 0] = -1
discharge.diff[discharge.diff > 0] = 1
discharge.diff[discharge.diff == 0] = 0
names(discharge.diff) = 'Limb'

indexTZ(discharge.diff) = Sys.getenv("TZ")
plot(discharge.diff)
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

above = 5 - 1  # not indexing starts with 0
below = 7 - 1
subsetseries = fullseries[.indexmon(fullseries) > above & .indexmon(fullseries) < below]
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


par(mfrow=c(2,2))
rising = subset[summer$Limb == 1,]
plot(rising$Discharge/1000, rising$HPOA.mgl, ylim=c(1,3.5), xlim=c(0,100000)/1000)
falling = subset[summer$Limb == -1,]
plot(falling$Discharge/1000, falling$HPOA.mgl, ylim=c(1,3.5), xlim=c(0,100000)/1000)
baseflow = subset[summer$Limb == 0,]
plot(baseflow$Discharge/1000, baseflow$HPOA.mgl, ylim=c(1,3.5), xlim=c(0,100000)/1000)
storm = rbind(rising, falling)
plot(storm$Discharge/1000, storm$HPOA.mgl, ylim=c(1,3.5), xlim=c(0,100000)/1000)


#collection.dates = strptime(as.character(fractionation$Collection.Date), '%m/%d/%y')
#fractionation_series = xts(x=fractionation, order.by=collection.dates)
#series = fractionation_series[!is.na(index(fractionation_series))]
#head(series)
#series = merge(discharge.diff, series)
