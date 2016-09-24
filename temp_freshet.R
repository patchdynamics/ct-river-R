library(grDevices)
library(lmtest)
library(sandwich)

# make separate graphs of all the freshets
temp.series = ts.stemp$SoilTemperature1
temp.series = tstemp
tshval = rising.step(temp.series)
tslval = falling.step(temp.series)
min = 281 # soil T 1
max = 290
#min= 273
#max = 280

times2 = index(tshval[tshval$Temperature >= min & tshval$Temperature < max])

#par(mfrow=c(1,1))
ts.subset = tscopy2[times2]
plot(ts.subset[.indexyear(ts.subset) == 115])
df = as.data.frame(ts.subset)
plot(df$Discharge, df$HPOA.mgl, ylim=c(1, 3.5), col=rainbow(5)[.indexyear(ts.subset) - 110])
lm.model = lm(HPOA.mgl ~ Discharge, df)
coeftest(lm.model, NeweyWest(lm.model))

plot(df$Discharge, df$HPOA.mgl, ylim=c(1, 3.5), 
     #col=rainbow(5)[.indexyear(ts.subset) - 110],
     col=c('red', 'green', 'blue')[df$Limb + 2])
dfl = df[df$Limb == -1,]
lm.model = lm(HPOA.mgl ~ Discharge, dfl)
summary(lm.model)
lines(lm.model$model$Discharge, predict(lm.model))

par(mfrow=c(1,5))
for (year in 2011:2015){
  ts2 = ts[times2]
  ts2 = ts2[paste0(as.character(year),'-01-01/', as.character(year),'-12-31')]
  
  ylimit = c(1,3)  # 3.5
  xlimit = c(0,100000) # 100000
  discharge = na.approx(ts2, na.rm=FALSE)
  discharge = as.data.frame(discharge)$Discharge
  colors = colorRampPalette(c("black", rainbow(5)[year-2010]))( length(as.vector(ts2$HPOA.mgl) ) )
  plot(discharge/1000, as.data.frame(ts2)$HPOA.mgl, ylim=ylimit,
         col=colors, 
         xlim=xlimit/1000,
         xlab='Discharge / 1000  cfs',
         ylab='[HPOA] ppm',
         typ='l'
         #main='Highest Avg Surface Temp   280K < T < 290K',
         #sub='Freshets Seem to Occur Exclusively in this Range'
  )
  s = 1:(length(discharge)-1)
  arrows(discharge[s]/1000, as.data.frame(ts2)$HPOA.mgl[s],
         discharge[s+1]/1000, as.data.frame(ts2)$HPOA.mgl[s+1],
       length=0.1,
       lwd=2,
       col=colors 
       #main='Highest Avg Surface Temp   280K < T < 290K',
       #sub='Freshets Seem to Occur Exclusively in this Range'
  )
  legend( x="topright", 
          #legend=month.abb[min(.indexmon(ts2)):max(.indexmon(ts2))],
          #col=rainbow(length(unique(.indexmon(ts2)))), 
          legend = as.character(unique(.indexyear(ts2)) + 1900),
          col= rainbow(5)[year-2010], 
          bty = "n",
          pch=1,
          inset=0.05)   
}



par(mfrow=c(1,1))
ylimit = c(1,3)  # 3.5
xlimit = c(0,100000) # 100000
plot(0, 0, typ='n', xlim=xlimit, ylim=ylimit)
for (year in 2011:2015){
  ts2 = ts[times2]
  ts2 = ts2[paste0(as.character(year),'-01-01/', as.character(year),'-12-31')]

  discharge = na.approx(ts2, na.rm=FALSE)
  discharge = as.data.frame(discharge)$Discharge
  colors = colorRampPalette(c("black", rainbow(5)[year-2010]))( length(as.vector(ts2$HPOA.mgl) ) )
  lines(discharge/1000, as.data.frame(ts2)$HPOA.mgl, ylim=ylimit,
       col=colors, 
       xlim=xlimit/1000,
       xlab='Discharge / 1000  cfs',
       ylab='[HPOA] ppm',
       typ='l'
       #main='Highest Avg Surface Temp   280K < T < 290K',
       #sub='Freshets Seem to Occur Exclusively in this Range'
  )
  s = 1:(length(discharge)-1)
  #arrows(discharge[s]/1000, as.data.frame(ts2)$HPOA.mgl[s],
  #       discharge[s+1]/1000, as.data.frame(ts2)$HPOA.mgl[s+1],
  #       length=0.1,
  #       lwd=2,
  #       col=colors 
  #       #main='Highest Avg Surface Temp   280K < T < 290K',
  #       #sub='Freshets Seem to Occur Exclusively in this Range'
  #)
  legend( x="topright", 
          #legend=month.abb[min(.indexmon(ts2)):max(.indexmon(ts2))],
          #col=rainbow(length(unique(.indexmon(ts2)))), 
          legend = as.character(unique(.indexyear(ts2)) + 1900),
          col= rainbow(5)[year-2010], 
          bty = "n",
          pch=1,
          inset=0.05)   
}





# let's look into the stats here
ts2 = ts[times2]
# actually let's not, it's hysteresis response, which 
# is more interesting that some grab bag regression
# claims



# bar graph
times2 = index(tshval[tshval$Temperature >= min & tshval$Temperature < max])
series = tshval[times2]
series$yearday = .indexyday(series)
series$year = .indexyear(series)

df = as.data.frame(series)
par(mfrow=c(1,1))
plot(df$yearday, df$year+1900, pch=15, cex=3,
     xlab='Day of Year', ylab='Year', xlim=c(0,365),
     col='red')

times2 = index(tshval[tslval$Temperature >= min & tslval$Temperature < max])
series = tshval[times2]
series$yearday = .indexyday(series)
series$year = .indexyear(series)
df = as.data.frame(series)
points(df$yearday, df$year+1900, pch=15, cex=3, col='blue')
legend('topleft',legend=c('Freshet', 'Autumn Cooling'), col=c('red', 'blue'),
       pch=15, cex=.9)

#par(mfrow=c(1,1))
ts.subset = tscopy[times2]
df = as.data.frame(ts.subset)
plot(df$Discharge, df$HPOA.mgl, ylim=c(1, 3.5), col=rainbow(5)[.indexyear(ts.subset) - 110],
     xlab='Discharge', ylab='HPOA')
lm.model = lm(HPOA.mgl ~ Discharge, df)
coeftest(lm.model, NeweyWest(lm.model, prewhite=FALSE))





