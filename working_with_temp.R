library(xts)

data = read.csv("temperature_2011_2015.csv")
data = data[!is.na(data[,2]),]

times = strptime(paste0(data[,2],data[,3]), '%Y%j')
tstemp = xts(data[,4], order.by=times)
names(tstemp) = c('Temperature')

processed = tstemp
minimum = min(tstemp$Temperature)
highest = minimum
for(i in 1:nrow(tstemp)){
  if(.indexyday(tstemp[i]) == 0) {
    highest = minimum
  }
  if(as.numeric(tstemp[i]$Temperature) > as.numeric(highest)){
    highest = tstemp[i]
  }
  processed[i] = highest
}
tshval = processed

processed = tstemp
minimum = min(tstemp$Temperature)
highest = minimum
for(i in nrow(tstemp):1){
  if(.indexyday(tstemp[i]) == 0) {
    highest = minimum
  }
  if(as.numeric(tstemp[i]$Temperature) > as.numeric(highest)){
    highest = tstemp[i]
  }
  processed[i] = highest
}
tslval = processed

par(mfrow=c(2,1))
plot(tshval, main='Max Avg Rising Surface Temp', ylab='Temperature (K)')
plot(tslval, main='Min Avg Falling Surface Temp', ylab='Temperature (K)')


tsrollingmean = rollmean(tstemp, 5)
plot(tsrollingmean$Temperature)

# want to get dates 'before' thaw
ts1 = tshval
ts1[ts1$Temperature > 273.15] = 273.15
plot(ts1)

ts = cbind(tscombined$Discharge, tscombined$HPOA.mgl)
ts = ts['2010-01-01/2015-12-31']
head(ts)

# spring by temp
timesbelow = index(tshval[tshval$Temperature < 280.15])
tsfreezing = ts[timesbelow]

ylimit = c(1,3.5)  # 3.5
xlimit = c(0,100000) # 100000
discharge = na.approx(tsfreezing$Discharge, na.rm=FALSE)
discharge = as.data.frame(discharge)$Discharge

plot(discharge/1000, as.data.frame(tsfreezing)$HPOA.mgl, ylim=ylimit,
     col=rainbow(6)[.indexmon(tsfreezing)],
     xlim=xlimit/1000)
legend( x="topleft", 
        legend=c('Jan', 'Fed', 'Mar', 'Apr', 'May'),
        col=rainbow(6), lwd=1, lty=c(1,2), 
        pch=c(15,17) )




# find the freshet
# looks like it pretty much happens in this range.
times2 = index(tshval[tshval$Temperature >= 280.15 & tshval$Temperature < 290.15])
ts2 = ts[times2]
#ts2 = ts2['2011-01-01/2011-12-31']

ylimit = c(1,3.5)  # 3.5
xlimit = c(0,100000) # 100000
discharge = na.approx(ts2, na.rm=FALSE)
discharge = as.data.frame(discharge)$Discharge
plot(discharge/1000, as.data.frame(ts2)$HPOA.mgl, ylim=ylimit,
     #col=rainbow(length(unique(.indexmon(ts2)))),
     col=rainbow(length(unique(.indexyear(ts2)))), 
     xlim=xlimit/1000,
     xlab='Discharge / 1000  cfs',
     ylab='[HPOA] ppm',
     main='Highest Avg Surface Temp   280K < T < 290K',
     #sub='Freshets Seem to Occur Exclusively in this Range'
     )
legend( x="topright", 
        #legend=month.abb[min(.indexmon(ts2)):max(.indexmon(ts2))],
        #col=rainbow(length(unique(.indexmon(ts2)))), 
        legend = as.character(unique(.indexyear(ts2)) + 1900),
        col=rainbow(length(unique(.indexyear(ts2)))), 
        pch=1 )

# after freshet by temp
times2 = index(tshval[tshval$Temperature >= 290.15 & tshval$Temperature < 295.15])
ts2 = ts[times2]

ylimit = c(1,3.5)  # 3.5
xlimit = c(0,100000) # 100000
discharge = na.approx(ts2, na.rm=FALSE)
discharge = as.data.frame(discharge)$Discharge
plot(discharge/1000, as.data.frame(ts2)$HPOA.mgl, ylim=ylimit,
     #col=rainbow(length(unique(.indexmon(ts2)))),
     col=rainbow(length(unique(.indexyear(ts2))))[.indexyear(ts2) - 110], 
     
     xlim=xlimit/1000,
     xlab='Discharge / 1000  cfs',
     ylab='[HPOA] ppm',
     main='290K < Max Avg Rising Surface Temperature < 295K'
)
legend( x="topright", 
        #legend=month.abb[min(.indexmon(ts2)):max(.indexmon(ts2))],
        #col=rainbow(length(unique(.indexmon(ts2)))), 
        legend=unique(.indexyear(ts2)) + 2000,
        col=rainbow(length(unique(.indexyear(ts2)))),
        pch=1)


#summer
times2 = index(tshval[tshval$Temperature >= 295.15 & tslval$Temperature >= 295.15])
ts2 = ts[times2]
#ts2 = ts[.indexmon(ts) >= 7 & .indexmon(ts) <= 9]
ts2 = ts2[.indexyear(ts2) != 113]

ylimit = c(1,3.5)  # 3.5
discharge = na.approx(ts2$Discharge, na.rm=FALSE)
discharge = as.data.frame(discharge)$Discharge
xlimit = c(min(discharge[!is.na(discharge)]),100000) # 100000

#discharge[discharge < 0] = 1

par(mfrow=c(1,1))
plot(discharge/1000, as.data.frame(ts2)$HPOA.mgl, ylim=ylimit,
     #col=rainbow(length(unique(.indexmon(ts2)))),
     col=rainbow(5)[.indexyear(ts2)-110],
     xlim=xlimit/1000, #log="x",
     pch=.indexyear(ts2)-110
     )
legend( x="bottomright", 
        #legend=month.abb[min(.indexmon(ts2)):max(.indexmon(ts2))-1],
        #col=rainbow(length(unique(.indexmon(ts2)))), lwd=1, lty=c(1,2), 
        legend=2011:2015,
        col=rainbow(5),
        pch=1:5)

plot(discharge/1000, as.data.frame(ts2)$HPOA.mgl, ylim=ylimit,
     col=colors(ts2),
     xlim=xlimit/1000)
hpoa = as.data.frame(ts2)$HPOA.mgl

formula = hpoa ~ log(discharge)
lm.hpoa = lm(formula, data = data.frame(hpoa = hpoa, discharge = discharge))
lines(discharge[!is.na(hpoa)]/1000, predict(lm.hpoa, data=data.frame(discharge=discharge[!is.na(hpoa)])) )
legend("topleft", bty="n", 
       legend=paste("R2 =",format(summary(lm.hpoa)$adj.r.squared, digits=4)))


#fall 1 - no good
times2 = index(tshval[tslval$Temperature >= 290.15 & tslval$Temperature <= 295.15])
ts2 = ts[times2]
#ts2 = ts2['2015-01-01/2015-12-31']

ylimit = c(1,3.5)  # 3.5
xlimit = c(0,100000) # 100000
discharge = na.approx(ts2, na.rm=FALSE)
discharge = as.data.frame(discharge)$Discharge
plot(discharge/1000, as.data.frame(ts2)$HPOA.mgl, ylim=ylimit,
     col=rainbow(5)[.indexyear(ts2)-110],
     xlim=xlimit/1000,
     pch=.indexyear(ts2)-110)
legend( x="topright", 
        #legend=month.abb[min(.indexmon(ts2)):max(.indexmon(ts2)) + 1],
        legend = 2011:2015,
        col=rainbow(5),
        lwd=1, lty=c(1,2), 
        pch=1:5 )

plot(discharge/1000, as.data.frame(ts2)$HPOA.mgl, ylim=ylimit,
     col=year_colors(ts2),
     xlim=xlimit/1000)
hpoa = as.data.frame(ts2)$HPOA.mgl
lm.hpoa = lm(hpoa ~ discharge, data = data.frame(hpoa = hpoa, discharge = discharge))
lines(discharge/1000, lm.hpoa$coefficients[1] + lm.hpoa$coefficients[2]*discharge )
legend("topleft", bty="n", 
       legend=paste("R2 =",format(summary(lm.hpoa)$adj.r.squared, digits=4)))

#fall 2

times2 = index(tshval[tslval$Temperature >= 285.15 & tslval$Temperature <= 295.15])

#times2 = index(tshval[tshval$Temperature >= 295.15 & tslval$Temperature <= 295.15])

ts2 = ts[times2]
#ts2 = ts2['2015-01-01/2015-12-31']

ylimit = c(1,3.5)  # 3.5
xlimit = c(0,100000) # 100000
discharge = na.approx(ts2$Discharge, na.rm=FALSE)
discharge = as.data.frame(discharge)$Discharge
plot(discharge/1000, as.data.frame(ts2)$HPOA.mgl, ylim=ylimit,
     col=rainbow(5)[.indexyear(ts2)-110],
     xlim=xlimit/1000,
     main='lval > 285, lval < 295',
     pch=.indexyear(ts2)-110)
legend( x="topright", 
        legend = 2011:2015,
        col=rainbow(5),
        lwd=1, lty=c(1,2), 
        pch=1:5  )

df = as.data.frame(ts2[.indexyear(ts2)==111 & .indexmon(ts2) < 9])
colors = colorRampPalette(c("blue", "red"))( length(as.vector(df$HPOA.mgl) ))
#colors=rainbow(length(as.vector(df$HPOA.mgl) ))
plot(df$Discharge/1000, df$HPOA.mgl, ylim=ylimit,
     col=colors,
     xlim=xlimit/1000,
     main='lval > 285, lval < 295',
     #typ='l',
     pch=1
     )


plot(discharge/1000, as.data.frame(ts2)$HPOA.mgl, ylim=ylimit,
     col=colors(ts2),
     xlim=xlimit/1000)
hpoa = as.data.frame(ts2)$HPOA.mgl

formula = hpoa ~ log(discharge)
lm.hpoa = lm(formula, data = data.frame(hpoa = hpoa, discharge = discharge))
#lines(sort(discharge[!is.na(hpoa)])/1000, predict(lm.hpoa, data=data.frame(discharge=sort(discharge[!is.na(hpoa)]))) )
lines(discharge[!is.na(hpoa)]/1000, predict(lm.hpoa, data=data.frame(discharge=discharge[!is.na(hpoa)])) )
legend("topleft", bty="n", 
       legend=paste("R2 =",format(summary(lm.hpoa)$adj.r.squared, digits=4)))


#
# Fall 3
#
times2 = index(tshval[tslval$Temperature <= 285.15])
ts2 = ts[times2]
#ts2 = ts2['2015-01-01/2015-12-31']

ylimit = c(1,3.5)  # 3.5
xlimit = c(0,100000) # 100000
discharge = na.approx(ts2$Discharge, na.rm=FALSE)
discharge = as.data.frame(discharge)$Discharge
plot(discharge/1000, as.data.frame(ts2)$HPOA.mgl, ylim=ylimit,
     col=rainbow(length(unique(.indexmon(ts2)))),
     xlim=xlimit/1000
     )
legend( x="topright", 
        legend=month.abb[min(.indexmon(ts2)):max(.indexmon(ts2)) + 1],
        col=rainbow(length(unique(.indexmon(ts2)))), lwd=1, lty=c(1,2), 
        pch=c(15,17) )

plot(discharge/1000, as.data.frame(ts2)$HPOA.mgl, ylim=ylimit,
     col=colors(ts2),
     xlim=xlimit/1000)
hpoa = as.data.frame(ts2)$HPOA.mgl

formula = hpoa ~ discharge
lm.hpoa = lm(formula, data = data.frame(hpoa = hpoa, discharge = discharge))
#lines(sort(discharge[!is.na(hpoa)])/1000, predict(lm.hpoa, data=data.frame(discharge=sort(discharge[!is.na(hpoa)]))) )
lines(discharge[!is.na(hpoa)]/1000, predict(lm.hpoa, data=data.frame(discharge=discharge[!is.na(hpoa)])) )
legend("topleft", bty="n", 
       legend=paste("R2 =",format(summary(lm.hpoa)$adj.r.squared, digits=4)))


#
# Early Winter
#
times2 = index(tshval[tslval$Temperature <= 280])
ts2 = ts[times2]
#ts2 = ts2['2015-01-01/2015-12-31']

ylimit = c(1,3.5)  # 3.5
xlimit = c(0,100000) # 100000
discharge = na.approx(ts2$Discharge, na.rm=FALSE)
discharge = as.data.frame(discharge)$Discharge
plot(discharge/1000, as.data.frame(ts2)$HPOA.mgl, ylim=ylimit,
     col=rainbow(length(unique(.indexmon(ts2)))),
     xlim=xlimit/1000
)
legend( x="topright", 
        legend=month.abb[min(.indexmon(ts2)):max(.indexmon(ts2)) + 1],
        col=rainbow(length(unique(.indexmon(ts2)))), lwd=1, lty=c(1,2), 
        pch=c(15,17) )

plot(discharge/1000, as.data.frame(ts2)$HPOA.mgl, ylim=ylimit,
     col=colors(ts2),
     xlim=xlimit/1000)
hpoa = as.data.frame(ts2)$HPOA.mgl

formula = hpoa ~ discharge
lm.hpoa = lm(formula, data = data.frame(hpoa = hpoa, discharge = discharge))
#lines(sort(discharge[!is.na(hpoa)])/1000, predict(lm.hpoa, data=data.frame(discharge=sort(discharge[!is.na(hpoa)]))) )
lines(discharge[!is.na(hpoa)]/1000, predict(lm.hpoa, data=data.frame(discharge=discharge[!is.na(hpoa)])) )
legend("topleft", bty="n", 
       legend=paste("R2 =",format(summary(lm.hpoa)$adj.r.squared, digits=4)))



par(mfrow=c(1,1))
plot(tshval)
lines(tslval)


