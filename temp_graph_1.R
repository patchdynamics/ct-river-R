par(mfrow=c(3,1))

thresh = 285
timesbelow = index(stemp.hval.level2[stemp.hval.level1$Temperature < thresh  | stemp.lval.level1$Temperature < thresh  ])

tsfreezing = ts[timesbelow]
tsfreezing = tsfreezing[!(.indexmon(tsfreezing) %in% (c(9,10,11,12)-1)) ]


ylimit = c(1,3.5)  # 3.5
xlimit = c(0,100000) # 100000
discharge = na.approx(tsfreezing$Discharge, na.rm=FALSE)
discharge = as.data.frame(discharge)$Discharge

plot(discharge/1000, as.data.frame(tsfreezing)$HPOA.mgl, ylim=ylimit,
     col=rainbow(12)[.indexmon(tsfreezing) + 1],
     xlim=xlimit/1000,
     xlab = 'Discharge / 1000 cfs',
     ylab = '[HPOA] mg/L',
     main = paste0('Spring, Surface Avg Soil Temp < ',thresh,'K')
)
legend( x="topright", 
        legend=c('Jan', 'Feb', 'Mar', 'Apr', 'May'),
        col=rainbow(12), pch=1
)


thresh = 278
timesbelow = index(stemp.hval.level2[stemp.hval.level2$Temperature < thresh  | stemp.lval.level2$Temperature < thresh  ])

tsfreezing = ts[timesbelow]
tsfreezing = tsfreezing[!(.indexmon(tsfreezing) %in% (c(9,10,11,12)-1)) ]


ylimit = c(1,3.5)  # 3.5
xlimit = c(0,100000) # 100000
discharge = na.approx(tsfreezing$Discharge, na.rm=FALSE)
discharge = as.data.frame(discharge)$Discharge

plot(discharge/1000, as.data.frame(tsfreezing)$HPOA.mgl, ylim=ylimit,
     col=rainbow(12)[.indexmon(tsfreezing) + 1],
     xlim=xlimit/1000,
     xlab = 'Discharge / 1000 cfs',
     ylab = '[HPOA] mg/L',
     main = paste0('Spring, 10cm Avg Soil Temp < ',thresh,'K')
)
legend( x="topright", 
        legend=c('Jan', 'Feb', 'Mar', 'Apr', 'May'),
        col=rainbow(12), pch=1
)


thresh = 282
timesbelow = index(stemp.hval.level2[stemp.hval.level2$Temperature < thresh  | stemp.lval.level2$Temperature < thresh  ])

tsfreezing = ts[timesbelow]
tsfreezing = tsfreezing[!(.indexmon(tsfreezing) %in% (c(9,10,11,12)-1)) ]


ylimit = c(1,3.5)  # 3.5
xlimit = c(0,100000) # 100000
discharge = na.approx(tsfreezing$Discharge, na.rm=FALSE)
discharge = as.data.frame(discharge)$Discharge

plot(discharge/1000, as.data.frame(tsfreezing)$HPOA.mgl, ylim=ylimit,
     col=rainbow(12)[.indexmon(tsfreezing) + 1],
     xlim=xlimit/1000,
     xlab = 'Discharge / 1000 cfs',
     ylab = '[HPOA] mg/L',
     main = paste0('Spring, 10cm Avg Soil Temp < ',thresh,'K')
)
legend( x="topright", 
        legend=c('Jan', 'Feb', 'Mar', 'Apr', 'May'),
        col=rainbow(12), pch=1
)




