library(xts)
load('ts_fdom_discharge.Rdata')
ts = tscombined['2011-01-01/2015-12-31']

temp = read.csv("temperature_2011_2015.csv")
temp = temp[!is.na(temp[,2]),]
times = strptime(paste0(temp[,2],temp[,3]), '%Y%j')
tstemp = xts(temp[,4], order.by=times)
names(tstemp) = c('Temperature')

stemp = read.csv("soil_temp_avg.csv")
stemp = stemp[!is.na(stemp[,2]),]
times = strptime(paste0(stemp[,2],sprintf("%.2d",stemp[,3]),sprintf("%.2d",stemp[,4])), '%Y%m%d')
ts.stemp = xts(stemp[,5:9], order.by=times)
names(ts.stemp) = c('SoilTemperature1', 'SoilTemperature2', 
                    'SoilTemperature3', 'SoilTemperature4', 
                    'SoilTemperature5')
plot(ts.stemp$SoilTemperature1)


pf = read.csv("soil_temp_percent_frozen.csv")
pf = pf[!is.na(pf[,5]),]
times = strptime(paste0(pf[,2],sprintf("%.2d",pf[,3]),sprintf("%.2d",pf[,4])), '%Y%m%d')
ts.pf = xts(pf[,5], order.by=times)





# find the warming / cooling profiles
source('xts.processing.R')
stemp.hval.level1 = yearly.hval(ts.stemp, 1)
names(stemp.hval.level1) = 'Temperature'
stemp.lval.level1 = yearly.lval(ts.stemp, 1)
names(stemp.lval.level1) = 'Temperature'
stemp.hval.level2 = yearly.hval(ts.stemp, 2)
names(stemp.hval.level2) = 'Temperature'
stemp.lval.level2 = yearly.lval(ts.stemp, 2)
names(stemp.lval.level2) = 'Temperature'
stemp.hval.level3 = yearly.hval(ts.stemp, 3)  # level 3 never freezes
names(stemp.hval.level3) = 'Temperature'
stemp.lval.level3 = yearly.lval(ts.stemp, 3)
names(stemp.lval.level3) = 'Temperature'


timesbelow = index(stemp.hval.level1[stemp.hval.level2$Temperature < 273.15])

timesbelow = index(stemp.hval.level1[stemp.hval.level2$Temperature > 273.15
                                     & stemp.hval.level2$Temperature < 278.15])


par(mfrow=c(2,1))

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
     main = 'Spring, Level 1 Soil Temp < 285K'
     )
legend( x="topright", 
        legend=c('Jan', 'Feb', 'Mar', 'Apr', 'May'),
        col=rainbow(12), pch=1
         )


thresh = 280
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
     main = paste0('Spring, Level 2 Soil Temp < ',thresh,'K')
)
legend( x="topright", 
        legend=c('Jan', 'Feb', 'Mar', 'Apr', 'May'),
        col=rainbow(12), pch=1
)



