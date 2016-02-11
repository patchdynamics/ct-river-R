par(mfrow=c(1,1))

thresh = 285
timesbelow = index(stemp.hval.level2[stemp.hval.level1$Temperature < thresh  | stemp.lval.level1$Temperature < thresh  ])

tsfreezing = ts[timesbelow]
#tsfreezing = tsfreezing[!(.indexmon(tsfreezing) %in% (c(9,10,11,12)-1)) ]
#tsfreezing = tsfreezing[.indexmon(tsfreezing) %in% (c(12) - 1) ]
#tsfreezing = ts
tsfreezing = tsfreezing[.indexyday(tsfreezing) > 350 ]


ylimit = c(1,3.5)  # 3.5
xlimit = c(0,100000) # 100000
discharge = na.approx(tsfreezing$Discharge, na.rm=FALSE)
discharge = as.data.frame(discharge)$Discharge

plot(discharge/1000, as.data.frame(tsfreezing)$HPOA.mgl, ylim=ylimit,
     col=rainbow(12)[.indexmon(tsfreezing) + 1],
     xlim=xlimit/1000,
     xlab = 'Discharge / 1000 cfs',
     ylab = '[HPOA] mg/L',
     main = paste0('Julian Day > 350, Lowest Surface Avg Soil Temp < ',thresh,'K')
)
#legend( x="topright", 
#        legend=c('Jan', 'Feb', 'Mar', 'Apr', 'May'),
#        col=rainbow(12), pch=1
#)

library(plotly)
library(rglwidget)
df = data.frame(Discharge = discharge / 1000, HPOA.mgl = as.data.frame(tsfreezing)$HPOA.mgl,
                pf = as.data.frame(tsfreezing)$ts.pf)
plot_ly(df, y = HPOA.mgl, x = Discharge, z = pf,  type = "scatter3d")