
hpi.subset = ts.hpi[.indexmon(ts.hpi) %in% (c(1,2,3,4)-1)]
df = as.data.frame(hpi.subset)
plot(df$Discharge/1000, df$HPI.Percent,ylim=c(0,.3), xlim=c(0,100))


hpi.subset = ts.hpi[.indexmon(ts.hpi) %in% (c(5,6,7)-1)]
df = as.data.frame(hpi.subset)
plot(df$Discharge/1000, df$HPI.Percent)


hpi.subset = ts.hpi[.indexmon(ts.hpi) %in% (c(4)-1)]
df = as.data.frame(hpi.subset)
plot(df$Discharge/1000, df$HPI.Percent)

hpi.subset = ts.hpi[.indexmon(ts.hpi) %in% (c(5,6,7,8,9,10,11,12)-1)]
df = as.data.frame(hpi.subset)
plot(df$Discharge/1000, df$HPI.Percent, ylim=c(0,.3), xlim=c(0,100))

hpi.subset = ts.hpi[.indexmon(ts.hpi) %in% (c(12)-1)]
df = as.data.frame(hpi.subset)
plot(df$Discharge/1000, df$HPI.Percent, ylim=c(0,.3), xlim=c(0,100))

df = as.data.frame(ts.hpi)
plot(df$Discharge/1000, df$HPI.Percent, ylim=c(0,.3), xlim=c(0,100))


# HPOA
# maybe something different going on in fall, but really hard to tell
# because we don't have high discharge values

hpoa.subset = ts.hpoa[.indexmon(ts.hpoa) %in% (c(1,2,3,4,5,8,7,8,9,10)-1)]
df = as.data.frame(hpoa.subset)
plot(df$Discharge/1000, df$HPOA.Percent, ylim=c(.4,.6), xlim=c(0,120))

hpoa.subset = ts.hpoa[.indexmon(ts.hpoa) %in% c(10,11,12)]
df = as.data.frame(hpoa.subset)
points(df$Discharge/1000, df$HPOA.Percent, ylim=c(.4,.6), xlim=c(0,120),col='red')



split = 11
hpoa.subset = ts.hpoa[.indexmon(ts.hpoa) < split]
df = as.data.frame(hpoa.subset)
plot(df$Discharge/1000, df$HPOA.Percent, ylim=c(.4,.6), xlim=c(0,120))

hpoa.subset = ts.hpoa[.indexmon(ts.hpoa) >= split]
df = as.data.frame(hpoa.subset)
points(df$Discharge/1000, df$HPOA.Percent, ylim=c(.4,.6), xlim=c(0,120),col='red')

