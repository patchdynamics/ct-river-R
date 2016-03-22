
threshold = 15000
ts.low.flow = tscopy$Discharge
ts.low.flow[ts.low.flow$Discharge <= threshold] = 1
ts.low.flow[ts.low.flow$Discharge > threshold] = 0
ts.low.flow[.indexmon(ts.low.flow) <= 4] = 0
ts.low.flow[.indexmon(ts.low.flow) >= 10] = 0
par(mfrow=c(2,1))
plot(tscopy$Discharge)
plot(ts.low.flow)

sum = 0
ts.low.flow$cumulative = 0
for(i in 1:nrow(ts.low.flow)){
  if(is.na(ts.low.flow$Discharge[i]) | is.nan(ts.low.flow$Discharge[i])){
    ts.low.flow$cumulative[i] = sum
    next
  }
  if(ts.low.flow$Discharge[i] == 1){
    sum = sum + 1
    ts.low.flow$cumulative[i] = sum
  } else {
    sum = 0
  }  
}
plot(tscopy$Discharge)
plot(ts.low.flow$cumulative)

ts.low.flow$max = ts.low.flow$cumulative
for(i in (nrow(ts.low.flow)-1):1){
  if(ts.low.flow[i]$cumulative > 0 && ts.low.flow$max[i+1] > 0){
    ts.low.flow$max[i] = ts.low.flow$max[i+1]
  }
}
plot(tscopy$Discharge)
plot(ts.low.flow$max)



ts.low.flow$low.flow = ts.low.flow$max > 30
par(mfrow=c(2,1))
plot(tscopy$Discharge)
plot(ts.low.flow$low.flow)

tscopy = merge(tscopy, fullseries$Limb)
ts.low.flow.values = tscopy[ts.low.flow$low.flow == 1]
#plot(ts.low.flow.values$Discharge)
#plot(ts.low.flow.values$HPOA.mgl)

df = as.data.frame(ts.low.flow.values)
df$year = .indexyear(ts.low.flow.values)
df = df[df$Limb==0,]
par(mfrow=c(1,1))
plot(df$Discharge, df$HPOA.mgl,
     col=rainbow(6)[df$year-110],
     pch=c(25,1,24)[df$Limb + 2],
     ylim=c(0,3),
     xlim=c(0,100000)
     )






