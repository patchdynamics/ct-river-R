
par(mfrow=c(3,4))
ylimit = c(1,3)  # 3.5
xlimit = c(0,70000) # 100000
year  = 115
for(i in 0:11) {
  
  above = i - 1  # note indexing starts with 0
  below = i+2 - 1
  subsetseries = fullseries[.indexmon(fullseries) > above & .indexmon(fullseries) < below]
  subsetseries = subsetseries[.indexyear(subsetseries) == year]
  discharge = na.approx(subsetseries$Discharge, na.rm=FALSE)
  discharge = as.data.frame(discharge)
  subset = as.data.frame(subsetseries)
  head(subset)
  plot(subset$Discharge/1000, subset$HPOA.mgl, ylim=ylimit,
       col=colors(subsetseries),
       xlim=xlimit/1000)
  
}