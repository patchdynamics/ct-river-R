window = 3
start = min(tshval)
stop.t = max(tshval)

plots = floor((stop.t - start) / window) + 1
plots = floor(stop.t - start)
colors = colorRampPalette(c("blue", "red"))( plots ) 


# just the warming phase
next.t = start
i = 1
all.avgs = NULL
class.count = NULL
while(next.t < stop.t) {
  print(next.t)
  min = next.t
  max = min + window
  times = index(tshval[tshval$Temperature >= min & tshval$Temperature < max & tslval$Temperature > 290])
  class.count = rbind(class.count, length(times))
  ts.window = tscopy[times]
  
  avgs = flux.avg(ts.window, bin.number=nbins)
  lm.avgs = lm( flux.mean ~ discharge.mean, data=avgs)
  print(summary(lm.avgs)$r.squared)
  
  if(next.t == start) {
    plot(avgs$discharge.mean, predict(lm.avgs), typ='l',
       xlab='Discharge (cfs)', ylab='HPOA Flux (mg/s)', 
       xlim=c(0,100000), 
       ylim=c(0, 8000000),
       col = colors[i],
       lwd = 3
       )
  } else {
    lines(avgs$discharge.mean, predict(lm.avgs), col = colors[i], lwd=3)
  }
  next.t = next.t + 1
  i = i + 1
}

# hows it look if we try a nonlinear model
ts.copy2 = tscopy
ts.copy2 = merge(ts.copy2, tshval$Temperature)
df.copy2 = as.data.frame(ts.copy2)
df.copy2 = df.copy2[!is.na(df.copy2$Temperature),]
df.lm = data.frame(flux.mgs = df.copy2$flux.mgs, Discharge = df.copy2$Discharge/10000, Temperature = (df.copy2$Temperature-min(df.copy2$Temperature)) + 1)

# df.lm doesn't actually need the preprocessing above
df.lm = data.frame(flux.mgs = df.copy2$flux.mgs, Discharge = df.copy2$Discharge, Temperature = df.copy2$Temperature )

df.lm = df.lm[!is.na(df.lm$Discharge) & !is.na(df.lm$Temperature) & df.lm$Discharge > 0 & !is.na(df.lm$flux.mgs),]
lm.nonlinear = lm(flux.mgs ~ Discharge * Temperature, df.lm)  
summary(lm.nonlinear)
plot(lm.nonlinear$residuals)  # pretty autocorrellated, R^2 not reliable
plot((df.copy2$Discharge/10000) * (df.copy2$Temperature-min(df.copy2$Temperature)+10), df.copy2$flux.mgs)
plot((df.copy2$Discharge/10000) * (df.copy2$Temperature), df.copy2$flux.mgs)

plot((df.copy2$Discharge/10000), df.copy2$flux.mgs)

predicted = predict(lm.nonlinear)
rmse = rmse(df.lm$flux.mgs, predicted)
print(rmse)
print(mean(df.lm$flux.mgs))

# try with training set
lm.nonlinear = lm(flux.mgs ~ Discharge * Temperature, df.lm[sort(sample(nrow(df.lm),100)),])  
summary(lm.nonlinear)
plot(lm.nonlinear$residuals, typ='l')

# sure you get a high R^2 with this, but the coefficients are probably not accurately estimated.
# yeah they are all over the place...
