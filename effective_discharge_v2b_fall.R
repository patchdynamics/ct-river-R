library(reshape2)
library(ggplot2)

# create nomax
mg <- aggregate(as.vector(tslval$Temperature), by=list(.indexyear(tslval)), FUN=max) 
tslval.nomax = tslval
compare = mg[.indexyear(tslval.nomax)- 110,]
tslval.nomax[tslval.nomax$Temperature >= (compare$x-1)] = NA
plot(tslval.nomax)



# do the stuff
par(mfrow=c(2,1))
new.data = data.frame(discharge.mean = seq(0,100000,100))

nbins = 20
window = 10



# just the warming phase
start = min(tshval)
stop.t = max(tshval)
plots = floor((stop.t - start) / window) + 1
plots = floor(stop.t - start)
colors = colorRampPalette(c("blue", "red"))( plots ) 

next.t = start
i = 1
all.avgs = NULL
class.count = NULL
regressions = NULL
while(next.t < stop.t) {
  #print(next.t)
  min = next.t
  max = min + window
  times = index(tshval[tshval$Temperature >= min & tshval$Temperature < max & is.na(tslval.nomax$Temperature)])
  class.count = rbind(class.count, length(times))
  ts.window = tscopy[times]
  if(length(times) == 0){
    break
  }
  
  avgs = flux.avg(ts.window, bin.number=nbins)
  lm.avgs = lm( flux.mean ~ discharge.mean, data=avgs)
  #print(summary(lm.avgs)$r.squared)
  avgs$temperature.class = i
  avgs$temperature.class.base = next.t
  avgs$predictions = predict(lm.avgs)
  all.avgs = rbind(all.avgs, avgs)
  
  regressions = rbind(regressions, data.frame(discharge.mean = new.data$discharge.mean, 
                                 predictions = predict(lm.avgs, new.data),
                                 temperature.class = i)) 
  
  if(next.t == start) {
    plot(avgs$discharge.mean, avgs$predictions, typ='l',
         xlab='Discharge (cfs)', ylab='HPOA Flux (mg/s)', 
         xlim=c(0,100000), 
         ylim=c(0, 8000000),
         col = colors[i],
         lwd = 3
    )
  } else {
    lines(avgs$discharge.mean, avgs$predictions, col = colors[i], lwd=3)
  }
  next.t = next.t + 1
  i = i + 1
}

# add fall
start = max(tslval)
stop.t = min(tslval)
plots = floor((stop.t - start) / window) + 1
plots = floor(start - stop.t)
colors = colorRampPalette(c("red", "blue"))( plots ) 

next.t = start
j = 1
while(next.t > stop.t) {
  max = next.t
  min = max - window
  
  # this next command is causing some issues
  # with the call to flux.avg below
  times = index(tshval[!is.na(tslval.nomax$Temperature) & tslval$Temperature <= max & tslval$Temperature > min ])
  if(length(times) == 0){
    next.t = next.t - 1
    next
  }
  class.count = rbind(class.count, length(times))
  ts.window = tscopy[times]
  
  avgs = flux.avg(ts.window, bin.number=nbins)
  lm.avgs = lm( flux.mean ~ discharge.mean, data=avgs)
  print(summary(lm.avgs)$r.squared)
  avgs$temperature.class = i
  avgs$temperature.class.base = next.t
  
  avgs$predictions = predict(lm.avgs)
  all.avgs = rbind(all.avgs, avgs)
  
  regressions = rbind(regressions, data.frame(discharge.mean = new.data$discharge.mean, 
                                              predictions = predict(lm.avgs, new.data),
                                              temperature.class = i)) 
  
  
  if(j == 1) {
    plot(avgs$discharge.mean, avgs$predictions, typ='l',
         xlab='Discharge (cfs)', ylab='HPOA Flux (mg/s)', 
         xlim=c(0,100000), 
         ylim=c(0, 8000000),
         col = colors[j],
         lwd = 3
    )
  } else {
    lines(avgs$discharge.mean, avgs$predictions, col = colors[j], lwd=3)
  }
  next.t = next.t - 1
  i = i + 1
  j = j + 1
}

# but we actually want to plot the REGESSION predictions
group = "temperature.class"
all.avgs.long <- melt(all.avgs, id=group, measure.vars = c('discharge.mean'))  # convert to long format
flux.long <- melt(all.avgs, id=group, measure.vars = c('predictions')) 
all.avgs.long$flux.mean = flux.long$value

ggplot(data=all.avgs.long,
       aes_string(x=group, y="value", group=group, colour="flux.mean")
  ) +
  geom_line(size=6) +
  scale_colour_gradientn(colours=c('purple', 'orange')) +
  #scale_colour_gradientn(colours=c('white', 'black')) +
  xlab('Temperature Class') +
  ylab('Discharge')

regressions.long = melt(regressions, id="temperature.class", measure.vars = c('discharge.mean'))  
predicted.flux.long <- melt(regressions, id="temperature.class", measure.vars = c('predictions')) 
regressions.long$flux.mean = predicted.flux.long$value
ggplot(data=regressions.long,
       aes(x=temperature.class, y=value, group=temperature.class, colour=flux.mean)) +
  geom_line(size=6) +
  scale_colour_gradientn(colours=c('black', 'orange')) +
  #scale_colour_gradientn(colours=c('white', 'black')) +
  #scale_colour_gradientn(colours=c('blue', 'red')) +
  xlab('Temperature Class') +
  ylab('Discharge (cfs)')

# let's do this as a surface, using color.
avgs$temperature.class = 1
ggplot(avgs, aes(y=temperature.class, x=discharge.mean)) + geom_line(aes(group = 1, colour=flux.mean))


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
