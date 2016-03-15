library(reshape2)
library(ggplot2)
source('multiplot.R')

#save(tscopy, file='tscopy.withflux.Rdata')
#tscopy.original = tscopy
tscopy = tscopy.original
#tscopy$flux.mgs[tscopy$flux.mgs > 6000000] = NA

tscopy$julian = .indexyday(tscopy)
ts.prepare.1 = tscopy[!is.na(tscopy$flux.mgs) & !is.na(tscopy$Discharge)]
window = 60
nbins = 20


kfold.10 = nrow(ts.prepare.1) - nrow(ts.prepare.1)/1.1
ts.prepare = ts.prepare.1[sort(sample(1:nrow(ts.prepare.1), 100)),]
plot(ts.prepare$flux.mgs, ylim=c(0,10000000))

normalized.julian = NULL
point.estimates = NULL
par(mfrow=c(2,2))
for( i in 1:180){
  low = i
  high = i + window
  if(high > 365){
    high = high - 365
    ts.process = ts.prepare[ts.prepare$julian >= low | ts.prepare$julian <= high]
  } else {
    ts.process = ts.prepare[ts.prepare$julian >= low & ts.prepare$julian <= high]
  }
  ts.process$bin = cut2(ts.process$Discharge, g=nbins)
  avgs = ddply(as.data.frame(ts.process), ~bin, summarise, discharge.mean=mean(Discharge), flux.mean=mean(flux.mgs), n=length(flux))
  lm.avgs = lm( flux.mean ~ poly(discharge.mean, 1), data=avgs)
  p = predict(lm.avgs)
  df.process = as.data.frame(ts.process)

  
  normalized.data = data.frame(discharge.mean = c(50000))
  normalized.julian[i] = predict(lm.avgs, newdata = normalized.data)[1]  

  julian.target = i + window/2
  if(julian.target > 365) {
    julian.target = julian.target - 365
  }
  ts.target = ts.prepare.1[ts.prepare.1$julian == julian.target]  # and actually for k fold it would be the other data, not inclusive
  df = as.data.frame(ts.target)                                 # and i guess for k fold you just skip when there is no target value
  
  plot(df.process$Discharge, df.process$flux.mgs,
              xlab='Discharge (cfs)', ylab='HPOA Flux (mg/s)', 
              xlim=c(0,100000), 
              ylim=c(0, 8000000)
       )
  points(df$Discharge, df$flux.mgs, col='blue', pch=15)
  lines(avgs$discharge.mean, p, col='red')
  
  predicted = predict(lm.avgs, newdata = data.frame(discharge.mean = df$Discharge))
  point.estimates.target = cbind(df$flux.mgs, predicted)
  point.estimates  = rbind(point.estimates, point.estimates.target)
}


point.estimates = data.frame(actual = point.estimates[,1], predicted = point.estimates[,2])
point.estimates = point.estimates[!is.na(point.estimates$actual),]
rmserror = rmse(point.estimates$actual, point.estimates$predicted)
rmserror

par(mfrow=c(1,1))
plot(normalized.julian)#, ylim=c(0,6000000))
dff = as.data.frame(ts.prepare)
points(dff$julian, dff$flux.mgs, col='coral')








# checking regressions out

window = 30
low = i
high = i + window
if(high > 365){
  high = high - 365
  ts.process = ts.prepare[ts.prepare$julian >= low | ts.prepare$julian <= high]
} else {
  ts.process = ts.prepare[ts.prepare$julian >= low & ts.prepare$julian <= high]
}
ts.process$bin = cut2(ts.process$Discharge, g=nbins)
avgs = ddply(as.data.frame(ts.process), ~bin, summarise, discharge.mean=mean(Discharge), flux.mean=mean(flux.mgs), n=length(flux))
lm.avgs = lm( flux.mean ~ poly(discharge.mean, 1), data=avgs)
df = as.data.frame(ts.process)
p = predict(lm.avgs, newdata = data.frame(discharge.mean = df$Discharge))
rmserror = rmse(ts.process$flux.mgs, p)
plot(avgs$discharge.mean, avgs$flux.mean, 
     xlim=c(0,100000), ylim=c(0,8000000),
     xlab = 'Discharge (cfs)', ylab='Flux (mg/s)',
     main=paste0('Julian Day: ', i, ' RMSE: ', as.integer(rmserror)) )
points(df$Discharge, df$flux.mgs, col='green')
lines(df$Discharge, p, col='red', lwd=2)
i = i + 20


# what about the temp classes ?
window = 30
low = i
high = i + window
if(high > 365){
  high = high - 365
  ts.process = ts.prepare[ts.prepare$julian >= low | ts.prepare$julian <= high]
} else {
  ts.process = ts.prepare[ts.prepare$julian >= low & ts.prepare$julian <= high]
}
ts.process$bin = cut2(ts.process$Discharge, g=nbins)
avgs = ddply(as.data.frame(ts.process), ~bin, summarise, discharge.mean=mean(Discharge), flux.mean=mean(flux.mgs), n=length(flux))
lm.avgs = lm( flux.mean ~ poly(discharge.mean, 1), data=avgs)
p = predict(lm.avgs)
plot(avgs$discharge.mean, avgs$flux.mean, main = i, xlim=c(0,100000), ylim=c(0,8000000))
df = as.data.frame(ts.process)
points(df$Discharge, df$flux.mgs, col='green')
lines(avgs$discharge.mean, p, col='red')
i = i + 10



# do the stuff
par(mfrow=c(2,1))
new.data = data.frame(discharge.mean = seq(0,100000,100))
noramlize.data = data.frame(discharge.mean = c(25000))

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
lms = NULL
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
  lm.avgs = lm( flux.mean ~ poly(discharge.mean, 2), data=avgs)
  lms[[i]] <- lm.avgs
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
avgs.warming = all.avgs
regression.warming = regressions

# add fall
avgs.cooling = NULL
regressions.cooling = NULL

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
  if(length(times) <= 2){
    next.t = next.t - 1
    next
  }
  class.count = rbind(class.count, length(times))
  ts.window = tscopy[times]
  
  avgs = flux.avg(ts.window, bin.number=nbins)
  lm.avgs = lm( flux.mean ~ poly(discharge.mean, 2), data=avgs)
  lms[[i]] <- lm.avgs
  
  print(summary(lm.avgs)$r.squared)
  avgs$temperature.class = i
  avgs$temperature.class.base = next.t
  
  avgs$predictions = predict(lm.avgs)
  avgs.cooling = rbind(avgs.cooling, avgs)
  
  regressions.cooling = rbind(regressions.cooling, data.frame(discharge.mean = new.data$discharge.mean, 
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
all.avgs = rbind(all.avgs, avgs.cooling)
regressions = rbind(regressions, regressions.cooling)

# TODO: Split warming and cooling into a 2 up with classes on x axis
# make sure cooling classes make sense, they seem elevated

# but we actually want to plot the REGESSION predictions

group = "temperature.class.base"
avgs.subset = avgs.warming
avgs.subset.long <- melt(avgs.subset, id=group, measure.vars = c('discharge.mean'))  # convert to long format
flux.long <- melt(avgs.subset, id=group, measure.vars = c('predictions')) 
avgs.subset.long$flux.mean = flux.long$value

p1 = ggplot(data=avgs.subset.long,
            aes_string(x=group, y="value", group=group, colour="flux.mean")
) +
  geom_line(size=6) +
  scale_colour_gradientn(colours=c('purple', 'orange'), limits=c(0,6000000)) +
  #scale_colour_gradientn(colours=c('white', 'black')) +
  xlab('Temperature Class') +
  ylab('Discharge')

avgs.subset = avgs.cooling
avgs.subset.long <- melt(avgs.subset, id=group, measure.vars = c('discharge.mean'))  # convert to long format
flux.long <- melt(avgs.subset, id=group, measure.vars = c('predictions')) 
avgs.subset.long$flux.mean = flux.long$value

p2 = ggplot(data=avgs.subset.long,
            aes_string(x=group, y="value", group=group, colour="flux.mean")
) +
  geom_line(size=6) +
  scale_x_reverse() +
  scale_colour_gradientn(colours=c('purple', 'orange'), limits=c(0,6000000)) +
  #scale_colour_gradientn(colours=c('white', 'black')) +
  xlab('Temperature Class') +
  ylab('Discharge')

multiplot(p1, p2)



# the regressions
group = "temperature.class"
regression.subset = regression.warming
avgs.subset.long <- melt(regression.subset, id=group, measure.vars = c('discharge.mean'))  # convert to long format
flux.long <- melt(regression.subset, id=group, measure.vars = c('predictions')) 
avgs.subset.long$flux.mean = flux.long$value

p1 = ggplot(data=avgs.subset.long,
            aes_string(x=group, y="value", group=group, colour="flux.mean")
) +
  geom_line(size=6) +
  scale_colour_gradientn(colours=c('purple', 'orange'), limits=c(0,10000000)) +
  #scale_colour_gradientn(colours=c('white', 'black')) +
  xlab('Temperature Class') +
  ylab('Discharge')

regression.subset = regression.cooling
avgs.subset.long <- melt(regression.subset, id=group, measure.vars = c('discharge.mean'))  # convert to long format
flux.long <- melt(regression.subset, id=group, measure.vars = c('predictions')) 
avgs.subset.long$flux.mean = flux.long$value

p2 = ggplot(data=avgs.subset.long,
            aes_string(x=group, y="value", group=group, colour="flux.mean")
) +
  geom_line(size=6) +
  scale_x_reverse() +
  scale_colour_gradientn(colours=c('purple', 'orange'), limits=c(0,10000000)) +
  #scale_colour_gradientn(colours=c('white', 'black')) +
  xlab('Temperature Class') +
  ylab('Discharge')

multiplot(p1, p2)




# do the normalized version
normalized = regressions[regressions$discharge.mean==25000,]
ggplot( data=normalized, aes(x=temperature.class, y=predictions)) +
  geom_line()


# let's look at the  regressions
par(mfrow=c(4,4))
for(i in 1:70) {
  df = all.avgs[all.avgs$temperature.class==i,]
  plot(df$discharge.mean, df$flux.mean/100000, xlim=c(0,100000), ylim=c(0,80),
       main=i)
  lines(df$discharge.mean, df$predictions/100000, typ='l', col='red')
}

par(mfrow=c(2,2))
i = 20
plot(lms[[i]]$model[2][,1][,1], lms[[i]]$residuals, ylim=c(-500000,500000))
lms[[i]]