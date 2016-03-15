library(reshape2)
library(ggplot2)
source('multiplot.R')
source('tools.R')

#save(tscopy, file='tscopy.withflux.Rdata')
#tscopy.original = tscopy
tscopy = tscopy.original
#tscopy$flux.mgs[tscopy$flux.mgs > 6000000] = NA

ts.temperature = ts.stemp$SoilTemperature2
rising.temp = rising.step(ts.temperature)
falling.temp = falling.step(ts.temperature)
names(rising.temp) = c("Temperature")
names(falling.temp) = c("Temperature")

tshval = rising.temp
tslval = falling.temp


# create nomax
mg <- aggregate(as.vector(tslval$Temperature), by=list(.indexyear(tslval)), FUN=max) 
tslval.nomax = tslval
compare = mg[.indexyear(tslval.nomax)- 110,]
tslval.nomax[tslval.nomax$Temperature >= (compare$x-1)] = NA
plot(tslval.nomax)



# do the stuff
par(mfrow=c(2,1))
new.data = data.frame(discharge.mean = seq(0,100000,100))
noramlize.data = data.frame(discharge.mean = c(25000))

nbins = 20
window = 4
step = 1


# just the warming phase
start = min(tshval)
stop.t = max(tshval)
plots = floor((stop.t - start) / window) + 1
plots = floor(stop.t - start)
colors = colorRampPalette(c("blue", "red"))( plots ) 

variable = 'HPOA.mgl'
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
  if(length(times) <= 2){
    next.t = next.t + step
    i = i + 1
    next
  }
  
  avgs = timeseries.bin.and.average(ts.window, variable, bin.number=nbins)
  lm.avgs = lm( var.mean ~ poly(discharge.mean, 1), data=avgs)
  lms[[i]] <- lm.avgs
  #print(summary(lm.avgs)$r.squared)
  avgs$temperature.class = i
  avgs$temperature.class.base = next.t
  predictions = predict(lm.avgs, new.data)
  all.avgs = rbind(all.avgs, avgs)
  
  regressions = rbind(regressions, data.frame(discharge.mean = new.data$discharge.mean, 
                                              predictions = predict(lm.avgs, new.data),
                                              temperature.class = i)) 
  
  df = as.data.frame(ts.window)
  plot(df$Discharge, df[,variable],  xlab='Discharge (cfs)', ylab=variable, 
       xlim=c(0,100000), 
       ylim=c(0, 3),
       col = 'coral2',
       lwd = 3,
       main = paste0('T: ', as.integer(next.t), 'K'))
  lines(new.data$discharge.mean, predictions, col = colors[i], lwd=3)
  
  next.t = next.t + step
  i = i + 1
}
avgs.warming = all.avgs
regression.warming = regressions
warming.class.max = max(regression.warming$temperature.class)

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
  
  avgs = timeseries.bin.and.average(ts.window, variable, bin.number=nbins)
  lm.avgs = lm( var.mean ~ poly(discharge.mean, 1), data=avgs)
  #lm.avgs = nls( var.mean ~ Vm * discharge.mean/(K+discharge.mean), data = avgs)
  lms[[i]] <- lm.avgs
  
  print(summary(lm.avgs)$r.squared)
  avgs$temperature.class = i
  avgs$temperature.class.base = next.t
  
  #avgs$predictions = predict(lm.avgs)
  avgs.cooling = rbind(avgs.cooling, avgs)
  predictions = predict(lm.avgs, new.data)
  
  regressions.cooling = rbind(regressions.cooling, data.frame(discharge.mean = new.data$discharge.mean, 
                                                              predictions = predict(lm.avgs, new.data),
                                                              temperature.class = i)) 
  
  if(1) {
    df = as.data.frame(ts.window)
    plot(df$Discharge, df[,variable],  xlab='Discharge (cfs)', ylab=variable, 
         xlim=c(0,100000), 
         ylim=c(0, 4),
         col = 'coral2',
         lwd = 3,
         main = paste0('T: ', as.integer(next.t), 'K'))
    points(avgs$discharge.mean, avgs$var.mean, lwd = 4, col='purple')
    lines(new.data$discharge.mean, predictions, col='blue', lwd=3)
  } else {
  if(j == 1) {
    plot(avgs$discharge.mean, avgs$predictions, typ='l',
         xlab='Discharge (cfs)', ylab='HPOA Flux (mg/s)', 
         xlim=c(0,100000), 
         ylim=c(0, 4),
         col = colors[j],
         lwd = 3
    )
  } else {
    lines(avgs$discharge.mean, avgs$predictions, col = colors[j], lwd=3)
  }
  }
  next.t = next.t - step
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


# do it the other way


group = "temperature.class.base"
avgs.subset = avgs.warming
avgs.subset.long <- melt(avgs.subset, id=group, measure.vars = c('discharge.mean'))  # convert to long format
flux.long <- melt(avgs.subset, id=group, measure.vars = c('predictions')) 
avgs.subset.long$flux.mean = flux.long$value

p1 = ggplot(data=avgs.subset.long,
            aes_string(x=group, y="flux.mean", group=group, colour="value")
) +
  geom_line(size=6) +
  scale_y_continuous(limits = c(0, 8000000)) +
  scale_colour_gradientn(colours=c('purple', 'orange'), limits=c(0,80000), name='Discharge') +
  #scale_colour_gradientn(colours=c('white', 'black')) +
  xlab('Temperature Class') +
  ylab('Flux')

avgs.subset = avgs.cooling
avgs.subset.long <- melt(avgs.subset, id=group, measure.vars = c('discharge.mean'))  # convert to long format
flux.long <- melt(avgs.subset, id=group, measure.vars = c('predictions')) 
avgs.subset.long$flux.mean = flux.long$value

p2 = ggplot(data=avgs.subset.long,
            aes_string(x=group, y="flux.mean", group=group, colour="value")
) +
  geom_line(size=6) +
  scale_x_reverse() +
  scale_y_continuous(limits = c(0, 8000000)) +
  scale_colour_gradientn(colours=c('purple', 'orange'), limits=c(0,80000), name='Discharge') +
  #scale_colour_gradientn(colours=c('white', 'black')) +
  xlab('Temperature Class') +
  ylab('Flux')

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

ggplot( data=normalized[25:45,], aes(x=temperature.class, y=predictions)) +
  geom_line()


# let's look at the  regressions
par(mfrow=c(2,2))
for(i in 30:40) {
  df = all.avgs[all.avgs$temperature.class==i,]
  plot(df$discharge.mean, df$flux.mean/100000, xlim=c(0,100000), ylim=c(0,80),
       main=i)
  lines(df$discharge.mean, df$predictions/100000, typ='l', col='red')
}

par(mfrow=c(2,2))
i = 20
plot(lms[[i]]$model[2][,1][,1], lms[[i]]$residuals, ylim=c(-500000,500000))
lms[[i]]

# analyze the dip
par(mfrow=c(1,1))
i= 30
df = all.avgs[all.avgs$temperature.class==i,]
plot(df$discharge.mean, df$flux.mean/100000, xlim=c(0,100000), ylim=c(0,80),
     main=i, pch=1)
lines(df$discharge.mean, df$predictions/100000, typ='l', col='red')

i= 35
df = all.avgs[all.avgs$temperature.class==i,]
points(df$discharge.mean, df$flux.mean/100000, xlim=c(0,100000), ylim=c(0,80),
       main=i, pch=2)
lines(df$discharge.mean, df$predictions/100000, typ='l', col='green')

i= 37
df = all.avgs[all.avgs$temperature.class==i,]
points(df$discharge.mean, df$flux.mean/100000, xlim=c(0,100000), ylim=c(0,80),
       main=i, pch=3)
lines(df$discharge.mean, df$predictions/100000, typ='l', col='blue')
