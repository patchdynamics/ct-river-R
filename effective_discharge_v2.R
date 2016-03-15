library(lubridate) 
library(plyr)

ts.ordered = as.data.frame(ts)[order(ts$Discharge),]
ts.ordered = ts.ordered[!is.na(ts.ordered$Discharge) & !is.nan(ts.ordered$Discharge),]
ts.ordered = ts.ordered[!is.na(ts.ordered$HPOA.mgl) & !is.nan(ts.ordered$HPOA.mgl),]
ts.ordered = ts.ordered[ts.ordered$Discharge > 0,]
tail(ts.ordered$Discharge, 10)
head(ts.ordered$Discharge, 10)

flux.mgs = ts.ordered$Discharge * (ts.ordered$HPOA.mgl / 1000) * 30.48^3  # mg/s
lndf = data.frame(
                  dates = as.Date(rownames(ts.ordered)),
                  discharge = ts.ordered$Discharge, 
                  flux = flux.mgs,
                  lndischarge = log(ts.ordered$Discharge), 
                  lnflux = log(flux.mgs))

pt.class = cut(lubridate::month(lndf$dates), c(0,5,9,12))
plot(exp(lndf$lndischarge), exp(lndf$lnflux),
     col=rainbow(3)[pt.class],  # Class these by TEMP ranges
     pch = 4
)
legend(
      x="topleft", 
      legend=c("Spring", "Summer", "Fall"),
      col=rainbow(3),
      pch = 4
       )

# just do the binning to get it over with
lndf$bin = cut(lndf$lndischarge, c(7,8,9,10,11,12) )
ddply(lndf, ~bin, summarise, lndischarge.mean=mean(lndischarge), lnflux.mean=mean(lnflux))

# lets bin by # in group
lndf$bin = cut2(lndf$discharge, g=100)
avgs = ddply(lndf, ~bin, summarise, discharge.mean=mean(discharge), flux.mean=mean(flux))
head(avgs)
tail(avgs)
plot(avgs$discharge.mean, avgs$flux.mean, xlab='Discharge (cfs)', ylab='HPOA Flux (mg/s)')

# now split it by the temp ranges
tscopy = ts
tscopy$flux.mgs = ts$Discharge * (ts$HPOA.mgl / 1000) * 30.48^3  # mg/s

timesbelow = index(tshval[tshval$Temperature < 280.15])
tsfreezing = tscopy[timesbelow]

times2 = index(tshval[tshval$Temperature >= 280.15 & tshval$Temperature < 290.15])
tsfreshet = tscopy[times2]

times2 = index(tshval[tshval$Temperature >= 290.15 & tshval$Temperature < 295.15])
tsafterthaw = tscopy[times2]

times2 = index(tshval[tshval$Temperature >= 295.15 & tslval$Temperature >= 295.15])
tssummer = tscopy[times2]

times2 = index(tshval[tslval$Temperature >= 285.15 & tslval$Temperature <= 295.15])
tsfall = tscopy[times2]



ts.season = tssummer

flux.avg = function(ts.season, bin.number = 100) {
  ts.ordered = as.data.frame(ts.season)[order(ts.season$Discharge),]
  ts.ordered = ts.ordered[!is.na(ts.ordered$Discharge) & !is.nan(ts.ordered$Discharge),]
  ts.ordered = ts.ordered[!is.na(ts.ordered$HPOA.mgl) & !is.nan(ts.ordered$HPOA.mgl),]
  ts.ordered = ts.ordered[ts.ordered$Discharge > 0,]
  #ts.ordered = ts.ordered[!is.na(ts.ordered$flux.mgs),]  #???

  lndf = data.frame(
    dates = as.Date(rownames(ts.ordered)),
    discharge = ts.ordered$Discharge, 
    flux = ts.ordered$flux.mgs,
    lndischarge = log(ts.ordered$Discharge), 
    lnflux = log(ts.ordered$flux.mgs))

  lndf$bin = cut2(lndf$discharge, g=bin.number)
  avgs = ddply(lndf, ~bin, summarise, discharge.mean=mean(discharge), flux.mean=mean(flux), n=length(flux))
  return(avgs)
}

nbins = 50
# freezing
avgs =  flux.avg(tsfreezing, bin.number=nbins)
lm.avgs = lm( flux.mean ~ discharge.mean, data=avgs)
plot(avgs$discharge.mean, avgs$flux.mean, xlab='Discharge (cfs)', ylab='HPOA Flux (mg/s)', 
     xlim=c(0,100000), 
     ylim=c(0, 8000000), 
     col='blue',
#     log=c('x','y')
     )
lines(avgs$discharge.mean, predict(lm.avgs), col='purple')

# summer
avgs =  flux.avg(tssummer, bin.number=nbins)
lm.avgs.outlier = lm( flux.mean ~ exp(discharge.mean/100000), data=avgs)

avgs.cleaned = avgs[-nrow(avgs),]
lm.avgs = lm( flux.mean ~ discharge.mean, data=avgs.cleaned)

points(avgs$discharge.mean, avgs$flux.mean, col='red')
lines(avgs.cleaned$discharge.mean, predict(lm.avgs), col='orange')

# fall
avgs = flux.avg(tsfall, bin.number=nbins)
lm.avgs = lm( flux.mean ~ discharge.mean, data=avgs)
points(avgs$discharge.mean, avgs$flux.mean, col='green')
lines(avgs$discharge.mean, predict(lm.avgs), col='aquamarine3')

# after thaw
#avgs = flux.avg(tsafterthaw, bin.number=nbins)
#lm.avgs = lm( flux.mean ~ discharge.mean, data=avgs)
#points(avgs$discharge.mean, avgs$flux.mean, col='chartreuse1')
#lines(avgs$discharge.mean, predict(lm.avgs), col='chartreuse1')

# freshet
#avgs = flux.avg(tsfreshet, bin.number=nbins)
#lm.avgs = lm( flux.mean ~ discharge.mean, data=avgs)
#points(avgs$discharge.mean, avgs$flux.mean, col='darkgoldenrod3')
#lines(avgs$discharge.mean, predict(lm.avgs), col='darkgoldenrod3')


legend(
  x="topleft", 
  legend=c("Winter", "Summer", "Fall"),
  col=c('blue', 'red', 'green'),
  pch = 1,
  title = 'Soil Temperature Season'
)



# roll a 10 degree window
window = 10
start = min(tshval)
stop.t = max(tshval) - window + 1

#plot.new()
next.t = start
while(next.t < stop.t) {
  print(next.t)
  min = next.t
  max = min + window
  times = index(tshval[tshval$Temperature >= min & tshval$Temperature < max])
  ts.window = tscopy[times2]
  
  avgs = flux.avg(ts.window, bin.number=nbins)
  lm.avgs = lm( flux.mean ~ discharge.mean, data=avgs)
  
  plot(avgs$discharge.mean, predict(lm.avgs), typ='l', add='T')
  next.t = next.t + window
}



