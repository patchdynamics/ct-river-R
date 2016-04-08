library(waterData)

sdate = "1950-01-01"
sdate = "1986-01-01"
edate = "2016-01-01"

thompsonville = importDVs("01184000", code="00060", sdate=sdate, edate=edate)
plotParam(thompsonville)

par(mfrow=c(2,1))
hist(thompsonville$val)
quantiles = quantile(thompsonville$val, probs = c(.25,.5,.75,.9, .99, .999, .9999))
exceedences = c(.75, .5, .25, .1, .01, .001, .0001)
plot(quantiles)

par(mfrow=c(1,1))
plot(quantiles)

probs = c(.25, .5, .75, .8, .85, .9, .95, .98, 1-1/365, 1-1/(5*365), 1-1/(10*365))
probs = c(.25, .5, .75, .9, 1-1/365, 1-1/(5*365), 1-1/(10*365))
quantiles = quantile(thompsonville$val, probs)
plot(1-probs,quantiles)
