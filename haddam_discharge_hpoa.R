library(xts)
discharge = read.csv('../Haddam/discharge/full_record_discharge_date_formatted.csv')
dates = strptime(discharge[,1], '%Y%m%d')
tsd = xts(x=discharge[,2], order.by=dates)



collection.dates = strptime(as.character(fractionation$Collection.Date), '%m/%d/%y')
discharges_for_samples = tsd[collection.dates]

# merge
fractionation_series = xts(x=fractionation, order.by=collection.dates)
fractionation_series = merge(discharges_for_samples, fractionation_series[!is.na(index(fractionation_series))])

fdf = as.data.frame(fractionation_series)
plot(fdf$discharges_for_samples, fdf$DOC..ppm.C.)

# fit HPOA %
model = y ~ poly(x,2)
model = y ~ log(x)


plot(fdf$discharges_for_samples, fdf$HPOA_Percentage)
x = fdf$discharges_for_samples
y = fdf$HPOA_Percentage
x = x[!is.na(y)]
y = y[!is.na(y)]
data = data.frame(x=x, y=y)
lm.discharge.hpoa = lm(model, data)
summary(lm.discharge.hpoa)

plot(fdf$discharges_for_samples/1000, fdf$HPOA_Percentage, 
     xlab='Discharge (x1000 cfs)', ylab='HPOA %',
     col='blue'
     )
newdata = data.frame(x=seq(min(fdf$discharges_for_samples),max(fdf$discharges_for_samples),100))
lines(newdata$x/1000, predict(lm.discharge.hpoa, newdata=newdata),
      col='red')
legend("topright", bty="n", 
       legend=paste("R2 =",format(summary(lm.discharge.hpoa)$adj.r.squared, digits=4)))


# fit HPI %
hpi_fractionation = fractionation
hpi_fractionation_series = xts(x=hpi_fractionation, order.by=collection.dates)


hpi_fractionation = fractionation[as.character(fractionation$Collection.Date) != "4/15/14",]
hpi_fractionation_series = xts(x=hpi_fractionation, order.by=collection.dates[as.character(collection.dates) != "2014-04-15"])
hpi_fractionation_series = merge(tscombined$Discharge, hpi_fractionation_series[!is.na(index(hpi_fractionation_series))])
hpi_fdf = as.data.frame(hpi_fractionation_series)


model = y ~ poly(x,2)


model = y ~ log(x-4000)

plot(hpi_fdf$Discharge, hpi_fdf$HPI_Percentage)
x = hpi_fdf$Discharge
y = hpi_fdf$HPI_Percentage
x = x[!is.na(y)]
y = y[!is.na(y)]
data = data.frame(x=x, y=y)
lm.discharge.hpi = lm(model, data)
summary(lm.discharge.hpi)

plot(x/1000, y,
     xlab='Discharge (x1000 cfs)', ylab='HPI %',
     col='blue'
)
newdata = data.frame(x=seq(min(x),max(x),100))
lines(newdata$x/1000, predict(lm.discharge.hpi, newdata=newdata),
      col='red')

hpi_outlier = fractionation[as.character(fractionation$Collection.Date) == "4/15/14",]
points(as.vector(tsd['2014-04-15'])/1000, hpi_outlier$HPI_Percentage, col='orange')
legend("topright", bty="n", 
       legend=paste("R2 =",format(summary(lm.discharge.hpi)$adj.r.squared, digits=4)))



# Combined Plot
plot(fdf$discharges_for_samples/1000, fdf$HPOA_Percentage*100, 
     xlab='Discharge (x1000 cfs)', ylab='% of DOC',
     col='blue',
     ylim=c(0,60)
)
newdata = data.frame(x=seq(min(fdf$discharges_for_samples),max(fdf$discharges_for_samples),100))
lines(newdata$x/1000, predict(lm.discharge.hpoa, newdata=newdata)*100,
      col='blue', lwd=2)

points(x/1000, y*100,
     xlab='Discharge (x1000 cfs)', ylab='HPI %',
     col='red', pch=2
)
newdata = data.frame(x=seq(min(x),max(x),100))
lines(newdata$x/1000, predict(lm.discharge.hpi, newdata=newdata)*100,
      col='red')

hpi_outlier = fractionation[as.character(fractionation$Collection.Date) == "4/15/14",]
points(as.vector(tsd['2014-04-15'])/1000, hpi_outlier$HPI_Percentage*100, col='green', pch=2)

legend("bottomleft", pch=c(1,2,2), col=c('blue', 'red', 'green'),
       legend=c(
         paste("HPOA, r^2 =",format(summary(lm.discharge.hpoa)$adj.r.squared, digits=2), ' p < .001'),
         paste("HPI, r^2 =",format(summary(lm.discharge.hpi)$adj.r.squared, digits=2), 'p < .001'),
         'HPI outlier removed'
         ),
       cex=.8
)



