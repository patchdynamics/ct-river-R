# first get discharges and sort them

discharge.min = min(discharges)
discharge.max = max(discharges)
discharges[discharges < 0] = 0

bin.number = 50 
interval = (discharge.max - discharge.min) / bin.number
binned = cut(discharges, (0:bin.number) * interval)
df = data.frame(discharge= discharges, bin = binned)

flow.freq = ddply(df, ~bin, summarise, bin.count = length(discharge), .drop=FALSE)
flow.freq$freq = flow.freq$bin.count / length(discharges)
plot((0:bin.number) * interval, flow.freq$freq, typ='l',
     xlab = 'Discharge', ylab='Flow Frequency')

flux = 1437107 + (0:bin.number) * interval * 3217320  
plot((0:bin.number) * interval, flux, typ='l')
     
rating = flow.freq$freq * flux
plot((0:bin.number) * interval, rating, typ='l')


plot((0:bin.number) * interval, flow.freq$freq/max(flow.freq$freq), typ='l', col='red',
     ylab = 'Normalized Rating', xlab='Discharge (cfs)', lwd=c(1.5,1.5)
     )
lines((0:bin.number) * interval, flux/max(flux), typ='l', col='blue', lwd=c(1.5,1.5))
lines((0:bin.number) * interval, rating/max(rating), typ='l', col='green', lwd=c(1.5,1.5))
legend("topright", c('Flow Frequency', 'Flux Rating Curve', 'Effective Discharge'),
       col=c('red', 'blue', 'green'), lwd=c(1.5,1.5))

