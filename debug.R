window = 2
start = min(tshval)
stop.t = max(tshval)

plots = floor((stop.t - start) / window) + 1
colors = colorRampPalette(c("blue", "red"))( plots ) 


# just the warming phase
next.t = start
i = 1
while(next.t < stop.t) {
  print(next.t)
  min = next.t
  max = min + window
  times = index(tshval[tshval$Temperature >= min & tshval$Temperature < max & tslval$Temperature > 290])
  print(length(times))
  ts.window = tscopy[times]
  
  avgs = flux.avg(ts.window, bin.number=nbins)
  lm.avgs = lm( flux.mean ~ discharge.mean, data=avgs)
  
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
  next.t = next.t + window
  i = i + 1
}
