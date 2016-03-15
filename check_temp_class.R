start = min(tshval)
stop.t = max(tshval)
plots = floor((stop.t - start) / window) + 1
plots = floor(stop.t - start)
colors = colorRampPalette(c("blue", "red"))( plots ) 

nbins = 20
window = 10
step = 1

next.t = start
i = 1
all.avgs = NULL
class.count = NULL
regressions = NULL
lms = NULL
rmserrors = NULL


while(next.t <= stop.t) {
  
#print(next.t)
min = next.t
max = min + window
times = index(tshval[tshval$Temperature >= min & tshval$Temperature < max & is.na(tslval.nomax$Temperature)])
class.count = rbind(class.count, length(times))
ts.window = tscopy[times]
if(length(times) <= 1){
  next.t = next.t + step
  print(next.t)
  i = i + step
  next
}

avgs = flux.avg(ts.window, bin.number=nbins)
lm.avgs = lm( flux.mean ~ poly(discharge.mean, 1), data=avgs)
lms[[i]] <- lm.avgs
#print(summary(lm.avgs)$r.squared)
avgs$temperature.class = i
avgs$temperature.class.base = next.t
#avgs$predictions = predict(lm.avgs)
all.avgs = rbind(all.avgs, avgs)


df = as.data.frame(ts.window)
df = df[!is.nan(df$Discharge),]
predictions = predict(lm.avgs, newdata = data.frame(discharge.mean = df$Discharge))
rmserror = rmse(df$flux.mgs[!is.na(df$flux.mgs)], predictions[!is.na(df$flux.mgs)])
rmserrors[i] = rmserror

if(0){
plot(df$Discharge, df$flux.mgs, main = paste0('T: ',as.integer(next.t),'K  RMSE: ', as.integer(rmserror)),
     xlab='Discharge (cfs)', ylab='HPOA Flux (mg/s)', 
      xlim=c(0,100000), ylim=c(0,8000000))
if(next.t == start) {
  lines(df$Discharge, predictions, typ='l',
        col = colors[i],
        lwd = 3
  )
} else {
  lines(df$Discharge, predictions, col = colors[i], lwd=3)
}
}
next.t = next.t + step
print(next.t)
i = i + step

}





par(mfrow=c(1,1))
plot(rmserrors3, typ='l', lwd=2,main='Root Mean Squared Prediction Error', ylab='RMSE',
     ylim=c(0,300000))
lines(rmserrors5,lwd=2, col='red')
lines(rmserrors10, lwd=2,col='green')
legend('topleft', lwd=2, legend=c('3K', '5K', '10K'), col=c('black', 'red', 'green'),
       title = 'Window Size');

