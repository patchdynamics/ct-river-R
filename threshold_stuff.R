threshold.receiving$Files = substr(threshold.receiving$Files, 26,33)
threshold.receiving = read.csv('data-precip-percent-threshold_activated.csv')
threshold.receiving$Files = substr(threshold.receiving$Files, 26,33)
threshold.receiving.dates = strptime(threshold.receiving[,1],'%Y%m%d')
threshold.receiving = xts(x=threshold.receiving[,2:9], order.by=threshold.receiving.dates)

plot(threshold.receiving$X1["2014-06-01/2014-09-01"])

par(mfrow=c(2,1))
plot(threshold.receiving$X.1["2014-06-01/2014-09-01"])
plot(tscombined$X.1["2014-06-01/2014-09-01"])

xx = merge(threshold.receiving$X2["2014-06-01/2014-09-01"], tscombined$X2["2014-06-01/2014-09-01"])
