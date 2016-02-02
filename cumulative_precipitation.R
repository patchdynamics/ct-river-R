
par(mfrow=c(2,3))
tscombined$Precipitation[is.na(tscombined$Precipitation)] = 0
precip2011 = tscombined$Precipitation[.indexyear(tscombined) == 111]
precip2011.cum = cumsum(precip2011)
plot(precip2011.cum, ylim=c(0,80000))
precip2012 = tscombined$Precipitation[.indexyear(tscombined) == 112]
precip2012.cum = cumsum(precip2012)
plot(precip2012.cum, ylim=c(0,80000))
precip2013 = tscombined$Precipitation[.indexyear(tscombined) == 113]
precip2013.cum = cumsum(precip2013)
plot(precip2013.cum, ylim=c(0,80000))
precip2014 = tscombined$Precipitation[.indexyear(tscombined) == 114]
precip2014.cum = cumsum(precip2014)
plot(precip2014.cum, ylim=c(0,80000))
precip2015 = tscombined$Precipitation[.indexyear(tscombined) == 115]
precip2015.cum = cumsum(precip2015)
plot(precip2015.cum, ylim=c(0,80000))


par(mfrow=c(1,1))
#xaxis = format(strptime(index(as.data.frame(precip2011.cum)$Precipitation), format="%j"), format="%m-%d") 
plot(as.data.frame(precip2011.cum)$Precipitation, typ='l', col="blue")
lines(as.data.frame(precip2012.cum)$Precipitation, typ='l', col="red")
lines(as.data.frame(precip2013.cum)$Precipitation, typ='l', col="orange")
lines(as.data.frame(precip2014.cum)$Precipitation, typ='l', col="purple")
lines(as.data.frame(precip2015.cum)$Precipitation, typ='l', col="green")