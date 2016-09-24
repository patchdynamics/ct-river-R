# nice script to show rising and falling limbs

T = ts.stemp$SoilTemperature1
tslval.nomax = temp = tslval = falling.step(T)
temp$Temperature[is.na(temp$Temperature)] = min(temp$Temperature, na.rm=TRUE)
tslval.nomax[rollmax(temp$Temperature,150,align='center',fill='extend') == tslval] = NA

tshval.nomax = temp = tshval = rising.step(T)
temp$Temperature[is.na(temp$Temperature)] = min(temp$Temperature, na.rm=TRUE)
tshval.nomax[rollmax(temp$Temperature,150,align='center',fill='extend') == tshval] = NA
  


par(mfrow=c(2,1))
plot(T, main='NARR Average Surface Soil Temperature', ylab="Temperature (K)",
     minor.ticks=FALSE, )

T.match = T
T.match[!(tshval.nomax > 275 & tshval.nomax < 285) | is.na(tshval.nomax)] = NA
lines(T.match, col=c('red'), lwd=3)
plot.xts(tslval.nomax, ylab="Temperature (K)", minor.ticks=FALSE, 
         main='Rising and Falling Only')
lines(tshval.nomax, col='black')
T.match = tshval.nomax
T.match[!(tshval.nomax > 275 & tshval.nomax < 285) | is.na(tshval.nomax)] = NA
lines(T.match, col=c('red'), lwd=3)


# show how freshets don't line up
tshval = rising.step(ts.stemp$SoilTemperature1)
tslval = falling.step(ts.stemp$SoilTemperature1)

times = index(tshval[tshval$Temperature > 280 & tshval$Temperature < 290])
ts.subset = tscopy
ts.subset[ !( index(ts.subset) %in% times ),] = NA
#ts.subset = tscopy[times]
plot(ts.subset$HPOA.mgl)

par(mfrow=c(5,1))
for(i in 111:115){
  plot(ts.subset$HPOA.mgl[.indexyear(ts.subset) == i])
  #df = as.data.frame(ts.subset[.indexyear(ts.subset) == i])
  #plot(df$Discharge, df$HPOA.mgl)
}
