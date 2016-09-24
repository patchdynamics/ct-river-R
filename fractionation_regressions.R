library(xts)
library(plyr)

fractionation = read.csv('../data/Haddam_Fractionation_201602.csv')

df = fractionation[fractionation$Collection.Date != '',]

Discharge = tscombined$Discharge[!is.na(tscombined$Discharge)]

# HPOA
dfhpoa = df[!is.na(df$HPOA..) & df$HPOA.. != '' & df$HPOA.. !='<NA>' ,]
dfhpoa = data.frame(date = dfhpoa$Collection.Date, DOC = dfhpoa$DOC..ppm.C., HPOA.Percent = dfhpoa$HPOA..)
dfhpoa$HPOA.Percent = as.numeric(substr(as.character(dfhpoa$HPOA.Percent),1,2)) / 100
head(dfhpoa)
times = strptime(dfhpoa$date, '%m/%d/%y')
ts.hpoa = xts(dfhpoa, order.by=times)

ts.hpoa = merge(ts.hpoa, tscombined$FDOM.QSE[times])
ts.hpoa$HPOA = ts.hpoa$DOC * ts.hpoa$HPOA.Percent
formula = HPOA ~ FDOM.QSE
lm.hpoa = lm(formula, as.data.frame(ts.hpoa))
summary(lm.hpoa)

ts.hpoa = merge(ts.hpoa, Discharge[times])

plot(as.vector(ts.hpoa$Discharge), as.vector(ts.hpoa$HPOA))
formula = HPOA ~ Discharge
lm.hpoa.discharge = lm(formula, as.data.frame(ts.hpoa))
summary(lm.hpoa.discharge)


#   HPOA Percentage

times = strptime(dfhpoa$date, '%m/%d/%y')
ts.hpoa = merge(ts.hpoa, Discharge[times])


df.hpoa = as.data.frame(ts.hpoa)

# load tsdischarge.Rdata
limb = tsdischarge$Limb[times]
colors = as.vector(mapvalues(limb, from=c(-1,0,1), to=c('red', 'black', 'blue')))

plot(df.hpoa$Discharge/1000, df.hpoa$HPOA.Percent, col=colors)
formula = HPOA.Percent ~ log(Discharge+1000)
lm.hpoa.discharge = lm(formula, df.hpoa)
#summary(lm.hpoa.discharge)
summary(lm.hpoa.discharge)$adj.r.squared

newdata = data.frame(Discharge=seq(0,100000,100))
prediction = predict(lm.hpoa.discharge, newdata=newdata, interval="confidence")
lines(newdata$Discharge/1000, prediction[,1] ,
      col='blue')
lines(newdata$Discharge/1000, prediction[,2] ,
      col='red')
lines(newdata$Discharge/1000, prediction[,3] ,
      col='red')
legend("topright", bty="n", legend=paste("R2 =", 
                                         format(summary(lm.hpoa.discharge)$adj.r.squared, digits=4)))

# TPIA
dftpia = df[!is.na(df$TPIA..) & df$TPIA.. != '' & df$TPIA.. !='<NA>' ,]
dftpia = data.frame(date = dftpia$Collection.Date, DOC = dftpia$DOC..ppm.C., TPIA.Percent = dftpia$TPIA..)
dftpia$TPIA.Percent = as.numeric(substr(as.character(dftpia$TPIA.Percent),1,2)) / 100
head(dftpia)
times = strptime(dftpia$date, '%m/%d/%y')
ts.tpia = xts(dftpia, order.by=times)

FDOM = tscombined$FDOM.QSE[!is.na(tscombined$FDOM.QSE)]
ts.tpia = merge(ts.tpia, FDOM[times])
ts.tpia$TPIA = ts.tpia$DOC * ts.tpia$TPIA.Percent
formula = TPIA ~ FDOM.QSE
lm.tpia = lm(formula, as.data.frame(ts.tpia))
summary(lm.tpia)

tscombined$TPIA.mgl = predict(lm.tpia, tscombined)
ts = tscombined['2011-01-01/2015-12-31']
plot(ts$HPOA.mgl)
lines(ts$HPOA.mgl + ts$TPIA.mgl, col='red')

plot(ts$TPIA.mgl)

# FOA
dffoa = df[!is.na(df$TPIA..) & df$TPIA.. != '' & df$TPIA.. !='<NA>' & !is.na(df$HPOA..) & df$HPOA.. != '' & df$HPOA.. !='<NA>' ,]
dffoa = data.frame(date = dffoa$Collection.Date, DOC = dffoa$DOC..ppm.C., HPOA.Percent = dffoa$HPOA.., TPIA.Percent = dffoa$TPIA..)
dffoa$FOA.Percent = as.numeric(substr(as.character(dftpia$HPOA.Percent),1,2)) / 100 + as.numeric(substr(as.character(dftpia$TPIA.Percent),1,2)) / 100
head(dffoa)
times = strptime(dffoa$date, '%m/%d/%y')
ts.foa = xts(dffoa, order.by=times)

FDOM = tscombined$FDOM.QSE[!is.na(tscombined$FDOM.QSE)]
ts.foa = merge(ts.foa, FDOM[times])
ts.foa$FOA = ts.foa$DOC * ts.foa$FOA.Percent
formula = FOA ~ FDOM.QSE
lm.foa = lm(formula, as.data.frame(ts.foa))
summary(lm.foa)


library(ggplot2)
ts.df = as.data.frame(ts)
ts.df$Time = index(ts)
ts.df.long = reshape(ts.df, 
                     varying=c('HPOA.mgl', 'TPIA.mgl'), 
                     v.names='Concentration', 
                     timevar='DOM', 
                     times=c('HPOA', 'TPIA'), 
                     direction='long')

t = ggplot(ts.df.long, aes(
                  x = id,
                  y = ts.df.long$Concentration,
                  fill = DOM
                  ))   + geom_area(position = 'stack')  
t + xlab('Time') + ylab('Concentration ppm')

#    Discharge doesn't do much for TPIA
Discharge = tscombined$Discharge[!is.na(tscombined$Discharge)]
ts.tpia = merge(ts.tpia, Discharge[times])

plot(as.vector(ts.tpia$Discharge), as.vector(ts.tpia$TPIA))
formula = TPIA ~ Discharge
lm.tpia.discharge = lm(formula, as.data.frame(ts.tpia))
summary(lm.tpia.discharge)


# more fitting of the %s
# HPI
dfhpi = df[!is.na(df$HPI..) & df$HPI.. != '' & df$HPI.. !='<NA>' ,]
dfhpi = data.frame(date = dfhpi$Collection.Date, DOC = dfhpi$DOC..ppm.C., HPI.Percent = dfhpi$HPI..)
dfhpi$HPI.Percent = as.numeric(substr(as.character(dfhpi$HPI.Percent),1,2)) / 100
head(dfhpi)
times = strptime(dfhpi$date, '%m/%d/%y')
ts.hpi = xts(dfhpi, order.by=times)

ts.hpi = merge(ts.hpi, tscombined$FDOM.QSE[times])
ts.hpi$HPI = ts.hpi$DOC * ts.hpi$HPI.Percent
formula = HPI ~ FDOM.QSE
lm.hpi = lm(formula, as.data.frame(ts.hpi))
summary(lm.hpi)

ts.hpi = merge(ts.hpi, Discharge[times])

df.hpi = as.data.frame(ts.hpi[index(ts.hpi) != "2014-04-15"])

limb = tsdischarge$Limb[times]
colors = as.vector(mapvalues(limb, from=c(-1,0,1), to=c('red', 'black', 'blue')))

plot(df.hpi$Discharge/1000, df.hpi$HPI.Percent, col=colors)
formula = HPI.Percent ~ log(Discharge-4000)
lm.hpi.discharge = lm(formula, df.hpi)
summary(lm.hpi.discharge)
 
newdata = data.frame(Discharge=seq(0,100000,100))
prediction = predict(lm.hpi.discharge, newdata=newdata, interval="confidence")
lines(newdata$Discharge/1000, prediction[,1] ,
      col='blue')
lines(newdata$Discharge/1000, prediction[,2] ,
      col='red')
lines(newdata$Discharge/1000, prediction[,3] ,
      col='red')


 

