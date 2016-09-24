#fractionation.df
names(fractionation.df)

par(mfrow=c(1,2))
attach(fractionation.df)
lm.hpoa = lm(HPOA.ppm ~ DOC.ppm)
lm.hpi = lm(HPI.ppm ~ DOC.ppm)

plot(DOC.ppm, HPOA.ppm, ylim=c(0,3), col='chartreuse3',
     ylab='mg/l', xlab='DOC (mg/l)')
points(DOC.ppm, HPI.ppm, col='brown1')
legend('topleft', legend=c('[HPOA] r^2=0.87, p << .001', 
                           '[HPI] r^2=.18, p=.02'), 
       col=c('chartreuse3', 'brown1'), pch=1, cex=.8)

lm.hpoa = lm(HPOA.ppm ~ fractionation.df$Discharge)
lm.hpi = lm(HPI.ppm ~ fractionation.df$Discharge)
plot(fractionation.df$Discharge * 0.0283168, HPOA.ppm, ylim=c(0,3), col='chartreuse3',
     ylab='mg/l', xlab='Discharge (m^3/s)')
points(fractionation.df$Discharge * 0.0283168, HPI.ppm, col='brown1')

points(fractionation.df$Discharge * 0.0283168, TPIA.ppm, col='orange')
Remainder.ppm = DOC.ppm - (HPOA.ppm + HPI.ppm + TPIA.ppm)
points(fractionation.df$Discharge * 0.0283168, Remainder.ppm, col='blue')

detach(fractionation.df)


attach(fractionation.df)
OA.ppm = HPOA.ppm + HPI.ppm + TPIA.ppm
fractionation.df$Remainder.ppm = DOC.ppm - (HPOA.ppm + HPI.ppm + TPIA.ppm)
HTOA.ppm = HPOA.ppm + TPIA.ppm

lm.oa = lm(OA.ppm ~ DOC.ppm)
lm.hpoa = lm(HPOA.ppm ~ DOC.ppm)
lm.hpi = lm(HPI.ppm ~ DOC.ppm)
lm.tpia = lm(TPIA.ppm ~ DOC.ppm)
lm.remainder = lm(Remainder.ppm ~ DOC.ppm)
lm.htoa = lm(HTOA.ppm ~ DOC.ppm)

par(mfrow=c(1,1))
plot(DOC.ppm, OA.ppm, col='coral4', ylim=c(0,6), ylab='mg/l')
lines(lm.oa$model$DOC.ppm, predict(lm.oa), col='coral4')
points(DOC.ppm, HPOA.ppm, col='chartreuse3')
lines(lm.hpoa$model$DOC.ppm, predict(lm.hpoa), col='chartreuse3')
lines(DOC.ppm, DOC.ppm, col='pink')
summary(lm.oa)
lines(lm.oa$model$DOC.ppm, predict(lm.oa))
points(DOC.ppm, Remainder.ppm, col='purple')
lines(lm.remainder$model$DOC.ppm, predict(lm.remainder), col='purple')
#points(DOC.ppm, HPI.ppm, col='red')
lines(lm.hpi$model$DOC.ppm, predict(lm.hpi), col='red')
points(DOC.ppm, TPIA.ppm, col='blue')
lines(lm.tpia$model$DOC.ppm, predict(lm.tpia), col='blue')
# not very scattered, excellent r^2!
points(DOC.ppm, HTOA.ppm, col='black', pch=2)
lines(lm.htoa$model$DOC.ppm, predict(lm.htoa), col='black')
legend('topleft', c('OA', 'HPOA', 'DOC', 'Remainder', 'TPIA', 'HTOA'), 
               col=c('coral4', 'chartreuse3', 'pink', 'purple', 'blue', 'black'),
       pch=1)


par(mfrow=c(1,1))
plot(DOC.ppm, OA.ppm, col='coral2', ylim=c(0,6), ylab='mg/l', xlab='mg/l',
     main='Fractional Contributions to Total DOC')
lines(lm.oa$model$DOC.ppm, predict(lm.oa), col='coral2')
points(DOC.ppm, HPOA.ppm, col='darkorange2', pch=0)
lines(lm.hpoa$model$DOC.ppm, predict(lm.hpoa), col='darkorange2')
lines(DOC.ppm, DOC.ppm, col='blue')
points(DOC.ppm, Remainder.ppm, col='chartreuse3', pch=5)
lines(lm.remainder$model$DOC.ppm, predict(lm.remainder), col='chartreuse3')
#points(DOC.ppm, HPI.ppm, col='red')
#lines(lm.hpi$model$DOC.ppm, predict(lm.hpi), col='red')
#points(DOC.ppm, TPIA.ppm, col='blue')
#lines(lm.tpia$model$DOC.ppm, predict(lm.tpia), col='blue')
# not very scattered, excellent r^2!
points(DOC.ppm, HTOA.ppm, col='black', pch=2)
lines(lm.htoa$model$DOC.ppm, predict(lm.htoa), col='black')

# A silly line
# OO.ppm = TPIA.ppm + Remainder.ppm
# points(DOC.ppm, OO.ppm, col='magenta')
# lm.oo = lm(OO.ppm ~ DOC.ppm)
# lines(lm.oo$model$DOC.ppm, predict(lm.oo), col='magenta')


legend('topleft', c('DOC 1:1', 'OA', 'HPOA+TPIA','HPOA Only', 'Not OA'), 
       col=c('blue', 'coral2', 'black', 'darkorange2', 'chartreuse3'),
       lwd=c(1,NA,NA,NA,NA), pch=c(NA,1,2,0,5))
summary(lm.oa)
summary(lm.hpoa)
summary(lm.htoa)
summary(lm.remainder)
summary(lm.hpi)  # 'nearly flat'
 

par(mfrow=c(2,2))
plot(DOC.ppm, HPOA.ppm, ylim=c(0,3), col='chartreuse3',
     ylab='mg/l', xlab='DOC (mg/l)')
points(DOC.ppm, HPI.ppm, col='brown1')
#points(DOC.ppm[Month>5], HPOA.ppm[Month>5], col='green', pch=4, cex=1)

legend('topleft', legend=c('[HPOA]', # r^2=0.87, p << .001', 
                           '[HPI]'# r^2=.18, p=.02'
                           ), 
       col=c('chartreuse3', 'brown1'), pch=1, cex=.8)

plot(DOC.ppm, TPIA.ppm, col='orange',  ylim=c(0,3), xlab='DOC (mg/l)', ylab='')
points(DOC.ppm, Remainder.ppm, col='blue')
legend('topleft', legend=c('[TPIA]', 
                           '[Remaining DOC]'), 
       col=c('orange', 'blue'), pch=1, cex=.8)


lm.hpoa = lm(HPOA.ppm ~ fractionation.df$Discharge)
lm.hpi = lm(HPI.ppm ~ fractionation.df$Discharge)
lm.tpia = lm(TPIA.ppm ~ fractionation.df$Discharge)

lm.remainder = lm(Remainder.ppm ~ fractionation.df$Discharge)

lm.remainder = lm(Remainder.ppm[Month <= 5] ~ fractionation.df$Discharge[Month <= 5])
summary(lm.remainder)
lm.remainder = lm(Remainder.ppm[Month > 5] ~ fractionation.df$Discharge[Month > 5])
summary(lm.remainder)


cutoff = 5
plot(fractionation.df$Discharge* 0.0283168, HPOA.ppm, 
     ylim=c(0,3), xlim=c(0,3500), col='chartreuse3',
     ylab='mg/l', xlab='Discharge (m^3/s)')
points(fractionation.df$Discharge[fractionation.df$Month > cutoff ] * 0.0283168, HPOA.ppm[fractionation.df$Month > cutoff ], 
      col='chartreuse3',pch=4, cex=.5)
#points(fractionation.df$Discharge * 0.0283168, 
#       DOC.ppm, 
#       col='black')

points(fractionation.df$Discharge * 0.0283168, HPI.ppm, col='brown1')
points(fractionation.df$Discharge[fractionation.df$Month > cutoff ] * 0.0283168, HPI.ppm[fractionation.df$Month > cutoff ], 
       col='brown1',pch=4, cex=.5)

plot(fractionation.df$Discharge  * 0.0283168, TPIA.ppm , col='orange', ylim=c(0,3), xlab='Discharge (m^3/s)', ylab='')
points(fractionation.df$Discharge[fractionation.df$Month > cutoff ]  * 0.0283168, TPIA.ppm[fractionation.df$Month > cutoff ] , col='orange', ylim=c(0,3), xlab='Discharge (m^3/s)', ylab='',
     pch=4, cex=.5)

points(fractionation.df$Discharge * 0.0283168, Remainder.ppm, col='blue')
points(fractionation.df$Discharge[fractionation.df$Month > cutoff ]  * 0.0283168, Remainder.ppm[fractionation.df$Month > cutoff ] ,
       col='blue',pch=4, cex=.5)

library(bbmle)

lm.hpi = lm(HPI.ppm ~ Discharge)
summary(lm.hpi)


nfit.0 = mle2(HPI.ppm ~ dnorm(mean = m * Discharge + b, sd = sd),
              start=list(m = 0, b=1.5, sd=1), data = fractionation.df[!is.na(HPI.ppm),])

nfit.season = mle2(HPI.ppm ~ dnorm(mean = m * Discharge + b, sd = sd),
                   start=list(m = 0, b=1.5, sd=1), data = fractionation.df[!is.na(HPI.ppm),],
                   parameters = list( m ~ season, b ~ season))

anova(nfit.0, nfit.season)


nfit.0 = mle2(HPOA.ppm ~ dnorm(mean = m * Discharge + b, sd = sd),
              start=list(m = 0, b=1.5, sd=1), data = fractionation.df[!is.na(HPOA.ppm),])

nfit.season = mle2(HPOA.ppm ~ dnorm(mean = m * Discharge + b, sd = sd),
                   start=list(m = 0, b=1.5, sd=1), data = fractionation.df[!is.na(HPOA.ppm),],
                   parameters = list( m ~ season, b ~ season))

anova(nfit.0, nfit.season)



nfit.0 = mle2(HPOA.ppm ~ dnorm(mean = m * DOC.ppm + b, sd = sd),
              start=list(m = 0, b=1.5, sd=1), data = fractionation.df[!is.na(HPOA.ppm),])

nfit.season = mle2(HPOA.ppm ~ dnorm(mean = m * DOC.ppm + b, sd = sd),
                   start=list(m = 0, b=1.5, sd=1), data = fractionation.df[!is.na(HPOA.ppm),],
                   parameters = list( m ~ season, b ~ season))

anova(nfit.0, nfit.season)





nfit.0 = mle2(TPIA.ppm ~ dnorm(mean = m * Discharge + b, sd = sd),
              start=list(m = 0, b=1.5, sd=1), data = fractionation.df[!is.na(TPIA.ppm),])

nfit.season = mle2(TPIA.ppm ~ dnorm(mean = m * Discharge + b, sd = sd),
                   start=list(m = 0, b=1.5, sd=1), data = fractionation.df[!is.na(TPIA.ppm),],
                   parameters = list( m ~ season, b ~ season))

anova(nfit.0, nfit.season)



nfit.0 = mle2(Remainder.ppm ~ dnorm(mean = m * Discharge + b, sd = sd),
              start=list(m = 0, b=1.5, sd=1), data = fractionation.df[!is.na(Remainder.ppm),])

nfit.season = mle2(Remainder.ppm ~ dnorm(mean = m * Discharge + b, sd = sd),
                   start=list(m = 0, b=1.5, sd=1), data = fractionation.df[!is.na(Remainder.ppm),],
                   parameters = list( m ~ season, b ~ season))

anova(nfit.0, nfit.season)


#
# check time of year
#
par(mfrow=c(2,2))
fractionation.season = fractionation.df[fractionation.df$Month > 5,]
fractionation.season$Remainder.ppm = fractionation.season$DOC.ppm - (fractionation.season$HPOA.ppm + fractionation.season$HPI.ppm + fractionation.season$TPIA.ppm)

plot(fractionation.season$DOC.ppm, fractionation.season$HPOA.ppm, ylim=c(0,3), col='chartreuse3',
     ylab='mg/l', xlab='DOC (mg/l)', xlim=c(2.5,6))
points(fractionation.season$DOC.ppm, fractionation.season$HPI.ppm, col='brown1')
legend('topleft', legend=c('[HPOA]', # r^2=0.87, p << .001', 
                           '[HPI]'# r^2=.18, p=.02'
), 
col=c('chartreuse3', 'brown1'), pch=1, cex=.8)

plot(fractionation.season$DOC.ppm, fractionation.season$TPIA.ppm, col='orange', xlim=c(2.5,6), ylim=c(0,3), xlab='DOC (mg/l)', ylab='')
points(fractionation.season$DOC.ppm, fractionation.season$Remainder.ppm, col='blue')
legend('topleft', legend=c('[TPIA]', 
                           '[Remaining DOC]'), 
       col=c('orange', 'blue'), pch=1, cex=.8)


#lm.hpoa = lm(HPOA.ppm ~ fractionation.df$Discharge)
#lm.hpi = lm(HPI.ppm ~ fractionation.df$Discharge)
#lm.tpia = lm(TPIA.ppm ~ fractionation.df$Discharge)
#lm.remainder = lm(Remainder.ppm ~ fractionation.df$Discharge)

plot(fractionation.season$Discharge * 0.0283168, fractionation.season$HPOA.ppm, ylim=c(0,3), col='chartreuse3',
     ylab='mg/l', xlab='Discharge (m^3/s)', xlim=c(0,3500))
points(fractionation.season$Discharge * 0.0283168, fractionation.season$HPI.ppm, col='brown1')

plot(fractionation.season$Discharge * 0.0283168, fractionation.season$TPIA.ppm, col='orange', ylim=c(0,3), xlab='Discharge (m^3/s)', ylab='', xlim=c(0,3500))
points(fractionation.season$Discharge * 0.0283168, fractionation.season$Remainder.ppm, col='blue')







detach(fractionation.df)


