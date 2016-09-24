


ts.copy2 = tscopy
ts.copy2$Discharge.m3s = ts.copy2$Discharge * 0.0283168
winter.spring = as.data.frame(ts.copy2[.indexmon(ts.copy2) %in% c(0,1,2,3)])
late.summer = as.data.frame(ts.copy2[.indexmon(ts.copy2) %in% c(8,9,10)])

ts.copy2$flux.mgs.2 = ts.copy2$Discharge * 28.3 * ts.copy2$HPOA.mgl / 1000
plot(ts.copy2$flux.mgs.2)
plot(ts.copy2$flux.mgs / 1000)

par(mfrow=c(1,2))
plot(winter.spring$Discharge.m3s, winter.spring$HPOA.mgl, col='aquamarine4',
     ylim=c(0,4), xlim=c(0,4000),
     ylab='[HPOA] (ppm)', xlab='Discharge (m^3/s)')
points(late.summer$Discharge.m3s, late.summer$HPOA.mgl, col='coral', pch=3)
legend('bottomright', legend=c('Winter', 'Summer'), col=c('aquamarine4', 'coral'), pch=c(1,3))

plot(winter.spring$Discharge.m3s, winter.spring$flux.mgs/1000, col='aquamarine4',
     ylim=c(0,10000), 
     xlim=c(0,4000),
     ylab='HPOA Flux (g/s)', xlab='Discharge (m^3/s)')
points(late.summer$Discharge.m3s, late.summer$flux.mgs/1000, col='coral',pch=3)





par(mfrow=c(2,1))
plot(winter.spring$Discharge.m3s, winter.spring$HPOA.mgl, col='aquamarine4',
     ylim=c(0,4), xlim=c(0,4000),
     ylab='[HPOA] (ppm)', xlab='')
points(late.summer$Discharge.m3s, late.summer$HPOA.mgl, col='coral', pch=3)
legend('bottomright', legend=c('Winter', 'Summer'), col=c('aquamarine4', 'coral'),
       pch=c(1,3), cex=.8)

plot(winter.spring$Discharge.m3s, winter.spring$flux.mgs/1000, col='aquamarine4',
     ylim=c(0,10000), xlim=c(0,4000),
     ylab='HPOA Flux (g/s)', xlab='Discharge (m^3/s)')
points(late.summer$Discharge.m3s, late.summer$flux.mgs/1000, col='coral',pch=3)


# check for different populations
winter.spring$season = 'winter.spring'
late.summer$season = 'late.fall'
all.data = rbind(winter.spring, late.summer)

all.data = all.data[!is.na(all.data$Discharge.m3s) & !is.na(all.data$HPOA.mgl),]

nfit.0 = mle2(HPOA.mgl ~ dnorm(mean = m * Discharge.m3s + b, sd = 1),
              start=list(m = 1/1000 , b=1.5), data = all.data)

nfit.season = mle2(HPOA.mgl ~ dnorm(mean = m * Discharge.m3s + b, sd = 1),
              start=list(m = 1/1000 , b=1.5), data = all.data,
              parameters = list( m ~ season, b ~ season))

anova(nfit.0, nfit.season)


# flux

all.data = rbind(winter.spring, late.summer)

all.data = all.data[!is.na(all.data$Discharge.m3s) & !is.na(all.data$flux.mgs),]

nfit.0 = mle2(flux.mgs ~ dnorm(mean = m * Discharge.m3s + b, sd = 1),
              start=list(m = 4000000/1000 , b=0), data = all.data)

nfit.season = mle2(flux.mgs ~ dnorm(mean = m * Discharge.m3s + b, sd = 1),
                   start=list(m = 4000000/1000 , b=0), data = all.data,
                   parameters = list( m ~ season, b ~ season))

anova(nfit.0, nfit.season)

p = predict(nfit.season)


# strip
all.data = rbind(winter.spring, late.summer)
all.data = all.data[!is.na(all.data$Discharge.m3s) & !is.na(all.data$HPOA.mgl),]
strip = 10
part.data = all.data[1:floor(nrow(all.data)/strip)*strip,]
plot(part.data$HPOA.mgl[1:(nrow(part.data)-1)], part.data$HPOA.mgl[2:(nrow(part.data))],
     col=c('red', 'blue')[(part.data$season == 'late.fall')+1]
     )

lm.corr = lm(part.data$HPOA.mgl[1:(nrow(part.data)-1)] ~ part.data$HPOA.mgl[2:(nrow(part.data))])
summary(lm.corr)

nfit.0 = mle2(HPOA.mgl ~ dnorm(mean = m * Discharge.m3s + b, sd = sd),
              start=list(m = 1/1000 , b=1.5, sd=1), data = part.data)

nfit.season = mle2(HPOA.mgl ~ dnorm(mean = m * Discharge.m3s + b, sd = sd),
                   start=list(m = 1/1000 , b=1.5, sd=1), data = part.data,
                   parameters = list( m ~ season, b ~ season))

anova(nfit.0, nfit.season)
anova(nfit.0, nfit.season)[10]

# monte carlo
all.data = rbind(winter.spring, late.summer)
all.data = all.data[!is.na(all.data$Discharge.m3s) & !is.na(all.data$HPOA.mgl),]
power=NULL
for(i in 1:1000){
  part.data = all.data[sample(1:nrow(all.data), nrow(all.data)/10),]
  
  nfit.0 = mle2(HPOA.mgl ~ dnorm(mean = m * Discharge.m3s + b, sd = sd),
                start=list(m = 1/1000 , b=1.5, sd=1), data = part.data)
  
  nfit.season = mle2(HPOA.mgl ~ dnorm(mean = m * Discharge.m3s + b, sd = sd),
                     start=list(m = 1/1000 , b=1.5, sd=1), data = part.data,
                     parameters = list( m ~ season, b ~ season))
  
  power = rbind(power, anova(nfit.0, nfit.season)[10])
  
}
mean(power)


all.data = rbind(winter.spring, late.summer)
all.data = all.data[!is.na(all.data$Discharge.m3s) & !is.na(all.data$flux.mgs),]
power=NULL
for(i in 1:1000){
  part.data = all.data[sample(1:nrow(all.data), nrow(all.data)/10),]
  
  nfit.0 = mle2(flux.mgs ~ dnorm(mean = m * Discharge.m3s + b, sd = sd),
                start=list(m = 4000000/1000 , b=0, sd=1), data = part.data,
                method='SANN')
  
  nfit.season = mle2(flux.mgs ~ dnorm(mean = m * Discharge.m3s + b, sd = sd),
                     start=list(m = 4000000/1000 , b=0, sd=1), data = part.data,
                     parameters = list( m ~ season, b ~ season),
                     method='SANN')
  
  anova(nfit.0, nfit.season)
  power = rbind(power, anova(nfit.0, nfit.season)[10])
  
  plot(part.data$Discharge.m3s, part.data$flux.mgs, col=c('red', 'blue')[as.numeric(factor(part.data$season))])
  p = bbmle::predict(nfit.season)
  lines(part.data$Discharge.m3s[part.data$season == 'late.fall'], p[part.data$season == 'late.fall'], col='red')  
  lines(part.data$Discharge.m3s[part.data$season == 'winter.spring'], p[part.data$season == 'winter.spring'], col='blue')  
  
}
mean(power)

par(mfrow=c(1,2))
df = as.data.frame(ts.copy2)
plot(df$Discharge.m3s, df$HPOA.mgl, col='coral3',
     ylim=c(0,4), xlim=c(0,4000),
     ylab='[HPOA] (mg/l)', xlab='Discharge (m^3/s)')
lm.model= lm(HPOA.mgl ~ Discharge.m3s, df)
pvalue = coeftest(lm.model, NeweyWest(lm.model))
lines(lm.model$model$Discharge.m3s, predict(lm.model), lwd=2)
legend('bottomright', legend='p = .09', lwd=2)

plot(winter.spring$Discharge.m3s, winter.spring$HPOA.mgl, col='aquamarine4',
     ylim=c(0,4), xlim=c(0,4000),
     ylab='[HPOA] (mg/l)', xlab='Discharge (m^3/s)')
lm.model= lm(HPOA.mgl ~ Discharge.m3s, winter.spring)
pvalue1 = coeftest(lm.model, NeweyWest(lm.model))
lines(lm.model$model$Discharge.m3s, predict(lm.model), lwd=2, col='aquamarine4')
#legend('bottomright', legend='p = .09', lwd=2)

points(late.summer$Discharge.m3s, late.summer$HPOA.mgl, col='coral', pch=3)
legend('bottomright', legend=c('Winter', 'Summer'), col=c('aquamarine4', 'coral'), pch=c(1,3))
lm.model= lm(HPOA.mgl ~ Discharge.m3s, late.summer)
pvalue1 = coeftest(lm.model, NeweyWest(lm.model))
lines(lm.model$model$Discharge.m3s, predict(lm.model), lwd=2, col='coral')
legend('bottomright', legend=c('Summer, p << .001', 'Winter, p = .21'), lwd=2, col=c('coral', 'aquamarine4'))



# everything
df = as.data.frame(tscopy)
plot(df$Discharge, df$flux.mgs, col='coral4')
lm.model = lm(flux.mgs ~ Discharge, df)
p = predict(lm.model)
lines(lm.model$model$Discharge, p)
rmse(lm.model$model$HPOA.mgl, p)
coeftest(lm.model, NeweyWest(lm.model))
legend('bottomright', legend=c(p << .001))

