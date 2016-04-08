
ts.copy2 = tscopy
ts.copy2$Discharge.m3s = ts.copy2$Discharge * 0.0283168
winter.spring = as.data.frame(ts.copy2[.indexmon(ts.copy2) %in% c(0,1,2,3)])
late.summer = as.data.frame(ts.copy2[.indexmon(ts.copy2) %in% c(8,9,10)])

par(mfrow=c(1,2))
plot(winter.spring$Discharge.m3s, winter.spring$HPOA.mgl, col='aquamarine4',
     ylim=c(0,4), xlim=c(0,4000),
     ylab='[HPOA] (ppm)', xlab='Discharge (m^3/s)')
points(late.summer$Discharge.m3s, late.summer$HPOA.mgl, col='coral', pch=3)
legend('bottomright', legend=c('Winter', 'Summer'), col=c('aquamarine4', 'coral'), pch=c(1,3))

plot(winter.spring$Discharge.m3s, winter.spring$flux.mgs/1000, col='aquamarine4',
     ylim=c(0,10000), xlim=c(0,4000),
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


