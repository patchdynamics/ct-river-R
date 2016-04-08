library(bbmle)
library(xts)
library(Hmisc)

all.data = as.data.frame(tscopy2)
all.data$Discharge.m3s = all.data$Discharge * 0.028316
all.data$Month = cut(.indexmon(tscopy2), breaks=c(-1,5,9,12))
                         

all.data = all.data[!is.na(all.data$Discharge.m3s) & !is.na(all.data$HPOA.mgl),]

part.data = all.data[sample(1:nrow(all.data), nrow(all.data)/9),]
part.data$Limb = factor(part.data$Limb)

x1 = part.data$HPOA.mgl[-nrow(part.data)]
x2 = part.data$HPOA.mgl[-1]
summary(lm(x2 ~ x1))
coeftest(lm(x2 ~ x1))[8] 


nfit.0 = mle2(HPOA.mgl ~ dnorm(mean = m * Discharge.m3s + b, sd = 1),
              start=list(m = 1/1000 , b=0), data = part.data)

nfit.limb = mle2(HPOA.mgl ~ dnorm(mean = m * Discharge.m3s + b, sd = 1),
                   start=list(m = 1/1000 , b=0), data = part.data,
                   parameters = list( m ~ Limb, b ~ Limb))

anova(nfit.0, nfit.limb)
anova(nfit.0, nfit.limb)[10]


nfit.mon = mle2(HPOA.mgl ~ dnorm(mean = m * Discharge.m3s + b, sd = 1),
                 start=list(m = 1/1000 , b=0), data = part.data,
                 parameters = list( m ~ Month, b ~ Month))

anova(nfit.0, nfit.mon)[10]


power = NULL
for(i in 1:50){
  print(i)
  
  part.data = all.data[sample(1:nrow(all.data), nrow(all.data)/10),]
  part.data$Limb = factor(part.data$Limb)
  
  nfit.0 = mle2(HPOA.mgl ~ dnorm(mean = m * Discharge.m3s + b, sd = sd),
                start=list(m = 1/1000 , b=1.5, sd=1), data = part.data)
  
  nfit.limb = mle2(HPOA.mgl ~ dnorm(mean = m * Discharge.m3s + b, sd = sd),
                   start=list(m = 1/1000 , b=1.5, sd=1), data = part.data,
                   parameters = list( m ~ Limb, b ~ Limb))
  
  anova(nfit.0, nfit.limb)
  
  
  nfit.mon = mle2(HPOA.mgl ~ dnorm(mean = m * Discharge.m3s + b, sd = sd),
                  start=list(m = 1/1000 , b=1.5, sd=1), data = part.data,
                  parameters = list( m ~ Month, b ~ Month))
  p = predict(nfit.mon)
  
  plot(part.data$Discharge.m3s, part.data$HPOA.mgl, col=rainbow(3)[as.numeric(part.data$Month)])
  points(part.data$Discharge.m3s[as.numeric(part.data$Month)==1], p[as.numeric(part.data$Month)==1])
  points(part.data$Discharge.m3s[as.numeric(part.data$Month)==2], p[as.numeric(part.data$Month)==2])
  points(part.data$Discharge.m3s[as.numeric(part.data$Month)==3], p[as.numeric(part.data$Month)==3])
  
  nfit.mon.limb = mle2(HPOA.mgl ~ dnorm(mean = m * Discharge.m3s + b, sd = sd),
                  start=list(m = 1/1000 , b=1.5, sd=1), data = part.data,
                  parameters = list( m ~ Month + Limb, b ~ Month + Limb))
  anova(nfit.0, nfit.mon, nfit.mon.limb )[15]
 # anova(nfit.0, nfit.limb, nfit.mon.limb )
  
  power = rbind(power, cbind(anova(nfit.0, nfit.limb)[10], anova(nfit.0, nfit.mon)[10], anova(nfit.0, nfit.mon, nfit.mon.limb )[15]))
  
}
mean(power[,1])
mean(power[,2])
mean(power[,3])



# flux
all.data = as.data.frame(tscopy2)
all.data$Discharge.m3s = all.data$Discharge * 0.028316
all.data$Month = cut(.indexmon(tscopy2), breaks=c(-1,5,9,12))
all.data = all.data[!is.na(all.data$Discharge.m3s) & !is.na(all.data$flux.mgs),]
power = NULL
for(i in 1:100){
  print(i)
  part.data = all.data[sample(1:nrow(all.data), nrow(all.data)/10),]
  part.data$Limb = factor(part.data$Limb)
  part.data$flux.mgs = part.data$flux.mgs / 1000  # so that it will converge
  #x1 = part.data$flux.mgs[-nrow(part.data)]
  #x2 = part.data$flux.mgs[-1]
  #summary(lm(x2 ~ x1))
  #coeftest(lm(x2 ~ x1))[8] 
  
  nfit.0 = mle2(flux.mgs ~ dnorm(mean = m * Discharge.m3s + b, sd = sd),
                start=list(m = 1000000/500, b=0, sd=1), data = part.data,
                #method='SANN',
                control = list(maxit=500)
                )
  #print(nfit.0)
  
  nfit.limb = mle2(flux.mgs ~ dnorm(mean = m * Discharge.m3s + b, sd = sd),
                   start=list(m = 1000000/500, b=0, sd=1), data = part.data,
                   parameters = list( m ~ Limb, b ~ Limb),
            #       method='SANN',
                   control = list(maxit=500)
                   )
  
  anova(nfit.0, nfit.limb)
  
  if(0){
  plot(part.data[part.data$Limb==1,]$Discharge.m3s, part.data[part.data$Limb==1,]$flux.mgs, col='red',
       ylim=c(0,8000000))
  points(part.data[part.data$Limb==0,]$Discharge.m3s, part.data[part.data$Limb==0,]$flux.mgs, col='purple')
  points(part.data[part.data$Limb==-1,]$Discharge.m3s, part.data[part.data$Limb==-1,]$flux.mgs, col='blue')

  
  plot(part.data$Discharge.m3s, part.data$flux.mgs, 
       col=c('blue', 'purple', 'red')[as.numeric(part.data$Limb)],
       main = anova(nfit.0, nfit.limb)[10])
  lines(part.data$Discharge.m3s, predict(nfit.0, newdata=part.data), typ='l')
  lines(part.data$Discharge.m3s[part.data$Limb==1], 
        predict(nfit.limb, newdata=part.data[part.data$Limb==1,]),
        col='red')
  lines(part.data$Discharge.m3s[part.data$Limb==0], 
        predict(nfit.limb, newdata=part.data[part.data$Limb==0,]),
        col='purple')
  lines(part.data$Discharge.m3s[part.data$Limb==-1], 
        predict(nfit.limb, newdata=part.data[part.data$Limb==-1,]),
        col='blue')
#  lines(part.data$Discharge, #       predict(nfit.limb, newdata=part.data), #        col='blue')
  }
  
  nfit.mon = mle2(flux.mgs ~ dnorm(mean = m * Discharge.m3s + b, sd = sd),
                  start=list(m = 1000000/500  , b=0, sd=1), data = part.data,
                  parameters = list( m ~ Month, b ~ Month),
                 # method='Nelder-Mead',                  
                  control = list(maxit=500))
  
  
  nfit.mon.limb = mle2(flux.mgs ~ dnorm(mean = m * Discharge.m3s + b, sd = sd),
                  start=list(m = 1000000/500  , b=0, sd=1), data = part.data,
                  parameters = list( m ~ Month + Limb, b ~ Month + Limb),
                #  method='Nelder-Mead',                  
                  control = list(maxit=500))
  
  
  power = rbind(power, cbind(anova(nfit.0, nfit.limb)[10],
                             anova(nfit.0, nfit.mon)[10],  
                             anova(nfit.0, nfit.mon, nfit.mon.limb )[15])
  )
  
}
mean(power[,1])
mean(power[,2])
mean(power[,3])

# limb
data.rising = all.data[all.data$Limb == 1,]
data.base = all.data[all.data$Limb == 0,]
data.falling = all.data[all.data$Limb == -1,]

plot(data.rising$Discharge.m3s, data.rising$flux.mgs)
points(data.falling$Discharge.m3s, data.falling$flux.mgs, col='coral2')
points(data.base$Discharge.m3s, data.base$flux.mgs, col='aquamarine3')
