times2 = index(tshval[tslval$Temperature >= 285.15 & tslval$Temperature <= 295.15])

#times2 = index(tshval[tshval$Temperature >= 295.15 & tslval$Temperature <= 295.15])

ts2 = ts[times2]
ts2 = ts2[.indexyear(ts2)!=111]

ylimit = c(1,3.5)  # 3.5
xlimit = c(0,100000) # 100000
discharge = na.approx(ts2$Discharge, na.rm=FALSE)
discharge = as.data.frame(discharge)$Discharge
plot(discharge/1000, as.data.frame(ts2)$HPOA.mgl, ylim=ylimit,
     col=rainbow(5)[.indexyear(ts2)-110],
     xlim=xlimit/1000,
     main='lval > 285, lval < 295',
     pch=.indexyear(ts2)-110)
legend( x="topright", 
        legend = 2011:2015,
        col=rainbow(5),
        lwd=1, lty=c(1,2), 
        pch=1:5  )


df = as.data.frame(ts2)
df = df[!is.na(df$HPOA.mgl) & !is.na(df$Discharge),]


acf(as.vector(na.locf(ts2$HPOA.mgl)), lag.max = 25)


training.type = 2
if(training.type == 1) {
  training = df[sort(sample(nrow(df), 20)),]
} else {
  spacing = 10
  n = nrow(df) / spacing
  training = df[(1:n-1) * spacing + sample(1:spacing,1),]
}

acf(training$HPOA.mgl)

lm.hpoa = lm(HPOA.mgl ~ log(Discharge ), data = training)
summary(lm.hpoa)

hpoa.vector = training$HPOA.mgl
plot(hpoa.vector[1:length(hpoa.vector)-1], hpoa.vector[2:length(hpoa.vector)])
mod2 = lm(hpoa.vector[-length(hpoa.vector)] ~ hpoa.vector[-1]) 
summary(mod2)




