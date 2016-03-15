library(Metrics)

times2 = index(tshval[tshval$Temperature >= 295.15 & tslval$Temperature >= 295.15])
ts2 = ts[times2]
#ts2 = ts[.indexmon(ts) >= 7 & .indexmon(ts) <= 9]
#ts2 = ts2[.indexyear(ts2) != 113]

ylimit = c(1,3.5)  # 3.5
discharge = na.approx(ts2$Discharge, na.rm=FALSE)
discharge = as.data.frame(discharge)$Discharge
xlimit = c(min(discharge[!is.na(discharge)]),120000) # 100000

#discharge[discharge < 0] = 1

tss = ts2

ts2 = tss[.indexyear(tss) == 113]
df = as.data.frame(ts2)
df = df[!is.na(df$Discharge),]

colors = colorRampPalette(c("black", rainbow(5)[year-2010]))( length(as.vector(ts2$HPOA.mgl) ) )

par(mfrow=c(2,1))
plot(df$Discharge/1000, df$HPOA.mgl, ylim=ylimit,
     #col=rainbow(length(unique(.indexmon(ts2)))),
     #col=rainbow(5)[.indexyear(ts2)-110],
     col = colors,
     xlim=xlimit/1000, #log="x",
     pch=.indexyear(ts2)-110,
)

legend( x="bottomright", 
        #legend=month.abb[min(.indexmon(ts2)):max(.indexmon(ts2))-1],
        #col=rainbow(length(unique(.indexmon(ts2)))), lwd=1, lty=c(1,2), 
        legend=2011:2015,
        col=rainbow(5),
        pch=1:5)

#df1 = df
#df = df1[1:100,]


year = 2015
ts2 = tss[.indexyear(tss) == year - 1900]
colors = colorRampPalette(c("black", rainbow(5)[year-2010]))( length(as.vector(ts2$HPOA.mgl) ) )

df = as.data.frame(ts2)
df = df[!is.na(df$Discharge),]
par(mfrow=c(1,1))
plot(df$Discharge/1000, df$HPOA.mgl, ylim=ylimit,
     #col=rainbow(length(unique(.indexmon(ts2)))),
     #col=rainbow(5)[.indexyear(ts2)-110],
     col = colors,
     xlim=xlimit/1000, #log="x",
     pch=.indexyear(ts2)-110,
     typ='l',
     main = year
)
s = 1:(nrow(df)-1)
arrows(df[,"Discharge"][s]/1000, df[,"HPOA.mgl"][s], 
       df[,"Discharge"][s+1]/1000, df[,"HPOA.mgl"][s+1], 
       length=0.1,
       #code=1,
       lwd=2,
       col = colors
)



par(mfrow=c(2,3))
for( year in 2011:2015) {
  ts2 = tss[.indexyear(tss) == year - 1900]
  colors = colorRampPalette(c("black", rainbow(5)[year-2010]))( length(as.vector(ts2$HPOA.mgl) ) )
  
  df = as.data.frame(ts2)
  df = df[!is.na(df$Discharge),]
  plot(df$Discharge/1000, df$HPOA.mgl, ylim=ylimit,
       #col=rainbow(length(unique(.indexmon(ts2)))),
       #col=rainbow(5)[.indexyear(ts2)-110],
       col = colors,
       xlim=xlimit/1000, #log="x",
       pch=.indexyear(ts2)-110,
       typ='l',
       main = year
  )
  s = 1:(nrow(df)-1)
  arrows(df[,"Discharge"][s]/1000, df[,"HPOA.mgl"][s], 
         df[,"Discharge"][s+1]/1000, df[,"HPOA.mgl"][s+1], 
         length=0.1,
         #code=1,
         lwd=2,
         col = colors
  )
}




# take alook at 2013
par(mfrow=c(2,2))

times2 = index(tshval[tshval$Temperature >= 290.15 & tslval$Temperature >= 295.15])
ts2 = ts[times2]
ts2 = ts2[.indexmon(ts2) < 8]
ts2 = ts2[.indexyear(ts2) == 113]
tss = ts2

for( year in 2011:2015) {
  ts2 = tss[.indexyear(tss) == year - 1900]
  colors = colorRampPalette(c("black", rainbow(5)[year-2010]))( nrow(ts2) )
  
  df = as.data.frame(ts2)
  if(nrow(df) == 0){
    next
  }
  df = df[!is.na(df$Discharge),]
  plot(df$Discharge/1000, df$HPOA.mgl, ylim=ylimit,
       #col=rainbow(length(unique(.indexmon(ts2)))),
       #col=rainbow(5)[.indexyear(ts2)-110],
       col = colors,
       xlim=xlimit/1000, #log="x",
       pch=.indexyear(ts2)-110,
       typ='l',
       main = year
  )
  s = 1:(nrow(df)-1)
  arrows(df[,"Discharge"][s]/1000, df[,"HPOA.mgl"][s], 
         df[,"Discharge"][s+1]/1000, df[,"HPOA.mgl"][s+1], 
         length=0.1,
         #code=1,
         lwd=2,
         col = colors
  )
}


# the stats of it all
times2 = index(tshval[tshval$Temperature >= 295.15 & tslval$Temperature >= 295.15])
ts2 = ts[times2]

df = as.data.frame(ts2)
df = df[!is.na(df$HPOA.mgl) & !is.na(df$Discharge),]
training = df[sort(sample(nrow(df), 20)),]

# test for autocorrellation
hpoa.vector = training$HPOA.mgl
plot(hpoa.vector[1:length(hpoa.vector)-1], hpoa.vector[2:length(hpoa.vector)])

mod2 = lm(hpoa.vector[-length(hpoa.vector)] ~ hpoa.vector[-1]) 
#summary(mod2)

lm.hpoa = lm(HPOA.mgl ~ Discharge, data = training)
#summary(lm.hpoa)

mod3 = lm(lm.hpoa$residuals[-1] ~  lm.hpoa$residuals[-length(lm.hpoa$residuals)]) 
out = summary(mod3)
plot(lm.hpoa$residuals[-1], lm.hpoa$residuals[-length(lm.hpoa$residuals)], main=out$r.squared)
lines(lm.hpoa$residuals[-length(lm.hpoa$residuals)], predict(mod3))


# best so far
#doit = function() {
times2 = index(tshval[tshval$Temperature >= 295.15 & tslval$Temperature >= 295.15])

ts2 = ts[times2]

ts2 = ts.full[times2]

ts.detrend = NULL
year.min.d = NULL
year.max.d = NULL
for(year in 111:115) {
  ts.year = ts2[.indexyear(ts2) == year]
  mean.year = mean(ts.year$HPOA.mgl[!is.na(ts.year$HPOA.mgl)])
  year.means = rbind(year.means, c(year, mean.year))

  year.min.d = rbind(year.min.d, min(ts.year$Discharge[!is.na(ts.year$Discharge)]))
  year.max.d = rbind(year.max.d, max(ts.year$Discharge[!is.na(ts.year$Discharge)]))
  
}
min.shared.discharge = max(year.min.d)
max.shared.discharge = min(year.max.d)

year.means = NULL
for(year in 111:115) {
  ts.year = ts2[.indexyear(ts2) == year]
  ts.year = ts.year[ts.year$Discharge > min.shared.discharge & ts.year$Discharge < max.shared.discharge]
  ts.year = ts.year$HPOA.mgl[!is.na(ts.year$HPOA.mgl)]
  mean.year = mean(ts.year$HPOA.mgl)
  year.means = rbind(year.means, c(year, mean.year))
  
  # now sutract
  ts.year = ts2[.indexyear(ts2) == year]
  ts.year = ts.year$HPOA.mgl - mean.year
  ts.detrend = rbind(ts.detrend, ts.year)
}
ts2 = merge(ts.detrend, ts2$Discharge, ts2$Limb2)

df = as.data.frame(ts2)
df = df[!is.na(df$HPOA.mgl) & !is.na(df$Discharge),]

#df = df[df$Discharge < 40000,]

plot(df$Discharge/1000, df$HPOA.mgl)

#plot(df$HPOA.mgl)
plot(ts2$HPOA.mgl)
acf(as.vector(na.locf(ts2$HPOA.mgl)), lag.max = 25)


training.type = 2
if(training.type == 1) {
  training = df[sort(sample(nrow(df), 20)),]
} else {
  spacing = 10
  n = nrow(df) / spacing
  training = df[(1:n-1) * spacing + sample(1:spacing,1),]
}

# test for autocorrellation
hpoa.vector = training$HPOA.mgl
plot(hpoa.vector[1:length(hpoa.vector)-1], hpoa.vector[2:length(hpoa.vector)])

mod2 = lm(hpoa.vector[-length(hpoa.vector)] ~ hpoa.vector[-1]) 
summary(mod2)


lm.hpoa = lm(HPOA.mgl ~ log(Discharge ), data = training)

# nls not really working too well
# nlm.hpoa = nls(HPOA.mgl~ p0 + p1 * log(Discharge + p2), data=training, start=c(p0=-3.171, p1=.338, p2=0) )
x`
summary(lm.hpoa)$adj.r.squared
predict.data = data.frame(Discharge = sort(df$Discharge[df$Discharge > 0]))
prd = predict(lm.hpoa, predict.data,interval = c("prediction"))
rmse = rmse(df$HPOA.mgl, as.vector(prd[,1]))

par(mfrow=c(1,1))

plot(df$Discharge/1000, df$HPOA.mgl, 
     log = c('x'),
     main = paste('RMSE = ', rmse),  sub=paste('Training R squared: ',summary(lm.hpoa)$r.squared),
     xlab = 'Discharge (cfs) / 1000',
     ylab = '[HPOA] ppm'
     )
lines(predict.data$Discharge/1000, prd[,1], col='purple', lwd=3)
lines(predict.data$Discharge/1000, prd[,2], col='orange', lwd=3)
lines(predict.data$Discharge/1000, prd[,3], col='orange', lwd=3)

}

plot(lm.hpoa$residuals, typ='l')

#plot(training$Discharge[order(training$Discharge)]/1000, lm.hpoa$residuals[order(training$Discharge)])
dforder = df[order(df$Discharge),]
dforder = dforder[dforder$Discharge > 0,]
prd = predict(lm.hpoa, dforder,interval = c("confidence"))
plot(dforder$Discharge/1000, dforder$HPOA.mgl - prd[,1])


mod3 = lm(lm.hpoa$residuals[-1] ~  lm.hpoa$residuals[-length(lm.hpoa$residuals)]) 
out = summary(mod3)
out$adj.r.squared

plot(lm.hpoa$residuals[-1], lm.hpoa$residuals[-length(lm.hpoa$residuals)], main=out$r.squared)
lines(lm.hpoa$residuals[-length(lm.hpoa$residuals)], predict(mod3))



# take look at the diff(log)
# nothing going on here, in fact it's nearly gaussian
diff.log.discharge = diff(log(df$Discharge))
diff.log.hpoa = diff(log(df$HPOA.mgl))
plot(diff.log.discharge, diff.log.hpoa)


# limbs
discharge = ts$Discharge
discharge.diff = diff.xts(discharge,3)
#discharge.diff[abs(discharge.diff) < 2000] = 0  # this needs to come from hysep

par(mfrow=c(2,1))
plot(discharge)
plot(discharge.diff)

discharge.diff[discharge.diff < 0] = -1
discharge.diff[discharge.diff > 0] = 1
discharge.diff[discharge.diff == 0] = 0
names(discharge.diff) = 'Limb'
plot(discharge.diff)

names(discharge.diff) = c('Limb2')
ts.full = merge(ts, discharge.diff)

par(mfrow=c(1,1))

colormap = c(1,2,3)
dfplot =df
#dfplot = df[df$Limb == 1 | df$Limb==-1,]
#dfplot = df[df$Limb == -1, ]
colormap = colormap[as.factor(dfplot$Limb)]
plot(dfplot$Discharge/1000, dfplot$HPOA.mgl, 
     log = c('x'),
     main = paste('RMSE = ', rmse),  sub=paste('Training R squared: ',summary(lm.hpoa)$r.squared),
     col=rainbow(3)[colormap]      
)
lines(predict.data$Discharge/1000, prd[,1], col='purple', lwd=3)
lines(predict.data$Discharge/1000, prd[,2], col='orange', lwd=3)
lines(predict.data$Discharge/1000, prd[,3], col='orange', lwd=3)

legend( x="topleft", 
        legend = c('Falling', 'Baseflow', 'Rising'),
        col=rainbow(3), 
        pch=1 )

