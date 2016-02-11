active.1 = ts$X.01
active.1[active.1 == 0] = NA
active.1.locf = na.locf(active.1)
names(active.1.locf) = c('RecentlyActive')

index(active.1.locf) = index(active.1.locf) - 2*60*60*24

tsa = merge(ts, active.1.locf)
head(tsa)

tshval = yearly.hval(tstemp, 1)
tslval = yearly.lval(tstemp, 1)


#summer
times2 = index(tshval[tshval$Temperature >= 295.15 & tslval$Temperature >= 295.15])
ts2 = tsa[times2]
ts2 = tsa[.indexmon(ts) >= 7 & .indexmon(ts) <= 9]

ylimit = c(1,3.5)  # 3.5
discharge = na.approx(ts2$Discharge, na.rm=FALSE)
discharge = as.data.frame(discharge)$Discharge
xlimit = c(min(discharge[!is.na(discharge)]),100000) # 100000

# set up colors
rbPal <- colorRampPalette(c('red','blue'))
dataColors <- rbPal(10)[as.numeric(cut(ts2$RecentlyActive,breaks = 5))]
dataColors <- rainbow(10)[as.numeric(cut(ts2$RecentlyActive,breaks = 10))]


par(mfrow=c(1,1))
plot(discharge/1000, as.data.frame(ts2)$HPOA.mgl, ylim=ylimit,
     col=dataColors,
     xlim=xlimit/1000, pch=8)
legend( x="bottomright", 
        legend=  as.character(levels(cut(ts2$RecentlyActive,breaks = 10))),
        col=rainbow(10), pch=8 )



plot(discharge/1000, as.data.frame(ts2)$HPOA.mgl, ylim=ylimit,
col=colors(ts2),
xlim=xlimit/1000)
hpoa = as.data.frame(ts2)$HPOA.mgl

formula = hpoa ~ log(discharge)
lm.hpoa = lm(formula, data = data.frame(hpoa = hpoa, discharge = discharge))
lines(discharge[!is.na(hpoa)]/1000, predict(lm.hpoa, data=data.frame(discharge=discharge[!is.na(hpoa)])) )
legend("topleft", bty="n", 
       legend=paste("R2 =",format(summary(lm.hpoa)$adj.r.squared, digits=4)))
