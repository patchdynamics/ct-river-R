library(grDevices)

# make separate graphs of all the freshets

par(mfrow=c(1,5))
times2 = index(tshval[tshval$Temperature >= 280.15 & tshval$Temperature < 290.15])
for (year in 2011:2015){
  ts2 = ts[times2]
  ts2 = ts2[paste0(as.character(year),'-01-01/', as.character(year),'-12-31')]
  
  ylimit = c(1,3.5)  # 3.5
  xlimit = c(0,100000) # 100000
  discharge = na.approx(ts2, na.rm=FALSE)
  discharge = as.data.frame(discharge)$Discharge
  
  colors = colorRampPalette(c("black", rainbow(5)[year-2010]))( length(as.vector(ts2$HPOA.mgl) ) )
  
  plot(discharge/1000, as.data.frame(ts2)$HPOA.mgl, ylim=ylimit,
       col=colors, 
       xlim=xlimit/1000,
       xlab='Discharge / 1000  cfs',
       ylab='[HPOA] ppm',
       #main='Highest Avg Surface Temp   280K < T < 290K',
       #sub='Freshets Seem to Occur Exclusively in this Range'
  )
  
  legend( x="topright", 
          #legend=month.abb[min(.indexmon(ts2)):max(.indexmon(ts2))],
          #col=rainbow(length(unique(.indexmon(ts2)))), 
          legend = as.character(unique(.indexyear(ts2)) + 1900),
          col= rainbow(5)[year-2010], 
          bty = "n",
          pch=1,
          inset=0.05)   
}







# let's look into the stats here
ts2 = ts[times2]
# actually let's not, it's hysteresis response, which 
# is more interesting that some grab bag regression
# claims


