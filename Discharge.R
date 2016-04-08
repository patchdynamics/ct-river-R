# importing water data
library('waterData')

sdate = "2001-01-01"
edate = "2016-01-21"

haddam = importDVs("01193050",code="00060", sdate=sdate, edate=edate)
plotParam(haddam)

thompsonville = importDVs("01184000", code="00060", sdate=sdate, edate=edate)
plotParam(thompsonville)

holyoke = importDVs("01172010", code="00060", sdate=sdate, edate=edate)
plotParam(holyoke)

montague = importDVs("01170500", code="00060", sdate=sdate, edate=edate)
plotParam(montague)


northwalpole = importDVs("01154500", code="00060", sdate=sdate, edate=edate)
plotParam(northwalpole)

westlebanon = importDVs("01144500", code="00060", sdate=sdate, edate=edate)
plotParam(westlebanon)

atwells = importDVs("01138500", code="00060", sdate=sdate, edate=edate)
plotParam(atwells)



farmington = importDVs("01189995",code="00060", sdate=sdate, edate=edate)
plotParam(farmington)

westfield = importDVs("01183500",code="00060", sdate=sdate, edate=edate)
plotParam(westfield)

deerfield = importDVs("01170000",code="00060", sdate=sdate, edate=edate)
plotParam(deerfield)

white = importDVs("01144000", code="00060", sdate=sdate, edate=edate)
plotParam(white)



chicoppe = importDVs("01177000", code="00060", sdate=sdate, edate=edate)
plotParam(chicoppe)

millers = importDVs("01166500", code="00060", sdate=sdate, edate=edate)
plotParam(millers)

ashuelot = importDVs("01161000", code="00060", sdate=sdate, edate=edate)
plotParam(ashuelot)

ammonoosuc = importDVs("01137500", code="00060", sdate=sdate, edate=edate)
plotParam(ammonoosuc)




passumpsic = importDVs("01135500", code="00060", sdate=sdate, edate=edate)
plotParam(passumpsic)

# west = importDVs("01155910", code="00060", sdate=sdate, edate=edate)
# data not available for west river in 2014

par(mfrow=c(5,1))
plot(westfield$val ~ westfield$dates, typ="l", ylim=c(0,3000) )
plot(farmington$val ~ farmington$dates, typ="l", ylim=c(0,3000))
plot(millers$val ~ millers$dates, typ="l", ylim=c(0,3000))
plot(chicoppe$val ~ chicoppe$dates, typ="l", ylim=c(0,3000))
plot(haddam$val ~ haddam$dates, typ="l")



par(mfrow=c(3,1))
plot(farmington$val ~ farmington$dates, typ="l", ylim=c(0,3000), col='blue', xlab="Dates", ylab="Discharge")
lines(westfield$val ~ westfield$dates, typ="l", ylim=c(0,3000), col='red' )
lines(deerfield$val ~ deerfield$dates, typ="l", ylim=c(0,3000), col='green' )
lines(white$val ~ white$dates, typ="l", ylim=c(0,3000), col='black')
lines(passumpsic$val ~ passumpsic$dates, typ="l", col="orange")


plot(chicoppe$val ~ chicoppe$dates, typ="l", ylim=c(0,3000), col='blue', xlab="Dates", ylab="Discharge")
lines(millers$val ~ millers$dates, typ="l", ylim=c(0,3000), col='red' )
lines(ashuelot$val ~ ashuelot$dates, typ="l", ylim=c(0,3000), col='green' )
lines(ammonoosuc$val ~ ammonoosuc$dates, typ="l", ylim=c(0,3000), col='black' )


plot(haddam$val ~ haddam$dates, typ="l")
lines(thompsonville$val ~ thompsonville$dates, typ="l", col='red')
lines(holyoke$val ~ holyoke$dates, typ="l", col='blue')
lines(montague$val ~ montague$dates, typ="l", col='green')
lines(northwalpole$val ~ northwalpole$dates, typ="l", col="pink")
lines(westlebanon$val ~ westlebanon$dates, typ="l", col='yellow')
lines(atwells$val ~ atwells$dates, typ="l", col='orange')




total = farmington$val + westfield$val + deerfield$val + white$val + chicoppe$val + millers$val + ashuelot$val + ammonoosuc$val + passumpsic$val 

plot(haddam$val ~ haddam$dates, typ="l")
lines(thompsonville$val ~ thompsonville$dates, typ="l", col='red')
lines(holyoke$val ~ holyoke$dates, typ="l", col='red')
lines(total ~ haddam$dates, col='orange')

# sonification projects
library(dataRetrieval)
discharges = readNWISuv('01193050', '00060', Sys.Date())
max = discharges[]


