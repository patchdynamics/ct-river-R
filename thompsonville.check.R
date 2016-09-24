library(waterData)

sdate = "2010-01-01"
edate = "2016-04-01"
thomp.discharge = importDVs("01184000", code="00060", sdate=sdate, edate=edate)
thomp.discharge = xts(thomp.discharge$val, order.by=thomp.discharge$dates)


thomp.values = read.csv('../data/Thompsonville_Table1_data.csv')
dates = as.Date(as.character(thomp.values$TIMESTAMP))
thomp.cdom = aggregate(thomp.values$CDOM, list(dates), mean)
names(thomp.cdom) = c('Date', 'CDOM')

#as.Date(as.character(thomp.values$TIMESTAMP))
thomp.cdom = xts(thomp.cdom$CDOM, order.by=thomp.cdom$Date)


thomp = merge(thomp.cdom, thomp.discharge)
thomp = thomp[!is.na(thomp$thomp.cdom)]

thomp.df = as.data.frame(thomp[90:130])
thomp.df = as.data.frame(thomp[130:250])
plot(thomp.df$thomp.discharge, thomp.df$thomp.cdom, type='l')


