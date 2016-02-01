library("foreign")

# gage height
# 00065

sdate = "2014-06-01"
edate = "2014-09-01"

# thomp 01184000
# haddam 01193050
stage = importDVs("01184000",code="00065", sdate=sdate, edate=edate)
plotParam(stage)

Name = rep("Thompsonville", length(TSValue))
TSValue = stage$val * 0.3048 # convert to meters
TSValue = TSValue + 11.471 # convert to elevation datum HACK
TSTime = format(stage$dates, '%Y-%m-%d')
VarID = rep.int(29, length(TSValue))
FeatureID = rep.int(1184000, length(TSValue))

timeseries = data.frame(TSValue, TSTime, VarID, FeatureID)

write.dbf( timeseries, 'thompsonville_gage.dbf')