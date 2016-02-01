FARM = read.csv('/Users/matthewxi/Downloads/Sonde_FARM_Provisional.csv')
HUBB = read.csv('/Users/matthewxi/Downloads/Sonde_HUBB_Provisional (1).csv')
NEPA = read.csv('/Users/matthewxi/Downloads/Sonde_NEPA_Provisional.csv')
UNIO = read.csv('/Users/matthewxi/Downloads/Sonde_UNIO_Provisional (1).csv')
THOM = read.csv('/Users/matthewxi/Downloads/Sonde_THOM_Provisional.csv')



THOM_timestamps = strptime(paste(THOM$DATE, THOM$TIME), "%m/%d/%Y %H:%M");
THOM_timestamps = c(THOM_timestamps)
THOM$timestamps = THOM_timestamps
THOM = THOM[order(timestamps),]
par(mfrow=c(2,1))
plot(THOM$timestamps, as.numeric(THOM$CDOM_ugl), type='l' )
plot(thompsonville$dates, thompsonville$val, typ='l')

plot(THOM$CDOM_ug.l[1:6000], typ='l')
plot(THOM$Depth_m[1:6000], typ='l')

FARM = FARM[order(FARM$DATE,FARM$TIME),]
FARM_timestamps = strptime(paste(FARM$DATE,' ', FARM$TIME), "%Y-%m-%d %H:%M:%S");
FARM_timestamps = c(FARM_timestamps)
par(mfrow=c(2,1))
plot(as.numeric(FARM$CDOM_ug.l)~as.numeric(FARM_timestamps),type='l' )
plot(as.numeric(FARM$Depth_m)~as.numeric(FARM_timestamps),type='l' )

plot(FARM$CDOM_ug.l[1:6000], typ='l')
plot(FARM$Depth_m[1:6000], typ='l')

plot(FARM$CDOM_ug.l[15000:20000], typ='l')
plot(FARM$Depth_m[15000:20000], typ='l')



UNIO = UNIO[order(UNIO$DATE,UNIO$TIME),]
UNIO_timestamps = strptime(paste(UNIO$DATE,' ', UNIO$TIME), "%Y-%m-%d %H:%M:%S");
UNIO_timestamps = c(UNIO_timestamps)
plot(as.numeric(UNIO$CDOM_ug.l)~as.numeric(UNIO_timestamps),type='l' )
plot(as.numeric(UNIO$Depth_m)~as.numeric(UNIO_timestamps),type='l' )

plot(UNIO$CDOM_ug.l[4200:4500], typ='l')
plot(UNIO$Depth_m[4200:4500], typ='l')


HUBB = HUBB[order(HUBB$DATE,HUBB$TIME),]
HUBB_timestamps = strptime(paste(HUBB$DATE,' ', HUBB$TIME), "%Y-%m-%d %H:%M:%S");
HUBB_timestamps = c(HUBB_timestamps)
plot(HUBB_timestamps,as.numeric(HUBB$CDOM_ug.l),type='l' )
plot(as.numeric(HUBB$Depth_m)~as.numeric(HUBB_timestamps),type='l' )

plot(HUBB$CDOM_ug.l[1200:1500], typ='l')
plot(HUBB$Depth_m[1200:1500], typ='l')
# 12 hour delay

NEPA = NEPA[order(NEPA$DATE, NEPA$TIME),]
NEPA_timestamps = strptime(paste(NEPA$DATE,' ', NEPA$TIME), "%Y-%m-%d %H:%M:%S");
NEPA_timestamps = c(NEPA_timestamps)
par(mfrow=c(2,1))
plot(as.numeric(NEPA$CDOM_ug.l)~as.numeric(NEPA_timestamps),type='l' )
plot(as.numeric(NEPA$Depth_m)~as.numeric(NEPA_timestamps),type='l' )


plot(NEPA$CDOM_ug.l[1000:4000], typ='l')
plot(NEPA$Depth_m[1000:4000], typ='l')

