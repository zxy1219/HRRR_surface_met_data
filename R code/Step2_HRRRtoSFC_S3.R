library(terra)
library(data.table)
library(fipio)
library(lubridate)
library(tidyverse)
library(foreign)
library(countytimezones)
library(pracma)

year <- 2019

#Get all HRRR points nearest to NO2 sites
site <- read.dbf("/home/xzhang/gfs/GIS/NO2/NO2_2019_site_HRRR_10km.dbf", as.is = FALSE)
unique_id <- unique(site$Grid_ID)

site_grid_id <- unique_id[1:6]# To generate meteorologic outputs for six HRRR points. 
#                               You can change the number of outputs based on your preferred computation time (the more outputs, the longer time).

# Scenario 3 (set max or min limits for certain parameters, consider SBL or CBL situation). 
# Major parameters:
# Wind speed: HRRR wind speed at 10m
# Wind direction : calculate from u wind and v wind
# Surface friction velocity (u): HRRR raw data
# Sensitive heat flux (H): use HRRR raw data
# Mixing height: use PBL
# Temperature: HRRR temperature (C) at 2m + 273.15. 10m data doesn't have temperature
# Convective velocity scale (w): calculate using H, mixing height (PBL) and temperature in K
# Monin-Obukhov length (L): calculated based on the initial setting (same as SBL) in AERMET manual.
# Bowen ratio: H/latent heat flux. No limits
# Albedo: (Downward shortwave flux - Upword shotwave flux)/Downward shortwave flux. Nightime albedo set to 1


#Read HRRR excel data
file_00 <- list.files("/home/xzhang/gfs/outputs/NO2_sites/Step1/",all.files=FALSE,full.names=T,pattern =".csv")
name_HRRR <- list.files("/home/xzhang/gfs/outputs/NO2_sites/Step1/",all.files=FALSE,full.names=F,pattern =".csv")

hrrr1 <- data.frame()
hrrr2 <- data.frame()
hrrr3 <- data.frame()
hrrr4 <- data.frame()
hrrr5 <- data.frame()
hrrr6 <- data.frame()
for (i in 1:length(file_00)){
  
  dat_HRRR <- read.csv(file_00[i],header=T)

  #Subset to grid nearest to pollen site
  dat_HRRR2 <- subset(dat_HRRR,dat_HRRR$Grid_ID %in% site_grid_id)
  
  dat_HRRR2$Date_local <- as.Date(dat_HRRR2$Date_local,"%Y-%m-%d")
  
  dat_HRRR2$j_day_local <- yday(dat_HRRR2$Date_local)
  
  dat_HRRR2$year_local <- as.numeric(year(dat_HRRR2$Date_local))-2000
  dat_HRRR2$month_local <- as.numeric(month(dat_HRRR2$Date_local))
  dat_HRRR2$day_local <- as.numeric(substr(dat_HRRR2$Date_local,9,10))
  dat_HRRR2$hour_local <- as.numeric(substr(dat_HRRR2$Datetime_local,12,13))
  
  #if H=0, fill H and L using 0.5. 
  dat_HRRR2$SFC_sensible_heat2 <- as.numeric(dat_HRRR2$SFC_sensible_heat)
  
  dat_HRRR2$SFC_sensible_heat2[dat_HRRR2$SFC_sensible_heat==0] <- 0.5
  #H : -64 to 800
  dat_HRRR2$SFC_sensible_heat2[dat_HRRR2$SFC_sensible_heat2 < -64] <- -64
  
  #H_CBL
  dat_HRRR2$N_radiation <- as.numeric(dat_HRRR2$Down_short_r) +  as.numeric(dat_HRRR2$Down_long_r)-as.numeric(dat_HRRR2$Up_short_r) -as.numeric(dat_HRRR2$Up_long_r) 
  
  dat_HRRR2$H_CBL <- dat_HRRR2$N_radiation -as.numeric(dat_HRRR2$SFC_sensible_heat) - as.numeric(dat_HRRR2$SFC_ground_heat)
  
  dat_HRRR2$H_CBL[dat_HRRR2$H_CBL < -64] <- -64
  
  #Calculate additional variables used by AERMET
  dat_HRRR2$TempK <- as.numeric(dat_HRRR2$HTGL2_tempC) + 273.15
  
  dat_HRRR2$u <- abs(as.numeric(dat_HRRR2$SFC_velocity))
  # Surface friction velocity: 0 to 2
  dat_HRRR2$u[dat_HRRR2$u > 2] <- 2
  
  # mixing height: 0 to 4000
  dat_HRRR2$SFC_PBLH <- as.numeric(dat_HRRR2$SFC_PBLH)
  dat_HRRR2$SFC_PBLH[dat_HRRR2$SFC_PBLH > 4000] <- 4000
  
  dat_HRRR2$Wind_dir <- (180+atan2(as.numeric(dat_HRRR2$HTGL10_uwind), as.numeric(dat_HRRR2$HTGL10_vwind))*180/3.14159 )%%360
  dat_HRRR2$CVS <- pracma::nthroot(9.81*dat_HRRR2$SFC_sensible_heat2*dat_HRRR2$SFC_PBLH/(1.293*1004*dat_HRRR2$TempK),3)
  dat_HRRR2$CVS_CBL <- pracma::nthroot(9.81*dat_HRRR2$H_CBL*dat_HRRR2$SFC_PBLH/(1.293*1004*dat_HRRR2$TempK),3)
  
  #Convective velocity scale: 0 to 2
  dat_HRRR2$CVS <- abs(dat_HRRR2$CVS)
  dat_HRRR2$CVS[dat_HRRR2$CVS > 2] <- 2
  dat_HRRR2$CVS_CBL <- abs(dat_HRRR2$CVS_CBL)
  dat_HRRR2$CVS_CBL[dat_HRRR2$CVS_CBL > 2] <- 2
  
  dat_HRRR2$L_day <- 0-((1.293*1004*dat_HRRR2$TempK)*(dat_HRRR2$u^3))/(0.4*9.81*dat_HRRR2$SFC_sensible_heat2)
  dat_HRRR2$L_CBL <- 0-((1.293*1004*dat_HRRR2$TempK)*(dat_HRRR2$u^3))/(0.4*9.81*dat_HRRR2$H_CBL)
  
  #H: -8888 to 8888
  dat_HRRR2$L_day[dat_HRRR2$L_day < -8888] <- -8888
  dat_HRRR2$L_day[dat_HRRR2$L_day > 8888] <- 8888
  dat_HRRR2$L_CBL[dat_HRRR2$L_CBL < -8888] <- -8888
  dat_HRRR2$L_CBL[dat_HRRR2$L_CBL > 8888] <- 8888
  
  #Latent H: -100 to 800
  dat_HRRR2$SFC_latent_heat[dat_HRRR2$SFC_latent_heat < -100] <- -100
  dat_HRRR2$SFC_latent_heat[dat_HRRR2$SFC_latent_heat > 800] <- 800
  dat_HRRR2$Bowen <- abs(as.numeric(dat_HRRR2$SFC_sensible_heat)/as.numeric(dat_HRRR2$SFC_latent_heat))
  
  dat_HRRR2$Albedo <- 1
  dat_HRRR2$Albedo[dat_HRRR2$SFC_beam_solar > 0] <- (dat_HRRR2$Down_short_r[dat_HRRR2$SFC_beam_solar>0] - dat_HRRR2$Up_short_r[dat_HRRR2$SFC_beam_solar>0])/dat_HRRR2$Down_short_r[dat_HRRR2$SFC_beam_solar>0]
  
  dat_HRRR2$Albedo[dat_HRRR2$Albedo < 0] <- 0
  
  #Surface roughness length: 0 to 2
  dat_HRRR2$SFC_roughness <- as.numeric(dat_HRRR2$SFC_roughness)
  dat_HRRR2$SFC_roughness[dat_HRRR2$SFC_roughness >2] <- 2

  #Set CBL and SBL for different variables
  dat_HRRR2$CVS <- dat_HRRR2$CVS
  dat_HRRR2$CVS[dat_HRRR2$SFC_beam_solar>0] <- dat_HRRR2$CVS_CBL[dat_HRRR2$SFC_beam_solar>0]
  
  dat_HRRR2$L <- dat_HRRR2$L_day
  dat_HRRR2$L[dat_HRRR2$SFC_beam_solar>0] <- dat_HRRR2$L_CBL[dat_HRRR2$SFC_beam_solar>0]

  
  #Some variables are not available in HRRR, use the airport data
  #Grid 1
  dat_HRRR3 <- subset(dat_HRRR2, dat_HRRR2$Grid_ID == site_grid_id[1])
  
  temp1 <- data.frame(Year=dat_HRRR3$year_local,#V1
                      Month=dat_HRRR3$month_local,#V2
                      Day=dat_HRRR3$day_local,#V3
                      j_day=dat_HRRR3$j_day_local, #V4
                      hour=dat_HRRR3$hour_local+1, #V5
                      H=as.numeric(dat_HRRR3$SFC_sensible_heat[1]),#V6
                      u=dat_HRRR3$u[1], #V7
                      w=dat_HRRR3$CVS[1], #V8
                      VPTG=-9, #V9
                      Zic=dat_HRRR3$SFC_PBLH[1], #V10
                      Zim=as.numeric(dat_HRRR3$EHLT_geoheight[1]),#V11
                      L=dat_HRRR3$L[1], #V12
                      z0 = dat_HRRR3$SFC_roughness[1],#V13
                      Bowen = dat_HRRR3$Bowen[1],#V14
                      r= dat_HRRR3$Albedo[1],#V16 
                      Ws= as.numeric(dat_HRRR3$HTGL10_wind_speed[1]), #V17
                      Wd= dat_HRRR3$Wind_dir[1], #V18
                      Zref=10, #V19
                      temp=dat_HRRR3$TempK[1], #V20
                      Ztemp=2, #V21
                      ipcode=11, #V22
                      pamt = as.numeric(dat_HRRR3$HR01_total_preci[1]),
                      rh= as.numeric(dat_HRRR3$HTGL2_humidity[1]),
                      pres = as.numeric(dat_HRRR3$SFC_pressure[1])/10,#pres: sea level pressure, millibars*10
                      ccvr = as.numeric(floor(dat_HRRR3$EATM_cloud_cover[1]/10)), 
                      WSADJ = "ADJ"
                      #Grid_ID = site_grid_id[1]
  )
  hrrr1 = rbind(temp1,hrrr1)
  
  #Grid 2
  dat_HRRR3 <- subset(dat_HRRR2, dat_HRRR2$Grid_ID == site_grid_id[2])
  temp2 <- data.frame(Year=dat_HRRR3$year_local,#V1
                      Month=dat_HRRR3$month_local,#V2
                      Day=dat_HRRR3$day_local,#V3
                      j_day=dat_HRRR3$j_day_local, #V4
                      hour=dat_HRRR3$hour_local+1, #V5
                      H=as.numeric(dat_HRRR3$SFC_sensible_heat[1]),#V6
                      u=dat_HRRR3$u[1], #V7
                      w=dat_HRRR3$CVS[1], #V8
                      VPTG=-9, #V9
                      Zic=dat_HRRR3$SFC_PBLH[1], #V10
                      Zim=as.numeric(dat_HRRR3$EHLT_geoheight[1]),#V11
                      L=dat_HRRR3$L[1], #V12
                      z0 = dat_HRRR3$SFC_roughness[1],#V13
                      Bowen = dat_HRRR3$Bowen[1],#V14
                      r= dat_HRRR3$Albedo[1],#V15 
                      Ws= as.numeric(dat_HRRR3$HTGL10_wind_speed[1]), #V16
                      Wd= dat_HRRR3$Wind_dir[1], #V17
                      Zref=10, #V18
                      temp=dat_HRRR3$TempK[1], #V19
                      Ztemp=2, #V20
                      ipcode=11, #V21
                      pamt = as.numeric(dat_HRRR3$HR01_total_preci[1]),
                      rh= as.numeric(dat_HRRR3$HTGL2_humidity[1]),
                      pres = as.numeric(dat_HRRR3$SFC_pressure[1])/10,
                      ccvr = as.numeric(floor(dat_HRRR3$EATM_cloud_cover[1]/10)),
                      WSADJ = "ADJ"                      
                      # Grid_ID = site_grid_id[2]
  )
  hrrr2 = rbind(temp2,hrrr2)
  
  #Grid 3
  dat_HRRR3 <- subset(dat_HRRR2, dat_HRRR2$Grid_ID == site_grid_id[3])
  temp3 <- data.frame(Year=dat_HRRR3$year_local,#V1
                      Month=dat_HRRR3$month_local,#V2
                      Day=dat_HRRR3$day_local,#V3
                      j_day=dat_HRRR3$j_day_local, #V4
                      hour=dat_HRRR3$hour_local+1, #V5
                      H=as.numeric(dat_HRRR3$SFC_sensible_heat[1]),#V6
                      u=dat_HRRR3$u[1], #V7
                      w=dat_HRRR3$CVS[1], #V8
                      VPTG=-9, #V9
                      Zic=dat_HRRR3$SFC_PBLH[1], #V10
                      Zim=as.numeric(dat_HRRR3$EHLT_geoheight[1]),#V11
                      L=dat_HRRR3$L[1], #V12
                      z0 = dat_HRRR3$SFC_roughness[1],#V13
                      Bowen = dat_HRRR3$Bowen[1],#V14
                      r= dat_HRRR3$Albedo[1],#V16 
                      Ws= as.numeric(dat_HRRR3$HTGL10_wind_speed[1]), #V17
                      Wd= dat_HRRR3$Wind_dir[1], #V18
                      Zref=10, #V19
                      temp=dat_HRRR3$TempK[1], #V20
                      Ztemp=2, #V21
                      ipcode=11, #V22
                      pamt = as.numeric(dat_HRRR3$HR01_total_preci[1]),
                      rh= as.numeric(dat_HRRR3$HTGL2_humidity[1]),
                      pres = as.numeric(dat_HRRR3$SFC_pressure[1])/10,
                      ccvr = as.numeric(floor(dat_HRRR3$EATM_cloud_cover[1]/10)),
                      WSADJ = "ADJ"
                      # Grid_ID = site_grid_id[3]
  )
  hrrr3 = rbind(temp3,hrrr3)
  
  
  #Grid 4
  dat_HRRR3 <- subset(dat_HRRR2, dat_HRRR2$Grid_ID == site_grid_id[4])
  temp4 <- data.frame(Year=dat_HRRR3$year_local,#V1
                      Month=dat_HRRR3$month_local,#V2
                      Day=dat_HRRR3$day_local,#V3
                      j_day=dat_HRRR3$j_day_local, #V4
                      hour=dat_HRRR3$hour_local+1, #V5
                      H=as.numeric(dat_HRRR3$SFC_sensible_heat[1]),#V6
                      u=dat_HRRR3$u[1], #V7
                      w=dat_HRRR3$CVS[1], #V8
                      VPTG=-9, #V9
                      Zic=dat_HRRR3$SFC_PBLH[1], #V10
                      Zim=as.numeric(dat_HRRR3$EHLT_geoheight[1]),#V11
                      L=dat_HRRR3$L[1], #V12
                      z0 = dat_HRRR3$SFC_roughness[1],#V13
                      Bowen = dat_HRRR3$Bowen[1],#V14
                      r= dat_HRRR3$Albedo[1],#V15
                      Ws= as.numeric(dat_HRRR3$HTGL10_wind_speed[1]), #V16
                      Wd= dat_HRRR3$Wind_dir[1], #V17
                      Zref=10, #V18
                      temp=dat_HRRR3$TempK[1], #V19
                      Ztemp=2, #V20
                      ipcode=11, #V21
                      pamt = as.numeric(dat_HRRR3$HR01_total_preci[1]),
                      rh= as.numeric(dat_HRRR3$HTGL2_humidity[1]),
                      pres = as.numeric(dat_HRRR3$SFC_pressure[1])/10,
                      ccvr = as.numeric(floor(dat_HRRR3$EATM_cloud_cover[1]/10)), 
                      WSADJ = "ADJ"                      
                      # Grid_ID = site_grid_id[4]
  )
  hrrr4 = rbind(temp4,hrrr4)
  
  #Grid 5
  dat_HRRR3 <- subset(dat_HRRR2, dat_HRRR2$Grid_ID == site_grid_id[5])
  temp5 <- data.frame(Year=dat_HRRR3$year_local,#V1
                      Month=dat_HRRR3$month_local,#V2
                      Day=dat_HRRR3$day_local,#V3
                      j_day=dat_HRRR3$j_day_local, #V4
                      hour=dat_HRRR3$hour_local+1, #V5
                      H=as.numeric(dat_HRRR3$SFC_sensible_heat[1]),#V6
                      u=dat_HRRR3$u[1], #V7
                      w=dat_HRRR3$CVS[1], #V8
                      VPTG=-9, #V9
                      Zic=dat_HRRR3$SFC_PBLH[1], #V10
                      Zim=as.numeric(dat_HRRR3$EHLT_geoheight[1]),#V11
                      L=dat_HRRR3$L[1], #V12
                      z0 = dat_HRRR3$SFC_roughness[1],#V13
                      Bowen = dat_HRRR3$Bowen[1],#V14
                      r= dat_HRRR3$Albedo[1],#V15 
                      Ws= as.numeric(dat_HRRR3$HTGL10_wind_speed[1]), #V16
                      Wd= dat_HRRR3$Wind_dir[1], #V17
                      Zref=10, #V18
                      temp=dat_HRRR3$TempK[1], #V19
                      Ztemp=2, #V20
                      ipcode=11, #V21
                      pamt = as.numeric(dat_HRRR3$HR01_total_preci[1]),
                      rh= as.numeric(dat_HRRR3$HTGL2_humidity[1]),
                      pres = as.numeric(dat_HRRR3$SFC_pressure[1])/10,
                      ccvr = as.numeric(floor(dat_HRRR3$EATM_cloud_cover[1]/10)),
                      WSADJ = "ADJ"                      
                      # Grid_ID = site_grid_id[5]
  )
  hrrr5 = rbind(temp5,hrrr5)
  
  
  #Grid 6
  dat_HRRR3 <- subset(dat_HRRR2, dat_HRRR2$Grid_ID == site_grid_id[6])
  temp6 <- data.frame(Year=dat_HRRR3$year_local,#V1
                      Month=dat_HRRR3$month_local,#V2
                      Day=dat_HRRR3$day_local,#V3
                      j_day=dat_HRRR3$j_day_local, #V4
                      hour=dat_HRRR3$hour_local+1, #V5
                      H=as.numeric(dat_HRRR3$SFC_sensible_heat[1]),#V6
                      u=dat_HRRR3$u[1], #V7
                      w=dat_HRRR3$CVS[1], #V8
                      VPTG=-9, #V9
                      Zic=dat_HRRR3$SFC_PBLH[1], #V10
                      Zim=as.numeric(dat_HRRR3$EHLT_geoheight[1]),#V11
                      L=dat_HRRR3$L[1], #V12
                      z0 = dat_HRRR3$SFC_roughness[1],#V13
                      Bowen = dat_HRRR3$Bowen[1],#V14
                      r= dat_HRRR3$Albedo[1],#V15
                      Ws= as.numeric(dat_HRRR3$HTGL10_wind_speed[1]), #V16
                      Wd= dat_HRRR3$Wind_dir[1], #V17
                      Zref=10, #V18
                      temp=dat_HRRR3$TempK[1], #V19
                      Ztemp=2, #V20
                      ipcode=11, #V21
                      pamt = as.numeric(dat_HRRR3$HR01_total_preci[1]),
                      rh= as.numeric(dat_HRRR3$HTGL2_humidity[1]),
                      pres = as.numeric(dat_HRRR3$SFC_pressure[1])/10,
                      ccvr = as.numeric(floor(dat_HRRR3$EATM_cloud_cover[1]/10)),
                      WSADJ = "ADJ"
                      # Grid_ID = site_grid_id[6]
  )
  hrrr6 = rbind(temp6,hrrr6)
  
  print(i)
  
}

#Adjust Bowen ratio and L when H is zero
hrrr1$Bowen[hrrr1$Bowen %in% c(-Inf,Inf)] <- median(hrrr1$Bowen[is.na(hrrr1$Bowen)==F])
hrrr2$Bowen[hrrr2$Bowen %in% c(-Inf,Inf)] <- median(hrrr2$Bowen[is.na(hrrr2$Bowen)==F])
hrrr3$Bowen[hrrr3$Bowen %in% c(-Inf,Inf)] <- median(hrrr3$Bowen[is.na(hrrr3$Bowen)==F])
hrrr4$Bowen[hrrr4$Bowen %in% c(-Inf,Inf)] <- median(hrrr4$Bowen[is.na(hrrr4$Bowen)==F])
hrrr5$Bowen[hrrr5$Bowen %in% c(-Inf,Inf)] <- median(hrrr5$Bowen[is.na(hrrr5$Bowen)==F])
hrrr6$Bowen[hrrr6$Bowen %in% c(-Inf,Inf)] <- median(hrrr6$Bowen[is.na(hrrr6$Bowen)==F])

hrrr1$Bowen[is.na(hrrr1$Bowen) ==T] <- median(hrrr1$Bowen[is.na(hrrr1$Bowen)==F])
hrrr2$Bowen[is.na(hrrr2$Bowen) ==T] <- median(hrrr2$Bowen[is.na(hrrr2$Bowen)==F])
hrrr3$Bowen[is.na(hrrr3$Bowen) ==T] <- median(hrrr3$Bowen[is.na(hrrr3$Bowen)==F])
hrrr4$Bowen[is.na(hrrr4$Bowen) ==T] <- median(hrrr4$Bowen[is.na(hrrr4$Bowen)==F])
hrrr5$Bowen[is.na(hrrr5$Bowen) ==T] <- median(hrrr5$Bowen[is.na(hrrr5$Bowen)==F])
hrrr6$Bowen[is.na(hrrr6$Bowen) ==T] <- median(hrrr6$Bowen[is.na(hrrr6$Bowen)==F])

hrrr1$Bowen[hrrr1$Bowen < -10] <- -10
hrrr1$Bowen[hrrr1$Bowen > 10] <- 10
hrrr2$Bowen[hrrr1$Bowen < -10] <- -10
hrrr2$Bowen[hrrr1$Bowen > 10] <- 10
hrrr3$Bowen[hrrr1$Bowen < -10] <- -10
hrrr3$Bowen[hrrr1$Bowen > 10] <- 10
hrrr4$Bowen[hrrr1$Bowen < -10] <- -10
hrrr4$Bowen[hrrr1$Bowen > 10] <- 10
hrrr5$Bowen[hrrr1$Bowen < -10] <- -10
hrrr5$Bowen[hrrr1$Bowen > 10] <- 10
hrrr6$Bowen[hrrr1$Bowen < -10] <- -10
hrrr6$Bowen[hrrr1$Bowen > 10] <- 10


write.csv(hrrr1,paste0("/home/xzhang/gfs/outputs/NO2_sites/Step2_v3/HRRRv3_2019_ID",site_grid_id[1],".csv"),row.names = F)
write.csv(hrrr2,paste0("/home/xzhang/gfs/outputs/NO2_sites/Step2_v3/HRRRv3_2019_ID",site_grid_id[2],".csv"),row.names = F)
write.csv(hrrr3,paste0("/home/xzhang/gfs/outputs/NO2_sites/Step2_v3/HRRRv3_2019_ID",site_grid_id[3],".csv"),row.names = F)
write.csv(hrrr4,paste0("/home/xzhang/gfs/outputs/NO2_sites/Step2_v3/HRRRv3_2019_ID",site_grid_id[4],".csv"),row.names = F)
write.csv(hrrr5,paste0("/home/xzhang/gfs/outputs/NO2_sites/Step2_v3/HRRRv3_2019_ID",site_grid_id[5],".csv"),row.names = F)
write.csv(hrrr6,paste0("/home/xzhang/gfs/outputs/NO2_sites/Step2_v3/HRRRv3_2019_ID",site_grid_id[6],".csv"),row.names = F)


