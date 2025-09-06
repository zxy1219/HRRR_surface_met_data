library(terra)
library(data.table)
library(fipio)
library(lubridate)
library(tidyverse)
library(foreign)
library(countytimezones)
library(pracma)

year <- 2019

site <- read.dbf("/home/xzhang/gfs/GIS/NO2/NO2_2019_site_HRRR_10km.dbf", as.is = FALSE)

#Get FIPS code of NO2 sites
site$state_fips <- substr(site$site_ID,1,2)

#Exclude none-conus states
site <- subset(site,!site$state_fips %in% c("72","15","02"))

site$county_fips <- substr(site$site_ID,4,6)
site$fips <- paste0(site$state_fips,site$county_fips)
site$fips <- as.numeric(site$fips)

unique_id <- unique(site$Grid_ID)

HRRR_coord_TimeZone_r <- site %>% group_by(Grid_ID)  %>% summarise(fips = first(fips))

#Assign row and col ID to each point 
HRRR_coord_TimeZone_r$row <- floor(HRRR_coord_TimeZone_r$Grid_ID/1799)+1

HRRR_coord_TimeZone_r$col <- floor(HRRR_coord_TimeZone_r$Grid_ID %% 1799)


#HRRR variables to read
HRRR_var <- c("SFC=Ground or water surface; Visibility [m]", #1
              "SFC=Ground or water surface; Wind speed (gust) [m/s]", #2
              "MSL=Mean sea level; MSLP (MAPS System Reduction) [Pa]", #3
              "SFC=Ground or water surface; Pressure [Pa]",#4
              "SFC=Ground or water surface; Geopotential height [gpm]",#5
              "SFC=Ground or water surface; Temperature [C]", #6
              "2[m] HTGL=Specified height level above ground; Temperature [C]",#7
              "2[m] HTGL=Specified height level above ground; Dew point temperature [C]", #8
              "2[m] HTGL=Specified height level above ground; Relative humidity [%]" , #9
              "10[m] HTGL=Specified height level above ground; u-component of wind [m/s]",#10
              "10[m] HTGL=Specified height level above ground; v-component of wind [m/s]",#11
              "10[m] HTGL=Specified height level above ground; Wind speed [m/s]", #12
              "SFC=Ground or water surface; Surface roughness [m]",#13
              "SFC=Ground or water surface; Frictional velocity [m/s]", #14
              "SFC=Ground or water surface; Sensible heat net flux [W/(m^2)]", #15
              "SFC=Ground or water surface; Latent heat net flux [W/(m^2)]" , #16
              "SFC=Ground or water surface; Ground heat flux [W/(m^2)]", #17
              "EATM=Entire Atmosphere; Total cloud cover [%]", #18
              "SFC=Ground or water surface; Visible Beam Downward Solar Flux [W/(m^2)]", #19
              "SFC=Ground or water surface; Visible Diffuse Downward Solar Flux [W/(m^2)]",  #20
              "SFC=Ground or water surface; Planetary boundary layer height [m]", #21
              "EHLT=Equilibrium level; Geopotential height [gpm]",#22
              "SFC=Ground or water surface; Downward short-wave radiation flux [W/(m^2)]",#23
              "SFC=Ground or water surface; Downward long-wave radiation flux [W/(m^2)]",#24
              "SFC=Ground or water surface; Upward short-wave radiation flux [W/(m^2)]",#25
              "SFC=Ground or water surface; Upward long-wave radiation flux [W/(m^2)]"#26
              )

HRRR_varname <- c( "SFC_visibility",#1
              "SFC_wind_gust",#2
              "MSL",#3
              "SFC_pressure",#4
              "SFC_geoheight", #5
              "SFC_tempC", #6
              "HTGL2_tempC", #7
              "HTGL2_dew_tempC", #8
              "HTGL2_humidity",#9
              "HTGL10_uwind",#10
              "HTGL10_vwind",#11
              "HTGL10_wind_speed", #12
              "SFC_roughness", #13
              "SFC_velocity", #14
              "SFC_sensible_heat", #15
              "SFC_latent_heat", #16
              "SFC_ground_heat", #17
              "EATM_cloud_cover", #18
              "SFC_beam_solar", #19
              "SFC_diffuse_solar", #20
              "SFC_PBLH", #21
              "EHLT_geoheight", #22
              "Down_short_r", #23
              "Down_long_r",#24
              "Up_short_r", #25
              "Up_long_r"#26
)

# Variable needed in F01 file
HRRR01_var <- c("SFC=Ground or water surface; 01 hr Total precipitation [kg/(m^2)]") #1

HRRR01_varname <- c("HR01_total_preci")#1


# Read hourly HRRR data files

#F00
file_HRRR <- list.files(paste0("/home/xzhang/gfs/HRRR/", year, "/f00/"), 
                        all.files = FALSE, full.names = T, pattern = ".grib2")

file_name <- list.files(paste0("/home/xzhang/gfs/HRRR/", year, "/f00/"), 
                        all.files = FALSE, full.names = F, pattern = ".grib2")

#F01
file01_HRRR <- list.files(paste0("/home/xzhang/gfs/HRRR/", year, "/f01/"), 
                        all.files = FALSE, full.names = T, pattern = ".grib2")

file01_name <- list.files(paste0("/home/xzhang/gfs/HRRR/", year, "/f01/"), 
                        all.files = FALSE, full.names = F, pattern = ".grib2")


# Process each HRRR data file
for (f in 1:8760) { #8760 is the number of hours (files) in a year with 365 days

  # Extract date and time components
  year <- substr(file_name[f], 6, 9)
  month <- substr(file_name[f], 10, 11)
  day <- substr(file_name[f], 12, 13)
  hour <- substr(file_name[f], 16, 17)
  
  datetime_UTC <- make_datetime(year = as.numeric(year), month =as.numeric(month), day = as.numeric(day),hour=as.numeric(hour),min=0, sec = 0)
  
  # Load HRRR raster data, F00
  hrrr_dat <- rast(file_HRRR[f])
  hrrr_dat2 <- hrrr_dat[[HRRR_var]]
  
  # Load HRRR raster data, F01 for precipitation
  hrrr01_dat <- rast(file01_HRRR[f])
  hrrr01_dat2 <- hrrr01_dat[[HRRR01_var]]
  
  output_table <- copy(HRRR_coord_TimeZone_r)

  # Add metadata to the table
  output_table[, "Datetime_UTC"] = datetime_UTC
  result <- calc_local_time(output_table$Datetime_UTC, fips =output_table$fips)
  output_table[, "Datetime_local"] = result[1]
  output_table[, "Date_local"] = result[2]
  

  #Create a index to extract data.
  layer_name <- names(hrrr_dat2)[1]
  temp_raster <- hrrr_dat2[[layer_name]]
  cell_index <- cellFromRowCol(temp_raster, output_table$row,output_table$col)
  
  
  #Read HRRR variables you area interested in F00
  for (i in 1: length(names(hrrr_dat2))) {
    layer_name <- names(hrrr_dat2)[i]
    temp_raster <- hrrr_dat2[[layer_name]]
    output_table[, paste0(HRRR_varname[i])] = terra::extract(temp_raster, cell_index)
    
  }
  
  layer01_name <- names(hrrr01_dat2)[1] #hrrr01_dat2 has only one variable
  temp_raster2 <- hrrr01_dat2[[layer01_name]]
  output_table[, paste0(HRRR01_varname)] = terra::extract(temp_raster2, cell_index)
  
  write.csv(output_table,paste0("/home/xzhang/gfs/outputs/NO2_sites/Step1/hrrr.no2sites.",year,month,day,"t",hour,".csv"),row.names = F)
}
  
  
