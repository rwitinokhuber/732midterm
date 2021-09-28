## ---------------------------
##
## Script name: Pull NOAA meteorological data
##
## Purpose of script: Pull NOAA RH data using `rnoaa` package for the five most affected
##                    counties by COVID-19.
##                    
## Authors: Grace Kuiper, Becky Witinok-Huber, Bonnie Young
##
## Date Created: 09/26/2021
## Newest Version: 09/27/2021
##
## Email: grelber13@gmail.com


####Set working directory####

setwd("~/ERHS732/")


####Load necessary packages####

library(tidyverse)
library(data.table)
library(lubridate)
library(dplyr)
library(rnoaa)
library(riem)
library(usmap)
library(rgeos)
library(sp)
library(rgdal)

####Identify NOAA stations within the five counties####
## Kings county
#Find county boundaries
us_map(
  regions = c("counties")
) %>%
  filter(county=="Kings County") %>%
  filter(abbr=="NY")

kings <- plot_usmap(regions=c("county"),include=c("36047"))
kings_dat <- us_map(regions=c("county"),include=c("36047"))

#What are the county "limits"?
print(min(kings_dat$x))#2143713
print(max(kings_dat$x))#2166203
print(min(kings_dat$y))#-149246.1
print(max(kings_dat$y))#-129019.1

#Find NOAA weather monitors
NY_ASOS_networks <- c("NY_ASOS")
NY_ASOS_stations <- data.frame()
for (i in 1:length(NY_ASOS_networks)) {
  temp_stations <- riem_stations(NY_ASOS_networks[i]) %>%
    mutate(network=NY_ASOS_networks[i])
  NY_ASOS_stations <- bind_rows(NY_ASOS_stations,temp_stations)
}

#Transform lat/lon to x/y for ggplot and filter to nearby-ish monitors
NY_ASOS_stations <- NY_ASOS_stations %>%
  rename(latitude=lat,
         longitude=lon)
transformed_data <- usmap_transform(NY_ASOS_stations %>%
                                      select(longitude,latitude,name)) %>%
  filter(longitude.1>2140000) %>%
  filter(longitude.1<2170000) %>%
  filter(latitude.1<(-128000)) %>%
  filter(latitude.1>(-150000))

# Plot transformed data on map
kings + geom_point(
  data = transformed_data,
  aes(x = longitude.1, y = latitude.1,color=name),
  alpha = 0.5
) #Choose "Manhattan - Wall Street" monitor


all_stations <- NY_ASOS_stations %>%
  filter(name=="Manhattan - Wall Street") %>%
  mutate(county="Kings")
## LA county
#Find county boundaries
us_map(
  regions = c("counties")
) %>%
  filter(county=="Los Angeles County") %>%
  filter(abbr=="CA")

la <- plot_usmap(regions=c("county"),include=c("06037"))
la_dat <- us_map(regions=c("county"),include=c("06037"))

#What are the county "limits"?
print(min(la_dat$x))#-1735612
print(max(la_dat$x))#-1604671
print(min(la_dat$y))#-1072487
print(max(la_dat$y))#-938048.9

#Find NOAA weather monitors
CA_ASOS_networks <- c("CA_ASOS")
CA_ASOS_stations <- data.frame()
for (i in 1:length(CA_ASOS_networks)) {
  temp_stations <- riem_stations(CA_ASOS_networks[i]) %>%
    mutate(network=CA_ASOS_networks[i])
  CA_ASOS_stations <- bind_rows(CA_ASOS_stations,temp_stations)
}

#Transform lat/lon to x/y for ggplot and filter to nearby-ish monitors
CA_ASOS_stations <- CA_ASOS_stations %>%
  rename(latitude=lat,
         longitude=lon)
transformed_data <- usmap_transform(CA_ASOS_stations %>%
                                      select(longitude,latitude,name)) %>%
  filter(longitude.1>(-1720000)) %>%
  filter(longitude.1<(-1590000)) %>%
  filter(latitude.1<(-930000)) %>%
  filter(latitude.1>(-1080000))

# Plot transformed data on map
la + geom_point(
  data = transformed_data,
  aes(x = longitude.1, y = latitude.1,color=name),
  alpha = 0.5
) +
  theme(legend.position="right")
key <- data.frame(name=c("BURBANK/GLENDALE","Cable",
                  "CHINO AIRPORT","Corona",
                  "EDWARDS AFB","EDWARDS N-AUX",
                  "EL MONTE","FULLERTON MUNICIPAL",
                  "HAWTHORNE MUNICIPAL","LA / WHITEMAN",
                  "LA VERNE/BRACKETT","LANCASTER/FOX FIELD",
                  "LONG BEACH AIRPORT","LOS ALAMITOS AAF",
                  "LOS ANGELES DOWNTOWN/USC",
                  "LOS ANGELES INTL","MOUNT WILSON",
                  "NEWHALL","ONTARIO INTL ARPT",
                  "Palmdale","PALMDALE PRODUCTION",
                  "RIVERSIDE MUNICIPAL","SANDBURG (AUT)",
                  "SANTA MONICA MUNI","TORRANCE MUNICIPAL",
                  "VAN NUYS AIRPORT"),
           overlap=c("Y",rep("N",5),"Y",
                     "N",rep("Y",5),"N",rep("Y",4),
                     "N",rep("Y",2),"N",rep("Y",4)))
la + geom_point(
  data = transformed_data %>%
    left_join(key,by=c("name")),
  aes(x = longitude.1, y = latitude.1,color=overlap),
  alpha = 0.5
) +
  theme(legend.position="right")
#the ones to use for LA
print(transformed_data %>%
        left_join(key,by=c("name")) %>%
        filter(overlap=="Y") %>%
        select(name))

all_stations <- bind_rows(all_stations,
                          transformed_data %>%
                            left_join(key,by=c("name")) %>%
                            filter(overlap=="Y") %>%
                            select(name) %>%
                            left_join(CA_ASOS_stations %>%
                                        mutate(county="LA"),
                                      by=c("name")))

## Queens county
#Find county boundaries
us_map(
  regions = c("counties")
) %>%
  filter(county=="Queens County") %>%
  filter(abbr=="NY")

queens <- plot_usmap(regions=c("county"),include=c("36081"))
queens_dat <- us_map(regions=c("county"),include=c("36081"))

#What are the county "limits"?
print(min(queens_dat$x))#2149973
print(max(queens_dat$x))#2171691
print(min(queens_dat$y))#-142625.1
print(max(queens_dat$y))#-116124.8

transformed_data <- usmap_transform(NY_ASOS_stations %>%
                                      select(longitude,latitude,name)) %>%
  filter(longitude.1>2140000) %>%
  filter(longitude.1<2180000) %>%
  filter(latitude.1<(-100000)) %>%
  filter(latitude.1>(-140000))

# Plot transformed data on map
queens + geom_point(
  data = transformed_data,
  aes(x = longitude.1, y = latitude.1,color=name),
  alpha = 0.5
) +
  theme(legend.position="right")#use "NEW YORK/JF KENNEDY" and "New York/LaGuardia"

all_stations <- bind_rows(all_stations,
                          NY_ASOS_stations %>%
                            filter(name=="NEW YORK/JF KENNEDY") %>%
                            mutate(county="Queens"),
                          NY_ASOS_stations %>%
                            filter(name=="New York/LaGuardia") %>%
                            mutate(county="Queens"))

## Cook county
#Find county boundaries
us_map(
  regions = c("counties")
) %>%
  filter(county=="Cook County") %>%
  filter(abbr=="IL")

cook <- plot_usmap(regions=c("county"),include=c("17031"))
cook_dat <- us_map(regions=c("county"),include=c("17031"))

#What are the county "limits"?
print(min(cook_dat$x))#965033.9
print(max(cook_dat$x))#1034903
print(min(cook_dat$y))#-316997
print(max(cook_dat$y))#-241378.7

#Find NOAA weather monitors
IL_ASOS_networks <- c("IL_ASOS")
IL_ASOS_stations <- data.frame()
for (i in 1:length(IL_ASOS_networks)) {
  temp_stations <- riem_stations(IL_ASOS_networks[i]) %>%
    mutate(network=IL_ASOS_networks[i])
  IL_ASOS_stations <- bind_rows(IL_ASOS_stations,temp_stations)
}

#Transform lat/lon to x/y for ggplot and filter to nearby-ish monitors
IL_ASOS_stations <- IL_ASOS_stations %>%
  rename(latitude=lat,
         longitude=lon)
transformed_data <- usmap_transform(IL_ASOS_stations %>%
                                      select(longitude,latitude,name)) %>%
  filter(longitude.1>960000) %>%
  filter(longitude.1<1040000) %>%
  filter(latitude.1<(-240000)) %>%
  filter(latitude.1>(-310000))

# Plot transformed data on map
cook + geom_point(
  data = transformed_data,
  aes(x = longitude.1, y = latitude.1,color=name),
  alpha = 0.5
) +
  theme(legend.position="right")
key <- data.frame(name=c("CHICAGO","Chicago - Schaumburg",
                         "CHICAGO O HARE","CHICAGO/DUPAGE",
                         "CHICAGO/LANSING","CHICAGO/MEIGS",
                         "PALWAUKEE","ROMEOVILLE/CHI"),
                  overlap=c("Y","Y","Y","N","Y",
                            "N","Y","N"))
cook + geom_point(
  data = transformed_data %>%
    left_join(key,by=c("name")),
  aes(x = longitude.1, y = latitude.1,color=overlap),
  alpha = 0.5
) +
  theme(legend.position="right")
#the ones to use for Cook
print(transformed_data %>%
        left_join(key,by=c("name")) %>%
        filter(overlap=="Y") %>%
        select(name))

all_stations <- bind_rows(all_stations,
                          transformed_data %>%
                            left_join(key,by=c("name")) %>%
                            filter(overlap=="Y") %>%
                            select(name) %>%
                            left_join(IL_ASOS_stations %>%
                                        mutate(county="Cook"),
                                      by=c("name")))


## Maricopa county
#Find county boundaries
us_map(
  regions = c("counties")
) %>%
  filter(county=="Maricopa County") %>%
  filter(abbr=="AZ")

maricopa <- plot_usmap(regions=c("county"),include=c("04013"))
maricopa_dat <- us_map(regions=c("county"),include=c("04013"))

#What are the county "limits"?
print(min(maricopa_dat$x))#-1251643
print(max(maricopa_dat$x))#-1025755
print(min(maricopa_dat$y))#-1304523
print(max(maricopa_dat$y))#-1124700

#Find NOAA weather monitors
AZ_ASOS_networks <- c("AZ_ASOS")
AZ_ASOS_stations <- data.frame()
for (i in 1:length(AZ_ASOS_networks)) {
  temp_stations <- riem_stations(AZ_ASOS_networks[i]) %>%
    mutate(network=AZ_ASOS_networks[i])
  AZ_ASOS_stations <- bind_rows(AZ_ASOS_stations,temp_stations)
}

#Transform lat/lon to x/y for ggplot and filter to nearby-ish monitors
AZ_ASOS_stations <- AZ_ASOS_stations %>%
  rename(latitude=lat,
         longitude=lon)
transformed_data <- usmap_transform(AZ_ASOS_stations %>%
                                      select(longitude,latitude,name)) %>%
  filter(longitude.1>(-1260000)) %>%
  filter(longitude.1<(-1000000)) %>%
  filter(latitude.1<(-1100000)) %>%
  filter(latitude.1>(-1300000))

# Plot transformed data on map
maricopa + geom_point(
  data = transformed_data,
  aes(x = longitude.1, y = latitude.1,color=name),
  alpha = 0.5
) +
  theme(legend.position="right")

key <- data.frame(name=c("Buckeye","CASA GRANDA (AWOS)",
                         "CHANDLER MUNICIPAL AIRPORT","Coolidge",
                         "DEER VALLEY/PHOENIX","GILA BEND (AAF)",
                         "Gila Bend Af Aux","GLENDALE",
                         "GOODYEAR MUNICIPAL","LUKE AFB/PHOENIX",
                         "Maricopa - Ak-chin","MESA/FALCON FIELD",
                         "Payson","PHOENIX/SKY HARBOR",
                         "SCOTTSDALE MUNI","WILLIAMS AFB/CHANDL"),
                  overlap=c("Y","N","Y","N",rep("Y",6),
                            "N",rep("Y",1),"N",rep("Y",3)))
maricopa + geom_point(
  data = transformed_data %>%
    left_join(key,by=c("name")),
  aes(x = longitude.1, y = latitude.1,color=overlap),
  alpha = 0.5
) +
  theme(legend.position="right")
#the ones to use for LA
print(transformed_data %>%
        left_join(key,by=c("name")) %>%
        filter(overlap=="Y") %>%
        select(name))
all_stations <- bind_rows(all_stations,
                          transformed_data %>%
                            left_join(key,by=c("name")) %>%
                            filter(overlap=="Y") %>%
                            select(name) %>%
                            left_join(AZ_ASOS_stations %>%
                                        mutate(county="Maricopa"),
                                      by=c("name")))

####Write function to clean data to be daily####
getmode <- function(v) {
  uniqv <- unique(v)[which(!is.na(unique(v)))]
  uniqv[which.max(tabulate(match(v[which(!is.na(v))], uniqv)))]
}
pull_riem <- function(test_station_ID) {
  test_riem_measures <- riem_measures(station = test_station_ID, date_start = "2019-11-30",
                                      date_end = "2021-09-25") %>%
    mutate(date=as.Date(valid,tryFormats=c("%Y-%m-%d %H:%M:%S"))) %>%
    select(station,date,tmpf,relh,valid) %>%
    group_by(station,date)
  if (nrow(test_riem_measures)>1) {
    tmpf_non_NAs <- test_riem_measures %>%
      filter(!is.na(tmpf)) %>%
      group_by(station) %>%
      mutate(interval = as.character.POSIXt(valid-lag(valid, order_by = valid))) %>%
      mutate(interval_num=0) %>%
      filter(!grepl("NA",interval))
    
    relh_non_NAs <- test_riem_measures %>%
      filter(!is.na(relh)) %>%
      group_by(station) %>%
      mutate(interval = as.character.POSIXt(valid-lag(valid, order_by = valid))) %>%
      mutate(interval_num=0) %>%
      filter(!grepl("NA",interval))
    
    for (i in 1:nrow(tmpf_non_NAs)) {
      if (grepl("min",tmpf_non_NAs[i,"interval"])) {
        tmpf_non_NAs[i,"interval_num"] <- as.double(str_split(tmpf_non_NAs[i,"interval"]," ")[[1]][length(str_split(tmpf_non_NAs[i,"interval"]," ")[[1]])-1])/60
      } else {
        tmpf_non_NAs[i,"interval_num"] <- as.double(str_split(tmpf_non_NAs[i,"interval"]," ")[[1]][length(str_split(tmpf_non_NAs[i,"interval"]," ")[[1]])-1])
      } 
    }
    
    for (i in 1:nrow(relh_non_NAs)) {
      if (grepl("min",relh_non_NAs[i,"interval"])) {
        relh_non_NAs[i,"interval_num"] <- as.double(str_split(relh_non_NAs[i,"interval"]," ")[[1]][length(str_split(relh_non_NAs[i,"interval"]," ")[[1]])-1])/60
      } else {
        relh_non_NAs[i,"interval_num"] <- as.double(str_split(relh_non_NAs[i,"interval"]," ")[[1]][length(str_split(relh_non_NAs[i,"interval"]," ")[[1]])-1])
      } 
    }
    
    tmpf_interval_df <- tmpf_non_NAs %>%
      select(-interval) %>%
      rename(interval=interval_num) %>%
      group_by(date,station) %>%
      mutate(tmpf_interval = getmode(interval)) %>%
      select(tmpf_interval,station,date) %>%
      distinct()
    relh_interval_df <- relh_non_NAs %>%
      select(-interval) %>%
      rename(interval=interval_num) %>%
      group_by(date,station) %>%
      mutate(relh_interval = getmode(interval)) %>%
      select(relh_interval,station,date) %>%
      distinct()
    
    test_riem_measures_temp <- test_riem_measures %>%
      filter(!is.na(tmpf)) %>%
      full_join(tmpf_interval_df,by=c("station","date")) %>%
      group_by(date) %>%
      add_tally() %>%
      group_by(station,date,tmpf_interval,n) %>%
      summarise(mean_tmpf=mean(tmpf,na.rm=TRUE),
                max_tmpf=max(tmpf,na.rm=TRUE)) %>%
      mutate(pct_reading_temp=(n/(24/as.double(tmpf_interval)))*100) %>%
      ungroup() %>%
      select(-n) %>%
      filter(pct_reading_temp>75)
    test_riem_measures_relh <- test_riem_measures %>%
      filter(!is.na(relh)) %>%
      full_join(relh_interval_df,by=c("station","date")) %>%
      group_by(date) %>%
      add_tally() %>%
      group_by(station,date,relh_interval,n) %>%
      summarise(mean_relh=mean(relh,na.rm=TRUE)) %>%
      mutate(pct_reading_relh=(n/(24/as.double(relh_interval)))*100) %>%
      ungroup() %>%
      select(-n) %>%
      filter(pct_reading_relh>75)
    output_riem_measures <- full_join(test_riem_measures_temp,test_riem_measures_relh,by=c("date","station"))
    return(output_riem_measures)
  }
}

#' Iterate through identified stations for all five counties
weather_df <- data.frame()
for (i in 29:nrow(all_stations)) {
  temp_weather_df <- pull_riem(all_stations[i,"id"])
  print(paste0("finished with ",i," of ",nrow(all_stations)))
  weather_df <- bind_rows(temp_weather_df,weather_df)
}
weather_df_summ <- left_join(weather_df,all_stations %>%
                          rename(station=id),
                        by=c("station")) %>%
  group_by(county,date) %>%
  summarise(relh=mean(mean_relh),
            max_temp=mean(max_tmpf),
            mean_temp=mean(mean_tmpf))

saveRDS(weather_df_summ,"weather_df_summ.rds")
