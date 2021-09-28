## ---------------------------
##
## Script name: Merge COVID-19 datasets
##
## Purpose of script: Merge together three datasets for time-series
##                    analysis. 1) COVID-19 mortality data, 2) NOAA
##                    meteorological data, 3) EPA PM2.5 and ozone data
##                    
## Authors: Grace Kuiper, Becky Witinok-Huber, Bonnie Young
##
## Date Created: 09/28/2021
## Newest Version: 09/28/2021
##
## Email: grelber13@gmail.com


####Set working directory####

setwd("~/ERHS732/")


####Load necessary packages####

library(tidyverse)
library(data.table)
library(lubridate)
library(dplyr)

####Read in COVID-19 data####
covid_df <- read.csv("us-counties.csv") %>%
  filter(county=="Los Angeles",
         state=="California") %>%
  mutate(date=as.Date(date,format="%Y-%m-%d")) %>%
  arrange(date)

####Read in cleaned NOAA data####
weather_df <- readRDS("weather_df_summ.rds") %>%
  filter(county=="LA") %>%
  mutate(county=ifelse(county=="LA","Los Angeles",NA))

####Read in cleaned exposure data####
exp_df <- readRDS("total_exp_data.rds") %>%
  filter(county=="Los Angeles")

####Merge together all three dataframes####
timeseries_df <- full_join(covid_df,weather_df,
                           by=c("county","date")) %>%
  full_join(exp_df,by=c("county","date"))
saveRDS(timeseries_df,"timeseries_df.rds")
