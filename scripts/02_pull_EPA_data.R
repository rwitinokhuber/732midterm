## ---------------------------
##
## Script name: Pull EPA PM2.5 and ozone data 
##
## Purpose of script: Pull PM2.5 and ozone data from EPA for LA county
##                    and merge to become the exposure data for time-series
##                    analysis
##                    
## Authors: Grace Kuiper, Becky Witinok-Huber, Bonnie Young
##
## Date Created: 09/27/2021
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
library(ggplot2)
library(ggpubr)

####Read in PM data####
PM_2020 <- read.csv("PM_2020_LA.csv") %>%
  select(Date,`Daily.Mean.PM2.5.Concentration`,DAILY_AQI_VALUE,COUNTY,`Site.Name`) %>%
  mutate(Date=as.Date(Date,format="%m/%d/%y")) %>%
  rename(PM_conc=`Daily.Mean.PM2.5.Concentration`,
         date=Date,
         AQI=DAILY_AQI_VALUE,
         county=COUNTY,
         Station=`Site.Name`) %>%
  arrange(date)
PM_2021 <- read.csv("PM_2021_LA.csv") %>%
  select(Date,`Daily.Mean.PM2.5.Concentration`,DAILY_AQI_VALUE,COUNTY,`Site.Name`) %>%
  mutate(Date=as.Date(Date,format="%m/%d/%y")+366) %>%
  rename(PM_conc=`Daily.Mean.PM2.5.Concentration`,
         date=Date,
         AQI=DAILY_AQI_VALUE,
         county=COUNTY,
         Station=`Site.Name`) %>%
  arrange(date)

####Clean PM data####
PM_total <- bind_rows(PM_2020,PM_2021) %>%
  group_by(date,county) %>%
  summarise(PM_conc_max=max(PM_conc),
            PM_conc_mean=mean(PM_conc),
            AQI_PM_max=max(AQI),
            AQI_PM_mean=mean(AQI))

####Plot correlation and distribution of mean daily PM concentration using maximum vs. mean across stations####
sp <- ggscatter(PM_total,
                x = "PM_conc_max", y = "PM_conc_mean",
                add = "reg.line",  # Add regressin line
                add.params = list(color = "blue", fill = "lightgray"), # Customize reg. line
                conf.int = TRUE # Add confidence interval
)
sp + stat_cor(method = "pearson", label.x = 3, label.y = 30)

colors <- c("Mean" = "#FF6666", "Maximum" = "blue")
ggplot(PM_total) +
  geom_density(aes(x=PM_conc_max, fill="Maximum"),alpha=.2) +
  geom_density(aes(x=PM_conc_mean, fill="Mean"),alpha=.2) +
  theme_bw() +
  labs(x = "Mean daily concentration of PM2.5",
       y = "density",
       fill = "Summarized across\nstations using:") +
  scale_fill_manual(values = colors)
  
ggplot(PM_total) +
  geom_histogram(aes(x=PM_conc_mean,fill="Mean"),color="black",alpha=0.2,binwidth=4)+
  geom_histogram(aes(x=PM_conc_max,fill="Maximum"),color="black",alpha=0.2,binwidth=4) +
  theme_bw() +
  labs(x = "Mean daily concentration of PM2.5",
       fill = "Summarized across\nstations using:") +
  scale_fill_manual(values = colors)

sp <- ggscatter(PM_total,
                x = "AQI_PM_max", y = "AQI_PM_mean",
                add = "reg.line",  # Add regressin line
                add.params = list(color = "blue", fill = "lightgray"), # Customize reg. line
                conf.int = TRUE # Add confidence interval
)
sp + stat_cor(method = "pearson", label.x = 3, label.y = 120)

ggplot(PM_total) +
  geom_density(aes(x=AQI_PM_max, fill="Maximum"),alpha=.2) +
  geom_density(aes(x=AQI_PM_mean, fill="Mean"),alpha=.2) +
  theme_bw() +
  labs(x = "Mean daily AQI for PM2.5 concentration",
       y = "density",
       fill = "Summarized across\nstations using:") +
  scale_fill_manual(values = colors)

ggplot(PM_total) +
  geom_histogram(aes(x=AQI_PM_mean,fill="Mean"),color="black",alpha=0.2,binwidth=4)+
  geom_histogram(aes(x=AQI_PM_max,fill="Maximum"),color="black",alpha=0.2,binwidth=4) +
  theme_bw() +
  labs(x = "Mean daily AQI for PM2.5 concentration",
       fill = "Summarized across\nstations using:") +
  scale_fill_manual(values = colors)

####Read in Ozone data####
Ozone_2020 <- read.csv("Ozone_2020_LA.csv") %>%
  select(Date,`Daily.Max.8.hour.Ozone.Concentration`,DAILY_AQI_VALUE,COUNTY,`Site.Name`) %>%
  mutate(Date=as.Date(Date,format="%m/%d/%y")) %>%
  rename(Ozone_conc=`Daily.Max.8.hour.Ozone.Concentration`,
         date=Date,
         AQI=DAILY_AQI_VALUE,
         county=COUNTY,
         Station=`Site.Name`) %>%
  arrange(date)
Ozone_2021 <- read.csv("Ozone_2021_LA.csv") %>%
  select(Date,`Daily.Max.8.hour.Ozone.Concentration`,DAILY_AQI_VALUE,COUNTY,`Site.Name`) %>%
  mutate(Date=as.Date(Date,format="%m/%d/%y")+366) %>%
  rename(Ozone_conc=`Daily.Max.8.hour.Ozone.Concentration`,
         date=Date,
         AQI=DAILY_AQI_VALUE,
         county=COUNTY,
         Station=`Site.Name`) %>%
  arrange(date)

####Clean ozone data####
Ozone_total <- bind_rows(Ozone_2020,Ozone_2021) %>%
  group_by(date,county) %>%
  summarise(Ozone_conc_max=max(Ozone_conc),
            Ozone_conc_mean=mean(Ozone_conc),
            AQI_Ozone_max=max(AQI),
            AQI_Ozone_mean=mean(AQI))

####Plot correlation and distribution of mean daily ozone concentration using maximum vs. mean across stations####
sp <- ggscatter(Ozone_total,
                x = "Ozone_conc_max", y = "Ozone_conc_mean",
                add = "reg.line",  # Add regressin line
                add.params = list(color = "blue", fill = "lightgray"), # Customize reg. line
                conf.int = TRUE # Add confidence interval
)
sp + stat_cor(method = "pearson", label.x = 0.05, label.y = 0.08)

ggplot(Ozone_total) +
  geom_density(aes(x=Ozone_conc_max, fill="Maximum"),alpha=.2) +
  geom_density(aes(x=Ozone_conc_mean, fill="Mean"),alpha=.2) +
  theme_bw() +
  labs(x = "Mean daily concentration of ozone",
       y = "density",
       fill = "Summarized across\nstations using:") +
  scale_fill_manual(values = colors)

ggplot(Ozone_total) +
  geom_histogram(aes(x=Ozone_conc_mean,fill="Mean"),color="black",alpha=0.2,binwidth=0.005)+
  geom_histogram(aes(x=Ozone_conc_max,fill="Maximum"),color="black",alpha=0.2,binwidth=0.005) +
  theme_bw() +
  labs(x = "Mean daily concentration of ozone",
       fill = "Summarized across\nstations using:") +
  scale_fill_manual(values = colors)

sp <- ggscatter(Ozone_total,
                x = "AQI_Ozone_max", y = "AQI_Ozone_mean",
                add = "reg.line",  # Add regressin line
                add.params = list(color = "blue", fill = "lightgray"), # Customize reg. line
                conf.int = TRUE # Add confidence interval
)
sp + stat_cor(method = "pearson", label.x = 3, label.y = 120)

ggplot(Ozone_total) +
  geom_density(aes(x=AQI_Ozone_max, fill="Maximum"),alpha=.2) +
  geom_density(aes(x=AQI_Ozone_mean, fill="Mean"),alpha=.2) +
  theme_bw() +
  labs(x = "Mean daily AQI for ozone concentration",
       y = "density",
       fill = "Summarized across\nstations using:") +
  scale_fill_manual(values = colors)

ggplot(Ozone_total) +
  geom_histogram(aes(x=AQI_Ozone_mean,fill="Mean"),color="black",alpha=0.2,binwidth=5)+
  geom_histogram(aes(x=AQI_Ozone_max,fill="Maximum"),color="black",alpha=0.2,binwidth=5) +
  theme_bw() +
  labs(x = "Mean daily AQI for ozone concentration",
       fill = "Summarized across\nstations using:") +
  scale_fill_manual(values = colors)

####Merge PM and ozone data####
total_exp_data <- full_join(Ozone_total,PM_total,
                            by=c("county","date"))
saveRDS(total_exp_data,"total_exp_data.rds")
