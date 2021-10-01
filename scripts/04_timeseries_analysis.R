## ---------------------------
##
## Script name: Time-Series Analysis
##
## Purpose of script: Begin time-series anlaysis of COVID-19 mortality
##                    in Los Angeles county to model its association with
##                    daily PM2.5 and ozone
##                    
## Authors: Grace Kuiper, Becky Witinok-Huber, Bonnie Young
##
## Date Created: 09/28/2021
## Newest Version: 09/28/2021
##
## Email: grelber13@gmail.com


####Setup####

##Set working directory##
setwd("~/ERHS732/")


##Load necessary packages##
library(tidyverse)
library(data.table)
library(lubridate)
library(dplyr)

##Read in data##
timeseries_df <- readRDS('~/ERHS732/timeseries_df.rds')

##Caliculate daily deaths from cumulative deaths##
summary(timeseries_df %>% select(PM_conc_mean,Ozone_conc_mean,relh,max_temp))
for(i in 1:nrow(timeseries_df)) {
  if (i==1) {
    timeseries_df[i,"daily_deaths"] <- 0
  } else {
    timeseries_df[i,"daily_deaths"] <- timeseries_df[i,"deaths"]-timeseries_df[i-1,"deaths"]
  }
}

####Explore the data####
##Plot daily deaths over time##
timeseries_df %>%
  ggplot(aes(x=date,y=daily_deaths)) +
  geom_line() +
  geom_smooth() +
  theme_bw()

##Create day of week variable##
timeseries_df <- timeseries_df %>%
  mutate(dow=weekdays(date)) %>%
  mutate(dow=factor(dow,levels=c("Sunday","Monday","Tuesday",
                                 "Wednesday","Thursday","Friday","Saturday"),
                    ordered=FALSE))


####Analysis####

#'Filter out negative daily death values, maximum outlier,
#'and days with missing exposure and confounder data
ts_mod_df <- timeseries_df %>%
  filter(daily_deaths>-1) %>%
  filter(daily_deaths<900) %>%
  filter(!is.na(PM_conc_mean),
         !is.na(Ozone_conc_mean),
         !is.na(max_temp))

#' linear model with daily PM2.5 predicting daily deaths
mod_linear_reg <- glm(daily_deaths ~ PM_conc_mean, data=ts_mod_df,
                      family="quasipoisson")

summary(mod_linear_reg)

glance(mod_linear_reg)
tidy(mod_linear_reg,conf.int=T)
augment(mod_linear_reg)

mod_linear_reg %>% 
  augment() %>%
  ggplot(aes(x=PM_conc_mean)) +
  geom_point(aes(y=daily_deaths),alpha=0.4,size=0.5) +
  geom_line(aes(y=.fitted),color="red") +
  labs(x="Mean daily PM2.5", y="Expected mortality count") +
  theme_bw()


#' linear model with daily PM2.5 and ozone predicting daily deaths
mod_linear_reg_2 <- glm(daily_deaths ~ PM_conc_mean + Ozone_conc_mean, data=ts_mod_df,
                      family="quasipoisson")

summary(mod_linear_reg_2)

glance(mod_linear_reg_2)
tidy(mod_linear_reg_2,conf.int=T)
augment(mod_linear_reg_2)

mod_linear_reg_2 %>% 
  augment() %>%
  ggplot(aes(x=PM_conc_mean)) +
  geom_point(aes(y=daily_deaths),alpha=0.4,size=0.5) +
  geom_line(aes(y=exp(.fitted)),color="red") +
  labs(x="Mean daily PM2.5", y="Log(Expected mortality count)") +
  theme_bw()

mod_linear_reg_2 %>%
  augment() %>%
  ggplot(aes(x=Ozone_conc_mean)) +
  geom_point(aes(y=daily_deaths),alpha=0.4,size=0.5) +
  geom_line(aes(y=exp(.fitted)),color="red") +
  labs(x="Daily maximum 8-hour ozone",y="Log(Expected mortality count)") +
  theme_bw()


#' linear model with daily PM2.5 and ozone predicting daily deaths,
#' controlling for DOW
mod_linear_reg_3 <- glm(daily_deaths ~ PM_conc_mean + Ozone_conc_mean + dow, data=ts_mod_df,
                        family="quasipoisson")

summary(mod_linear_reg_3)

glance(mod_linear_reg_3)
tidy(mod_linear_reg_3,conf.int=T)
augment(mod_linear_reg_3)

mod_linear_reg_3 %>% 
  augment() %>%
  ggplot(aes(x=PM_conc_mean)) +
  geom_point(aes(y=daily_deaths),alpha=0.4,size=0.5) +
  geom_line(aes(y=exp(.fitted)),color="red") +
  labs(x="Mean daily PM2.5", y="Log(Expected mortality count)") +
  theme_bw()

mod_linear_reg_3 %>%
  augment() %>%
  ggplot(aes(x=Ozone_conc_mean)) +
  geom_point(aes(y=daily_deaths),alpha=0.4,size=0.5) +
  geom_line(aes(y=exp(.fitted)),color="red") +
  labs(x="Daily maximum 8-hour ozone",y="Log(Expected mortality count)") +
  theme_bw()

#' How are daily mortality related with a linear and non-linear function of time?
ts_mod_df <- ts_mod_df %>%
  mutate(time=as.numeric(date)-first(as.numeric(date)))

ggplot(ts_mod_df, aes(x=time,y=daily_deaths)) +
  geom_point(size=0.5,alpha=0.5) +
  theme_bw()

mod_time <- glm(daily_deaths ~ time, data=ts_mod_df, family="quasipoisson")
mod_time %>%
  augment() %>%
  ggplot(aes(x=time)) +
  geom_point(aes(y=daily_deaths), alpha=0.4,size=0.5) +
  geom_line(aes(y=exp(.fitted)),color="red") +
  labs(x="Date in study", y="Expected mortality count") +
  theme_bw()

library(splines)
mod_time_nonlin <- glm(daily_deaths ~ ns(time,df=12),
                       data=ts_mod_df,family="quasipoisson")
mod_time_nonlin %>%
  augment() %>%
  mutate(time=ts_mod_df$time) %>%
  ggplot(aes(x=time)) +
  geom_point(aes(y=daily_deaths),alpha=0.4,size=0.5) +
  geom_line(aes(y=exp(.fitted)),color="red") +
  labs(x="Date in study",y="Expected mortality count") +
  theme_bw()

#'Now fit model with ozone and pm, controlling for DOW
#'and long-term and seasonal trends in mortality using spline with
#'12 df on time

mod_ctrl_dow <- glm(daily_deaths ~ PM_conc_mean + Ozone_conc_mean + dow, 
                    data = ts_mod_df, family = "quasipoisson")
mod_ctrl_dow_time <- glm(daily_deaths ~ PM_conc_mean + Ozone_conc_mean + dow +
                           ns(time, df = 12), 
                         data = ts_mod_df, family = "quasipoisson")
mod_ctrl_dow %>% 
  tidy() %>% 
  filter(grepl("conc_mean",term))

mod_ctrl_dow_time %>%
  tidy() %>%
  filter(grepl("conc_mean",term))

#'Add in controlling for temperature
mod_ctrl_weather <- glm(daily_deaths ~ PM_conc_mean + Ozone_conc_mean + dow +
                           max_temp + ns(time, df = 12), 
                         data = ts_mod_df, family = "quasipoisson")
summary(mod_ctrl_weather)
mod_ctrl_weather %>% 
  tidy() %>% 
  filter(grepl("conc_mean",term))

mod_ctrl_weather %>% 
  augment() %>%
  ggplot(aes(x=PM_conc_mean)) +
  geom_point(aes(y=daily_deaths),alpha=0.4,size=0.5) +
  geom_line(aes(y=exp(.fitted)),color="red") +
  labs(x="Mean daily PM2.5", y="Log(Expected mortality count)") +
  theme_bw()

#'Include non-linear functions on PM, ozone, and temperature
mod_ctrl_nl <- glm(daily_deaths ~ ns(PM_conc_mean,df=4) + ns(Ozone_conc_mean,df=4) + dow +
                          ns(max_temp,df=4) + ns(time, df = 12), 
                        data = ts_mod_df, family = "quasipoisson")

summary(mod_ctrl_nl)
mod_ctrl_nl %>%
  augment() %>%
  mutate(PM_conc_mean = ts_mod_df$PM_conc_mean) %>%
  ggplot(aes(x=PM_conc_mean)) +
  geom_point(aes(y=daily_deaths),alpha=0.4,size=0.5) +
  geom_line(aes(y=exp(.fitted)),color="red",alpha=0.7,size=0.4) +
  labs(x="Daily mean PM2.5",y="Expected mortality count") +
  theme_bw()

#'Generate predictions using spline for PM2.5
pm_spline <- ns(ts_mod_df$PM_conc_mean, df=4)
pm_spline_preds <- predict(pm_spline, newx=2.5:63.2)
pm_spline_preds %>%
  head()

mod_ctrl_nl %>%
  tidy() %>%
  filter(str_detect(term,"PM_conc_mean"))

pm_spline_ests <- mod_ctrl_nl %>%
  tidy() %>%
  filter(str_detect(term,"PM_conc_mean")) %>%
  pull(estimate)

pm_spline_ests

pred_pm_function <- tibble(
  pm = 2.5:63.2,
  pm_func=pm_spline_preds %*% pm_spline_ests,
  rr=exp(pm_func)
)

pred_pm_function %>%
  head()

ggplot(pred_pm_function,aes(x=pm,y=rr)) +
  geom_point() +
  geom_line() +
  labs(x="PM2.5",
       y="Relative Risk") +
  geom_hline(yintercept = 1.0,color="red",linetype=2) +
  geom_vline(xintercept = 5.0,color="blue",linetype=3) +
  geom_vline(xintercept = 15.0,color="blue",linetype=3) +
  theme_bw()

library(dlnm)
pm_basis <- crossbasis(ts_mod_df$PM_conc_mean, lag = 0, 
                         argvar = list(fun = "ns", df = 4), 
                         arglag = list(fun = "integer"))
pm_basis %>% 
  head()
pm_spline %>%
  head()

dlnm_mod_1 <- glm(daily_deaths ~ pm_basis + dow + ns(max_temp,df=4) +
                    ns(Ozone_conc_mean,df=4) + ns(time,df=12),
                  data=ts_mod_df,family="quasipoisson")
dlnm_mod_1 %>%
  tidy() %>%
  filter(str_detect(term,"pm_basis"))

crosspred(basis=pm_basis,model=dlnm_mod_1,
          cen=2.5,by=1) %>%
  str()

est_rr <- dlnm_mod_1 %>%
  crosspred(basis=pm_basis,model=.,cen=2.5,by=1) %>%
  pluck("allRRfit")
est_rr

dlnm_temp_function <- tibble(PM_conc_mean=as.numeric(names(est_rr)),
                             rr=est_rr)

dlnm_temp_function %>%
  head()

ggplot(dlnm_temp_function,aes(x=PM_conc_mean,y=rr)) +
  geom_point() +
  geom_line() +
  labs(x="Mean daily PM2.5",y="Relative Risk") +
  geom_hline(yintercept = 1.0,color="red",linetype=2)+
  theme_bw() +
  geom_vline(xintercept = 5.0,color="blue",linetype=3)+
  geom_vline(xintercept=15.0,color="blue",linetype=3)
dlnm_mod_1 %>%
  crosspred(basis=pm_basis,model=.,cen=2.5,by=1) %>%
  plot()
