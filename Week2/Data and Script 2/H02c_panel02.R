################################################################### 
# H02c: Panel Data Regression Analysis
# GoDaddy's Venture Forward Analysis 
# Data: May 2018, Nov 2018, Feb 2019, May 2019, Sept 2019 to July 2020
# By William Yu
# 5/19/2023
################################################################### 
setwd("C:/Users/wiyu/documents/Zip08/2023 Q4 Fall_Harvard/Data/")
library(readxl)  
library(dplyr)
library(tidyr)
library(lubridate)

ad.cy = read.csv("W08a_ACS_Selected_County2018.csv") 
ventures = read.csv("W08b_ventureAndOutcomeCountyData_vf.csv") 
covid_case = read.csv("W08c_covid_cases_usafacts202008.csv") 
covid_death = read.csv("W08d_covid_deaths_usafacts202008.csv") 

# COVID 19 data: Time series/stamp management
# gather: convert data from wide-form to long-form
n=ncol(covid_case)
c19_case = gather(covid_case, "day", "cases", 5:n)
c19_case = c19_case %>% mutate(day=gsub("X","", day)) %>% mutate(day=as.Date(day, format = "%m.%d.%Y")) 
c19_case = c19_case %>% mutate(year_month = make_date(year=year(day),month=month(day)))
c19_case_m = c19_case %>% group_by(countyFIPS, year_month) %>% summarize(cases=mean(cases))

c19_death = gather(covid_death, "day", "deaths", 5:n)
c19_death = c19_death %>% mutate(day=gsub("X","", day)) %>% mutate(day=as.Date(day, format = "%m.%d.%Y")) 
c19_death = c19_death %>% mutate(year_month = make_date(year=year(day),month=month(day)))
c19_death_m = c19_death %>% group_by(countyFIPS, year_month) %>% summarize(deaths=mean(deaths))

# Merge the data
c19_whole = left_join(c19_death_m, c19_case_m, by=c("countyFIPS","year_month"))
c19_whole = c19_whole %>% group_by(countyFIPS) %>% mutate(d_cases=cases-lag(cases), d_deaths=deaths-lag(deaths)) %>% 
                          rename(countycode=countyFIPS)

# write.csv(c19_whole,"COVID19_casesdeaths_County.csv", row.names = F)

# Add and arrange time date variable in lventures and rename
ventures = ventures %>% mutate(year_month = as.Date(date)) %>% arrange(county_code, year_month) %>% rename(countycode=county_code)
ad.cy = ad.cy %>% rename(countycode = id)

# Merge all the data
panel18_2007 = left_join(ventures, c19_whole, by=c("countycode","year_month")) %>% left_join(ad.cy, by="countycode") 

# Replace value by Feb 2020 with no cases or deaths with zeros
panel18_2007 = panel18_2007 %>% mutate(deaths = ifelse(is.na(deaths),0,deaths),cases = ifelse(is.na(cases),0,cases),
                                 d_deaths = ifelse(is.na(d_deaths),0,d_deaths), d_cases = ifelse(is.na(d_cases),0,d_cases))

panel18_2007 = panel18_2007 %>% mutate(pd_cases = d_cases/population, pd_deaths = d_deaths/population)

##
## Correlation
##
cs2007 = panel18_2007 %>% subset(year == 2020 & month == 7)
plot(cs2007$chci, cs2007$dv, col="blue", lwd=2)
abline(lm(dv ~ chci, data=cs2007), col="red", lwd=2) 

colnames(panel18_2007)

## Log level: taking care of log (0)
# emp_hhold_S: household employment, (S) seasonally adjusted 
panel18_2007 = panel18_2007 %>% mutate(emp_hhold_S = emp_hhold_S +1, nv = nv +1, nhav = nhav +1)

##
## Cross-sectional Regression 
##
# urate_hhold_S: unemployment rate
p01 = lm(urate_hhold_S ~ chci + colleged + enrollp + mhincome + foreignb + black + asian + latino + mage + log(population) + pdensity 
         + computer + barrier, data=panel18_2007) 
summary(p01)

p02 = lm(urate_hhold_S ~ chci + colleged + enrollp + mhincome + foreignb + black + asian + latino + mage + log(population) + pdensity 
         + computer + barrier + d_cases + pd_cases + pd_deaths, data=panel18_2007) 
summary(p02)

# dv: GoDaddy's venture density
# Simple correlation, univariate regression
# R2 = 0.0
p03 = lm(urate_hhold_S ~ dv, data=panel18_2007)
summary(p03)

# R2 = 0.15
p04 = lm(urate_hhold_S ~ dv + chci + colleged + enrollp + mhincome + foreignb + black + asian + latino + mage + log(population) + pdensity 
         + computer + barrier + d_cases + pd_cases + pd_deaths, data=panel18_2007) 
summary(p04)

# hadv: GoDaddy's highly active venture density
p05 = lm(urate_hhold_S ~ hadv + chci + colleged + enrollp + mhincome + foreignb + black + asian + latino + mage + log(population) + pdensity 
         + computer + barrier + d_cases + pd_cases + pd_deaths, data=panel18_2007) 
summary(p05)

# nv: GoDaddy's number of ventures
p06 = lm(log(emp_hhold_S) ~ log(nv), data=panel18_2007)
summary(p06)

# R2= 0.983
p07 = lm(log(emp_hhold_S) ~ log(nv) + chci + colleged + enrollp + mhincome + foreignb + black + asian + latino + mage + log(population) + pdensity 
         + computer + barrier + log(population) + d_cases + pd_cases + pd_deaths, data=panel18_2007)
summary(p07)

p08 = lm(log(emp_hhold_S) ~ log(nhav) + chci + colleged + enrollp + mhincome + foreignb + black + asian + latino + mage + log(population) + pdensity 
         + computer + barrier + log(population) + d_cases + pd_cases + pd_deaths, data=panel18_2007)
summary(p08)

# AR(1) term; Lag 1 term
# R2= 0.5 Compared to p04 (R2=0.15)
p09 = lm(urate_hhold_S ~ urate_hhold_S_L1 + dv + chci + colleged + enrollp + mhincome + foreignb + black + asian + latino + mage + log(population) + pdensity 
         + computer + barrier + d_cases + pd_cases + pd_deaths, data=panel18_2007) 
summary(p09)

# create lag term directly
p09a = lm(urate_hhold_S ~ lag(urate_hhold_S) + dv + chci + colleged + enrollp + mhincome + foreignb + black + asian + latino + mage + log(population) + pdensity 
         + computer + barrier + d_cases + pd_cases + pd_deaths, data=panel18_2007) 
summary(p09a)

##
## Panel Regression (Level, state and time fixed effect)
##
# R2= 0.7 Compared to p04 (R2=0.15)
p11 = lm(urate_hhold_S ~ dv + chci + colleged + enrollp + mhincome + foreignb + black + asian + latino + mage + log(population) + pdensity 
         + computer + barrier + d_cases + pd_cases + pd_deaths + factor(year_month) + factor(STATE) , data=panel18_2007) 
summary(p11)

# R2= 0.83
p12 = lm(urate_hhold_S ~ urate_hhold_S_L1 + dv + chci + colleged + enrollp + mhincome + foreignb + black + asian + latino + mage + log(population) + pdensity 
         + computer + barrier + d_cases + pd_cases + pd_deaths + factor(year_month) + factor(STATE) , data=panel18_2007) 
summary(p12)

# R2= 0.986
p13 = lm(log(emp_hhold_S) ~ log(nv) + chci + colleged + enrollp + mhincome + foreignb + black + asian + latino + mage + log(population) + pdensity 
         + computer + barrier + log(population) + d_cases + pd_cases + pd_deaths + factor(year_month) + factor(STATE), data=panel18_2007)
summary(p13)

##
## Panel Regression (Difference, state and time fixed effect)
##
# emp_hhold_S_D1: Change (difference from one period over the next period) of household employment 
# emp_hhold_S_L1: Lag (last period) of household employment
# R2=0.08
p21 = lm(emp_hhold_S_D1 ~ nv_D1 + chci + colleged + enrollp + mhincome + foreignb + black + asian + latino + mage + log(population) + pdensity 
         + computer + d_cases + pd_cases + pd_deaths + factor(year_month) + factor(STATE), data=panel18_2007) 
summary(p21)

# R2= 0.10
p22 = lm(emp_hhold_S_D1 ~ lag(emp_hhold_S_D1) + nv_D1 + chci + colleged + enrollp + mhincome + foreignb + black + asian + latino + mage + log(population)
         + pdensity + computer + d_cases + pd_cases + pd_deaths + factor(year_month) + factor(STATE), data=panel18_2007) 
summary(p22)

p23 = lm(emp_hhold_S_D1 ~ lag(emp_hhold_S_D1) + nhav_D1 + chci + colleged + enrollp + mhincome + foreignb + black + asian + latino + mage + log(population)
         + pdensity + computer + d_cases + pd_cases + pd_deaths + factor(year_month) + factor(STATE), data=panel18_2007) 
summary(p23)

" 
# County fixed effect
p24 = lm(emp_hhold_S_D1 ~ lag(emp_hhold_S_D1) + nv_D1 + d_cases + pd_cases + pd_deaths + factor(countycode), data=panel18_2007) 
summary(p24)
"
write.csv(panel18_2007,"panel18_2007_County.csv", row.names = F)

##
## Venture density county map
##
library(usmap)
library(RColorBrewer)
library(ggplot2)

venture2007 = ventures %>% subset(year == 2020 & month == 7) %>% rename(fips=countycode)

qt2=quantile(venture2007$dv, probs=c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1), na.rm=T)
venture2007$dv1 =cut(venture2007$dv, breaks=qt2, labels=paste(qt2[-1]))

plot_usmap(regions = "counties", data = venture2007, values = "dv1", color="black") + 
  theme(legend.position = "right") + scale_fill_brewer(palette = "RdBu") 

##
## Venture distribution
##
plot(density(venture2007$dv),xlim=c(0,10),ylim=c(0,1.5), main="GoDaddy's Venture Density: # of Venture per 100 People",lty=1,lwd=2, col="blue")
points(density(venture2007$hadv), type="l", lty=1, lwd=5, col="red")  #lty is line style, lwd is line width
legend(5, 1.5, legend=c("Venture Density", "Highly Active Venture Density"), lty=c(2,5), col=c("blue","red"))
grid()

##
## Venture time series
##
vforward = panel18_2007 %>% subset(year_month > as.Date("2017-12-01") & year_month < as.Date("2020-08-01")) %>%
  select(countycode,year_month, nv, nhav, emp_hhold_S) %>% 
  mutate(nv = ifelse(is.na(nv),0,nv), ns = ifelse(is.na(nhav),0,nhav), emp_hhold_S= ifelse(is.na(emp_hhold_S),0,emp_hhold_S)) %>%
  group_by(year_month) %>% summarise_all(sum)

# nv plot
vforward %>% ggplot(aes(x=year_month, y=nv/1000000)) + geom_bar(stat="identity", fill="dark green") + 
  coord_cartesian(ylim = c(15,21)) +
  scale_y_continuous(name="# of Venture (Million)")

# emp plot
vforward %>% ggplot(aes(x=year_month, y=emp_hhold_S/1000)) + geom_bar(stat="identity", fill="red") +
  coord_cartesian(ylim = c(130,160)) +  
  scale_y_continuous(name= "Total Employment (Million)")

