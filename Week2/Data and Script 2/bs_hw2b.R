################################################################### 
# H02b: Cross-sectional Analysis
# Health in America Covid-19 Variation Research
# 
# Brad Spitzbart
# CSCI-E-116 09/23/2026
################################################################### 
setwd("C:/Users/brs416/Desktop/CSCI-E-116_bigdata/Week2/Data and Script 2/Health in America Covid19 Variation")
# Unzip the folder "Health in America Covid19 Variation" and keep all the data in the folder.

#########################################
# Part I:
# Import American Community Survey Data
#########################################
library(readxl)  
library(dplyr)
library(stringr)
library(tidyr)
library(lubridate)
library(tidycensus)
library(tidyverse)
library(viridis)
library(mapview)
library(corrplot)
options(tigris_use_cache = TRUE)

# About Tidycensus package:
# Reference: https://walkerke.github.io/tidycensus/articles/spatial-data.html
# Get API key from Census: http://api.census.gov/data/key_signup.html
# You only need to run the following three lines one time only. 
# census_api_key("f4ba8eb5e07e8ec803bad6778d0923895438b758", install = TRUE)
# readRenviron("~/.Renviron")
# Sys.getenv("CENSUS_API_KEY")
# Then activate the key in the email

# 2019 5 year ACS the list of variables: https://api.census.gov/data/2019/acs/acs5/profile/variables.html
acs19_5y = get_acs(geography = "county",
                   variables = c(sparent_m = "DP02_0007PE", sparent_f = "DP02_0011PE", hhage65a = "DP02_0013PE", hhsize = "DP02_0016E", 
                                 college_higher = "DP02_0068PE", ed_below9 = "DP02_0060PE", ed_g912 = "DP02_0061PE",ed_hs = "DP02_0062PE",
                                 ed_scollege = "DP02_0063PE",ed_associate = "DP02_0064PE",ed_bachelor = "DP02_0065PE",ed_higher = "DP02_0066PE",
                                 disable = "DP02_0072PE", disable65a = "DP02_0078PE", 
                                 fborn = "DP02_0093PE",fb_nonc = "DP02_0096PE", computer = "DP02_0152PE", broadband = "DP02_0153PE", 
                                 lcp = "DP03_0003PE", ur = "DP03_0009PE",
                                 commute_p = "DP03_0021PE", wfh = "DP03_0024PE", commute_ms = "DP03_0025E",
                                 eea_hhs = "DP03_0042PE", eea_lh = "DP03_0043PE", mincome = "DP03_0062E", 
                                 fstamp = "DP03_0074PE", 
                                 hi_pub = "DP03_0098PE", hi_no = "DP03_0099PE", hi_no_neea ="DP03_0113PE", hi_no_nlc ="DP03_0118PE",
                                 poverty ="DP03_0119PE",
                                 rentalu = "DP04_0047PE", rental_size = "DP04_0049E", mrent = "DP04_0134E", mhomeprice = "DP04_0089E", 
                                 pop = "DP05_0001E", mage ="DP05_0018E", a85a ="DP05_0017PE", a7584="DP05_0016PE",
                                 a6574 = "DP05_0015PE", a6064 ="DP05_0014PE", a5559 ="DP05_0013PE",a4554 ="DP05_0012PE",
                                 a3544 ="DP05_0011PE",a2534 ="DP05_0010PE",a2024 ="DP05_0009PE",a1519 ="DP05_0008PE",
                                 black ="DP05_0038PE", asian = "DP05_0044PE", aindian = "DP05_0039PE", latino = "DP05_0071PE"
                   ),                                        
                   survey = "acs5",
                   output = "wide",
                   year = 2019)   

acs19 = acs19_5y %>% dplyr::select(GEOID, NAME, sparent_m, sparent_f, hhage65a, hhsize, college_higher, disable, disable65a, 
                            fborn,fb_nonc, computer, broadband, lcp, ur,commute_p, commute_ms, wfh, eea_hhs, eea_lh, mincome, 
                            fstamp, hi_pub, hi_no, hi_no_neea, hi_no_nlc, poverty, rentalu, rental_size, mrent, mhomeprice, 
                            pop, mage, a85a, a7584, a6574, a6064, a5559, a4554,a3544,a2534, a2024,a1519,black, asian, aindian, latino) 

chci = acs19_5y %>% dplyr::select(GEOID, NAME, ed_below9, ed_g912,ed_hs, ed_scollege ,ed_associate,ed_bachelor,ed_higher)
chci = chci %>% mutate(chci=(1/100)*(50*ed_below9+100*ed_g912+120*ed_hs+130*ed_scollege+140*ed_associate+190*ed_bachelor+230*ed_higher))
acs19 = left_join(acs19, chci[,c("GEOID","chci")], by="GEOID")

acs19 = acs19 %>% mutate(sparent = sparent_m + sparent_f, a75a=a7584+a85a, a65a=a6574+a75a, a60a=a65a+a6064, a5564=a5559+a6064,
                         a3554=a3544+a4554, a2034=a2024+a2534, afford = mincome/mhomeprice) %>% rename(county=GEOID)

#########################################
# Part II:
# Linear Regression Analysis
#########################################
# Data source: https://usafacts.org/visualizations/coronavirus-covid-19-spread-map/
covid_case = read.csv("covid_cases_usafacts.csv") 
covid_death = read.csv("covid_deaths_usafacts_Jul2023.csv") # use new data
population = read.csv("population.csv") 
usland =  read_xlsx("US_land_area.xlsx") 

covidc_12321 = covid_case[,c(1:4, 372)]
names(covidc_12321)=c("county", "name","statename","stateid","case")
covidd_12321 = covid_death[,c(1, 372)]
names(covidd_12321)=c("county", "death")
covidc_53120 = covid_case[,c("countyFIPS","X5.31.2020")]
names(covidc_53120)=c("county", "case0520")
# get new deaths but leave variable names so as not to mess up 
# calculations below
#covidd_53120 = covid_death[,c("countyFIPS","X5.31.2020")]
covidd_53120 = covid_death[,c("countyFIPS","X7.23.23")] 
names(covidd_53120)=c("county", "death0520")

pop_12321 = population[,c(1, 4)]
names(pop_12321) = c("county","population")
us_land = usland[,c("STCOU", "LND110190D")]
names(us_land)=c("county", "area")

covidc_12321 = covidc_12321 %>% filter(county!=0)
covidd_12321 = covidd_12321 %>% filter(county!=0)
covidc_53120 = covidc_53120 %>% filter(county!=0)
covidd_53120 = covidd_53120 %>% filter(county!=0)

pop_12321 = pop_12321 %>% filter(county!=0)

us_land = us_land %>% mutate(county=as.numeric(county))

acs19 = acs19 %>% mutate(county=as.numeric(county))

new = left_join(covidc_12321, covidd_12321, by="county") %>% left_join(covidc_53120, by="county") %>% left_join(covidd_53120, by="county") %>%
  left_join(pop_12321, by="county") %>% left_join(us_land, by="county") %>% left_join(acs19, by="county")

# calculating population density
new = new %>% mutate(pdensity = population/area) %>% mutate(pdensity=gsub("Inf", NA, pdensity)) %>% mutate(pdensity=as.numeric(pdensity))
new = new %>% mutate(casep = 100*case/population, deathp = 1000000*death/population, casep520 = 100*case0520/population, 
                     deathp520 = 1000000*death0520/population) 

str(new)

# sparent_m, sparent_f, hhage65a, hhsize, college_higher, disable, disable65a, 
# fborn, fb_nonc, computer, broadband, lcp, ur,commute_p, commute_ms, wfh, eea_hhs, eea_lh, mincome, 
# fstamp, hi_pub, hi_no, hi_no_neea, hi_no_nlc, poverty, rentalu, rental_size, mrent, mhomeprice, 
# pop, mage, a85a, a7584, a6574, a6064, black, asian, aindian, latino) 
# a75a=a7584+a85a, a65a=a6574+a75a, a60a=a65a+a6064

## QCEW NAICS Data; https://www.bls.gov/cew/downloadable-data-files.htm
nic621111 = read.csv("nic621111.csv") # Offices of physicians
doctor = nic621111 %>% filter(own_code ==5) %>% select(area_fips, annual_avg_emplvl) %>% 
  rename(county=area_fips, doctor = annual_avg_emplvl) %>% mutate(county=as.numeric(county))
nic713940 = read.csv("nic713940.csv") # Gym
gym = nic713940 %>% filter(own_code ==5) %>% select(area_fips, annual_avg_emplvl) %>% 
  rename(county=area_fips, gym = annual_avg_emplvl) %>% mutate(county=as.numeric(county))
nic445110 = read.csv("nic445110.csv") # Grocery stores
grocery = nic445110 %>% filter(own_code ==5) %>% select(area_fips, annual_avg_emplvl) %>% 
  rename(county=area_fips, grocery = annual_avg_emplvl) %>% mutate(county=as.numeric(county))
nic623110 = read.csv("nic623110.csv") # Nursing care facilities
nursehome = nic623110 %>% filter(own_code ==5) %>% select(area_fips, annual_avg_emplvl) %>% 
  rename(county=area_fips, nursehome = annual_avg_emplvl) %>% mutate(county=as.numeric(county))
nic311612 = read.csv("nic311612.csv") # Meat packing factories
meatpack = nic311612 %>% filter(own_code ==5) %>% select(area_fips, annual_avg_emplvl) %>% 
  rename(county=area_fips, meatpack = annual_avg_emplvl) %>% mutate(county=as.numeric(county))
nic445310 = read.csv("nic445310.csv") # Liquor stores
liquor = nic445310 %>% filter(own_code ==5) %>% select(area_fips, annual_avg_emplvl) %>% 
  rename(county=area_fips, liquor = annual_avg_emplvl) %>% mutate(county=as.numeric(county))
nic481111 = read.csv("nic481111.csv") # Airport
airport = nic481111 %>% filter(own_code ==5) %>% select(area_fips, annual_avg_emplvl) %>% 
  rename(county=area_fips, airport = annual_avg_emplvl) %>% mutate(county=as.numeric(county))
nic7211 = read.csv("nic7211.csv") # Travel
travel = nic7211 %>% filter(own_code ==5) %>% select(area_fips, annual_avg_emplvl) %>% 
  rename(county=area_fips, travel = annual_avg_emplvl) %>% mutate(county=as.numeric(county))

## Election data
election = read.csv("countypres_2000-2016.csv") 
election1 = election %>% filter(year==2016, party=="democrat") %>% select(FIPS, candidatevotes, totalvotes) %>%
  mutate(demv = 100*candidatevotes/totalvotes) %>% rename(county=FIPS)

new1 = left_join(new, election1, by="county") %>% left_join(doctor, by="county") %>% left_join(gym, by="county") %>%
  left_join(grocery, by="county") %>% left_join(nursehome, by="county")  %>% left_join(meatpack, by="county") %>%
  left_join(liquor, by="county")  %>% left_join(airport, by="county") %>% left_join(travel, by="county")

# Replace missing value with zero
new1[,75:81][is.na(new1[,75:81])]=0

new1 = new1 %>% mutate(cfr=deathp/casep, p_doctor=100*doctor/population, p_gym=100*gym/population, p_grocery=100*grocery/population,
                       p_nursehome=100*nursehome/population, p_meatpack=100*meatpack/population, p_liquor=100*liquor/population,
                       p_airport=100*airport/population, p_travel=100*travel/population, hi_no_eea = 100 - hi_no_neea - hi_no_nlc)

## Health Data: 
# https://www.countyhealthrankings.org/explore-health-rankings/rankings-data-documentation
health = read_xlsx("2020 County Health Rankings Data.xlsx") 

colnames(health)
str(health)
health = health %>% mutate(county=as.numeric(county))

new2 = left_join(new1, health, by="county") 

colnames(new2)

# rerun model 1 and 5
#eq01 = lm(deathp ~ a85a + a7584 + a6574 + a5564 + a2034 + pdensity + pop + aindian + black + latino + asian + sparent + 
#            mincome + poverty + chci + lcp + ur + disable + hi_pub  + demv + hosp + vcrime +
#            commute_p + wfh + computer + p_nursehome + p_liquor + drinking + prematured + lowbirthw, data=new2)
# remove hosp and vcrime to match article
eq01 = lm(deathp ~ a85a + a7584 + a6574 + a5564 + a2034 + pdensity + pop + aindian + black + latino + asian + sparent + 
            mincome + poverty + chci + lcp + ur + disable + hi_pub  + demv +
            commute_p + wfh + computer + p_nursehome + p_liquor + drinking + prematured + lowbirthw, data=new2)
summary(eq01) 

eq02 = lm(casep ~ a85a + a7584 + a6574 + a5564 + a2034 + pdensity + pop + aindian + black + latino + asian + sparent + 
            mincome + poverty + chci + lcp + ur + disable + hi_pub  + demv + hosp + vcrime +
            commute_p + wfh + computer + p_nursehome + p_liquor + drinking + prematured + lowbirthw, data=new2)
summary(eq02) 

eq03 = lm(cfr ~ a85a + a7584 + a6574 + a5564 + a2034 + pdensity + pop + aindian + black + latino + asian + sparent + 
            mincome + poverty + chci + lcp + ur + disable + hi_pub  + demv + hosp + vcrime +
            commute_p + wfh + computer + p_nursehome + p_liquor + drinking + prematured + lowbirthw, data=new2)
summary(eq03) 

eq04 = lm(deathp ~ a85a + a7584 + a6574 + a5564 + a2034 + pdensity + pop + sparent + 
            mincome + poverty + chci + lcp + ur + disable + hi_pub  + demv +
            commute_p + wfh + computer + p_nursehome + p_liquor + drinking + prematured + lowbirthw, data=new2)
summary(eq04) 

eq05 = lm(deathp ~ a85a + a7584 + a6574 + a5564 + a2034 + pdensity + pop + aindian + black + latino + asian + sparent + 
            mincome + poverty + chci + lcp + ur + disable + hi_pub  + demv +
            commute_p + wfh + computer + p_nursehome + p_liquor + drinking + prematured + lowbirthw + statename, data=new2)
summary(eq05) 

eq06 = lm(deathp ~ casep + a85a + a7584 + a6574 + a5564 + a2034 + pdensity + pop + aindian + black + latino + asian + sparent + 
            mincome + poverty + chci + lcp + ur + disable + hi_pub  + demv +
            commute_p + wfh + computer + p_nursehome + p_liquor + drinking + prematured + lowbirthw, data=new2)
summary(eq06) 

eq07 = lm(deathp ~ deathp520 + a85a + a7584 + a6574 + a5564 + a2034 + pdensity + pop + aindian + black + latino + asian + sparent + 
            mincome + poverty + chci + lcp + ur + disable + hi_pub  + demv + deathp520 +
            commute_p + wfh + computer + p_nursehome + p_liquor + drinking + prematured + lowbirthw, data=new2)
summary(eq07) 

library(broom)
stat01 = tidy(eq01); stat02 = tidy(eq02); stat03 = tidy(eq03) 
stat04 = tidy(eq04); stat05 = tidy(eq05); stat06 = tidy(eq06); stat07 = tidy(eq07)

stat_all = rbind(stat01, NA, stat02, NA, stat03, NA, stat04, NA, stat05, NA, stat06, NA, stat07)
write.csv(stat_all, "bs_hw2b_stat_all.csv") 

##
## Correlation check
##
colnames(new2)
small=new2[,c("aindian", "black", "latino", "asian","deathp","casep","chci","mincome", "mhomeprice","mrent","sparent","cfr","poverty",
              "p_gym","p_grocery","p_doctor","p_liquor","hi_no","hi_no_eea","prematured","poorhealth","unhealthyd","obesity",
              "healthyfood", "pinactivity","exerciseo","drinking","smoking","lowbirthw","pcdoctor")]
cor1=cor(small, use="complete.obs")

write.csv(cor1,"bs_hw2b_cor1.csv")
write.csv(new2,"bs_hw2b_new2.csv")

#########################################
# Part III:
# Visualization
#########################################
library(ggplot2)
library(maps)
g_county = map_data("county")
g_state = map_data("state")
head(g_county)     # longitude/latitude data for county to plot the map with county name region (state) + subregion (county) (but no fips)
head(g_state)      # longitude/latitude data for state to plot the map
head(county.fips)  # fips with county name (no longitude/latitude)

# Merge the both by county name to have both columns of long/lat and fips
# First, merge region (state) and sub-region (county) into one column
county01 = g_county %>% mutate(polyname=paste(region, subregion, sep=",")) %>% left_join(county.fips, by="polyname")
head(county01)

new3 = new2
colnames(new3)[1]="fips"   # change the column name of "id" to "fips"
mapdata = county01 %>% left_join(new3, by="fips")
head(mapdata)

library(RColorBrewer)

# Generating a divergent color scheme by 10 quantiles
state_layer=geom_polygon(aes(long,lat,group=group), fill=NA, data=g_state, color = "black") 

# Death Rate
qt1=quantile(mapdata$deathp, probs=c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1), na.rm=T)
mapdata$deathpa =cut(mapdata$deathp, breaks=qt1, labels=paste(qt1[-1]))
ggplot(mapdata, aes(long,lat,group=group)) + 
  geom_polygon(aes(fill = deathpa), colour = rgb(1,1,1,0.2)) + coord_quickmap() +
  scale_fill_brewer(palette = "RdBu", direction=-1) + state_layer +
  scale_y_continuous(name = "") + scale_x_continuous(name = "") 

# Case Rate
qt1=quantile(mapdata$casep, probs=c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1), na.rm=T)
mapdata$casepa =cut(mapdata$casep, breaks=qt1, labels=paste(qt1[-1]))
ggplot(mapdata, aes(long,lat,group=group)) + 
  geom_polygon(aes(fill = casepa), colour = rgb(1,1,1,0.2)) + coord_quickmap() +
  scale_fill_brewer(palette = "RdBu", direction=-1) + state_layer +
  scale_y_continuous(name = "") + scale_x_continuous(name = "") 

# CFR
qt1=quantile(mapdata$cfr, probs=c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1), na.rm=T)
mapdata$cfra =cut(mapdata$cfr, breaks=qt1, labels=paste(qt1[-1]))
ggplot(mapdata, aes(long,lat,group=group)) + 
  geom_polygon(aes(fill = cfra), colour = rgb(1,1,1,0.2)) + coord_quickmap() +
  scale_fill_brewer(palette = "RdBu", direction=-1) + state_layer +
  scale_y_continuous(name = "") + scale_x_continuous(name = "") 
