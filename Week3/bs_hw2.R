################################################################### 
# H02
# Model China Real GDP growth rate
# Brad Spitzbart
# CSCI-E-116 09/30/2026
################################################################### 
setwd("C:/Users/brs416/Desktop/CSCI-E-116_bigdata/Week3/Data_and_Script_3")

library(readxl)  
library(dplyr)
library(stringr)
library(fixest)
library(corrplot)

#########################################
# Part I:
# clean data
# Linear Regression Analysis
#########################################
country_gdp =  read_xlsx("CountryData.xlsx")

# filter 2001-2019
work_gdp <- country_gdp %>% filter(year > 2000 & year < 2020)
# well, might not need to filter dates because lm drops missing values
work_gdp <- country_gdp

# Dependent Variables: rgdpg -- annual real GDP growth
# Eq1. Benchmark Equation: Pool OLS Model without China
eq1 = lm(rgdpg ~ econg + co2g + eximg, data = work_gdp %>% filter(country!="China"))
summary(eq1)
# Eq2. Pool OLS Model with China
eq2 = lm(rgdpg ~ econg + co2g + eximg, data = work_gdp)
summary(eq2)
# Eq3. Fixed Effect Model without China
eq3 = lm(rgdpg ~ econg + co2g + eximg + factor(country), data = work_gdp %>% filter(country!="China"))
summary(eq3)
# possible other method using fixest, won't use here
# eq3b = feols(rgdpg ~ econg + co2g + eximg | country, data = work_gdp %>% filter(country!="China"))
# summary(eq3b) # get roughly same coeffs, but summary does not show fixed factors
# Eq4. Fixed Effect Model with China
eq4 = lm(rgdpg ~ econg + co2g + eximg + factor(country), data = work_gdp)
summary(eq4)
# Eq5. Year Fixed Effect Model without China
eq5 = lm(rgdpg ~ econg + co2g + eximg + factor(year), data = work_gdp %>% filter(country!="China"))
summary(eq5)
# Eq6. Year Fixed Effect Model with China
eq6 = lm(rgdpg ~ econg + co2g + eximg + factor(year), data = work_gdp)
summary(eq6)
# Eq7. County and Year Fixed Effect Model without China
eq7 = lm(rgdpg ~ econg + co2g + eximg + factor(country) + factor(year), data = work_gdp %>% filter(country!="China"))
summary(eq7)
# Eq8. Country and Year Fixed Effect Model with China
eq8 = lm(rgdpg ~ econg + co2g + eximg + factor(country) + factor(year), data = work_gdp)
summary(eq8)
# Eq9. Time Trend Model without China
eq9 = lm(rgdpg ~ econg + co2g + year, data = work_gdp %>% filter(country!="China"))
summary(eq9)
# Eq10. Time Trend Model with China
eq10 = lm(rgdpg ~ econg + co2g + year, data = work_gdp)
summary(eq10)

#########################################
# Part II:
# make predictions
#########################################
fit1a = predict(eq1)
fit4a = predict(eq4)

#########################################
# Part III:
# add updated data
# Linear Regression Analysis
#########################################
update_gdp =  read_xlsx("CountryData_update.xlsx")
eq11 = lm(rgdpg ~ econg + co2g + eximg + hpg, data = update_gdp %>% filter(country!="China"))
summary(eq11)
eq12 = lm(rgdpg ~ econg + co2g + eximg, data = update_gdp %>% filter(country!="China"))
summary(eq12)

# predict hypothetical 2023 China rgdpg
est_econg = 4.4
est_co2g = 3.6
est_eximg = -6
est_hpg = -20
abeta <- coef(eq11)
est1 <- abeta[1] + abeta[2]*est_econg + abeta[3]*est_co2g + abeta[4]*est_eximg + abeta[5]*est_hpg

print("Estimated hypothetical China 2023 GDP growth rate:")
print(sprintf("Equation 11: %.2f%%", est1))
