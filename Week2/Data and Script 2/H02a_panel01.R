###########################################################################################
# H02a. Panel Data Regression
# By William Yu, UCLA Anderson Forecast
# 9/12/2023
# https://www.jstatsoft.org/article/view/v027i02
# https://cran.r-project.org/web/packages/plm/plm.pdf
###########################################################################################
setwd("C:/Users/wiyu/documents/zip08/2023 Q4 Fall_Harvard/Data/")
library(readxl)  
library(forecast)  

##
## Simulations of Fixed effect DGP biased estimated by OLS
##
x1 = rnorm(60, mean=5, sd=3)
y1 = 7 + 0.8*x1 + rnorm(60, mean=0, sd=1)
x2 = rnorm(60, mean=10, sd=3)
y2 = 13 + 0.8*x2 + rnorm(60, mean=0, sd=1)
x3 = rnorm(60, mean=15, sd=3)
y3 = 20 + 0.8*x3 + rnorm(60, mean=0, sd=1)
x = c(x1, x2, x3)
y = c(y1, y2, y3)
ols = lm(y~x); summary(ols)
plot(x,y, pch=20); abline(lm(y ~ x), col="red")

x1 = rnorm(60, mean=15, sd=3)
y1 = 7 + 0.8*x1 + rnorm(60, mean=0, sd=1)
x2 = rnorm(60, mean=10, sd=3)
y2 = 13 + 0.8*x2 + rnorm(60, mean=0, sd=1)
x3 = rnorm(60, mean=5, sd=3)
y3 = 20 + 0.8*x3 + rnorm(60, mean=0, sd=1)
x = c(x1, x2, x3)
y = c(y1, y2, y3)
ols = lm(y~x); summary(ols)
plot(x,y, pch=20); abline(lm(y ~ x), col="red")

##
## Election data
##
election = read_excel("W01c_election.xlsx")  
str(election)

plot(election$Nonwhite, election$Dvote, pch=20)
plotmeans(Dvote~state, data=election)
plotmeans(Dvote~state, data=election, p=0.8)
plotmeans(Dvote~year, data=election)

# Dvote: % of votes for Democrat presidential candidates
# Nonwhite: % of residents who are nonwhite
# Christian: % of residents who are evangelical Christians
# Mhincome: Median household income (level data)
# Mhincomeg: Median household income growth rate

# Simple OLS
election12 = subset(election, year==2012)
fit01 = lm(Dvote ~ Nonwhite + Christian + Mhincome, data = election12)
summary(fit01)

# Pooled OLS (Across multiple years)
fit02 = lm(Dvote ~ Nonwhite + Christian + Mhincome + Mhincomeg, data=election)
summary(fit02)
checkresiduals(fit02) 

# Regional Fixed Effect
fit03 = lm(Dvote ~ state + Nonwhite + Christian + Mhincome , data = election)
summary(fit03)
checkresiduals(fit03) 

# Regional plus Time Fixed Effect
fit04 = lm(Dvote ~ state + factor(year) + Nonwhite + Christian + Mhincome , data = election)
summary(fit04)
checkresiduals(fit04) 

fit05 = lm(Dvote ~ 0 + state + factor(year) + Nonwhite + Christian + Mhincome , data = election)
summary(fit05)

fit06 = lm(Dvote ~ -1 + state + factor(year) + Nonwhite + Christian + Mhincome , data = election)
summary(fit06)