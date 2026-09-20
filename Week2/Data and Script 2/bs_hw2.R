###########################################################################################
# H02a. Panel Data Regression
# Brad Spitzbart
# CSCI-E-116 09/23/2026
###########################################################################################
setwd("C:/Users/brs416/Desktop/CSCI-E-116_bigdata/Week2/Data and Script 2")
library(readxl) 
library(writexl)  
library(corrplot) 
library(dplyr)
library(forecast)  

corp_tax = read_excel("P02_Corporate tax.xlsx")
# look at data
summary(corp_tax)
str(corp_tax)
corrplot(cor(corp_tax[,-1]), type="lower", method="number") # remove the 1st column because it is character

# reproduce fits from article
eq1 = lm(ypcg ~ ctax, data = corp_tax)
summary(eq1)
checkresiduals(eq1)
eq2 = lm(ypcg ~ ctax + ypc2000, data = corp_tax)
summary(eq2)
checkresiduals(eq2)
eq3 = lm(ypcg ~ ctax + ypc2000 + dty + ctax*dty, data = corp_tax)
summary(eq3)
checkresiduals(eq3)

# predict hypothetical ypcg
est_ctax = 20
est_ypc2000 = 10000
est_dty = 35
abeta1 <- coef(eq1)
est1 <- abeta1[1] + abeta1[2]*est_ctax
abeta2 <- coef(eq2)
est2 <- abeta2[1] + abeta2[2]*est_ctax + abeta2[3]*est_ypc2000
abeta3 <- coef(eq3)
est3 <- abeta3[1] + abeta3[2]*est_ctax + abeta3[3]*est_ypc2000 + abeta3[4]*est_dty + abeta3[5]*est_ctax*est_dty
print("Predicted hypothetical GDP per capital growth rate:")
print(sprintf("Equation 1: %.2f%%", est1))
print(sprintf("Equation 2: %.2f%%", est2))
print(sprintf("Equation 3: %.2f%%", est3))

# reproduce figure 4 from article
par(pty = "s")
plot(corp_tax$ctax, corp_tax$ypcg, pch=16, col="blue", 
     main="Figure 4", 
     xlab="Average Corporate Tax Rate '00-'08 (%)", 
     ylab="Average GDP per capita growth '00-'15",
     xlim=c(10,45), ylim=c(-1,6), yaxt="n")
grid()
ticks <- axTicks(2)
axis(2, at = ticks, labels = paste0(ticks, "%"), las = 1)
abline(eq1, col="red")
abline(h = 0)
text(15.2,2.8,"IRL",pos=2,cex=0.6)
text(18,5,"LVA",pos=4,cex=0.6)
text(39,1.3,"UK",pos=4,cex=0.6)
text(38.5,1,"USA",pos=4,cex=0.6)
text(39.5,0.6,"JPN",pos=4,cex=0.6)
text(37.5,-0.5,"ITY",pos=4,cex=0.6)

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