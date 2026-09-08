################################################################### 
# H01: Data Generating Process and Linear Regression
# By William Yu, Harvard Extension
# 9/4/2023
################################################################### 
setwd("C:/Users/wiyu/documents/Zip08/2023 Q4 Fall_Harvard/Data/")

# install.packages("readxl")
# install.packages("writexl")
# install.packages("corrplot")
# install.packages("dplyr")
library(readxl)  
library(writexl)  
library(corrplot) 
library(dplyr)

##
## Signal to Noise Ratio Simulation/Data Generating Process/Monte Carlo Simulation
##

## High signal-to-noise ratio 
set.seed(3)
x1=rnorm(60, mean=10, sd=3)
y1=7+0.8*x1+rnorm(60, mean=0, sd=1)

fit1=lm(y1~x1)
summary(fit1)

## Low signal-to-noise ratio 
set.seed(3)
x2=rnorm(60, mean=10, sd=3)
y2=7+0.8*x2+rnorm(60, mean=0, sd=6)

fit2=lm(y2~x2)
summary(fit2)

par(mfrow=c(1,2))
plot(x1,y1,pch=20, main="True Beta=0.8, Estiamted Beta=0.7"); abline(fit1, col="blue")
plot(x2,y2,pch=20, main="True Beta=0.8, Estiamted Beta=0.4"); abline(fit2, col="red")
par(mfrow=c(1,1))

##
## Linear Regression Example
##

## Example 1: Predicting Homeless
homeless = read_excel("W01b_homeless.xlsx") 
summary(homeless)
str(homeless)
corrplot(cor(homeless[,-1]), type="lower", method="number") # remove the 1st column because it is character

# Univariate regression / simple correlation
fit03 = lm(hl ~ hv, data=homeless)
summary(fit03)

plot(homeless$hv, homeless$hl, col="blue", lwd=2, xlab="Median home value", ylab="Homeless rate")     # Correlation
abline(lm(hl ~ hv, data=homeless), col="red", lwd=4)        

# Multivariate regression / partial correlation
fit04 = lm(hl ~ hv+rent+mhi+hsg+density, data=homeless)
summary(fit04) # Equation 1 in the article (Yu 2018)

pred04 = predict(fit04)     # In-sample prediction
plot(pred04, homeless$hl)   # Compare model prediction (y hat) and true y
grid()

fit04$coefficients
fit04$coefficients[[1]]
fit04$coef[2]
stat01 = coef(summary(fit04))[, c("Estimate","t value")] %>% as.data.frame()
# stat01 = as.data.frame(coef(summary(fit04))[, c("Estimate","t value")])

outputDir = "C:/Users/wiyu/documents/Zip08/2023 Q4 Fall_Harvard/Output/"
write_xlsx(stat01, paste0(outputDir, "stat01.xlsx"))

fit05 = lm(hl ~ hv+rent+mhi+hsg+density+tem+year+y2017, data=homeless)
summary(fit05)  # Identify multicollinearity problem

## Example II: Predicting Carseat Sales

Carseats = read.csv("Y02_carseats.csv")
str(Carseats)
table(Carseats$ShelveLoc)
table(Carseats$Urban)
table(Carseats$US)

# R can handle categorical variables directly
fit06=lm(Sales~.,data=Carseats)
summary(fit06)

