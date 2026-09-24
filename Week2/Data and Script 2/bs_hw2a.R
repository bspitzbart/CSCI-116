###########################################################################################
# H02a. Panel Data Regression
# Brad Spitzbart
# CSCI-E-116 09/23/2026
###########################################################################################
setwd("C:/Users/brs416/Desktop/CSCI-E-116_bigdata/Week2/Data and Script 2")
library(readxl) 
#library(writexl)  
library(corrplot) 
library(dplyr)
#library(forecast)  

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
# the fits match those produced in the article.

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

# Try some other variables
par(mfrow = c(2, 3))
plot(corp_tax$ctax, corp_tax$ypcg)
plot(corp_tax$ypc2000, corp_tax$ypcg)
plot(corp_tax$dty, corp_tax$ypcg)
plot(corp_tax$trade, corp_tax$ypcg)
plot(corp_tax$ihc, corp_tax$ypcg)
plot(corp_tax$y2000, corp_tax$ypcg)

eq4 = lm(ypcg ~ ctax + ypc2000 + dty + trade*ypc2000, data = corp_tax)
summary(eq4)