#####################################################################################################
# H03
# Brad Spitzbart
# CSCI-E-116 10/07/2026
# modified from:
# H03c: Introduction to Facebook Prophet 
# By William Yu, UCLA Anderson Forecast
# 9/23/2024
# R Reference: https://facebook.github.io/prophet/docs/quick_start.html#r-api
# Python Reference: https://facebook.github.io/prophet/docs/quick_start.html#python-api
# If you want to use Prophet in Python, you need to install older version:
# pip install pystan == 2.19.1.1
# pip install fbprophet == 0.7.1
##################################################################################################### 
library(data.table)
library(dplyr)
library(ggplot2)
library(prophet)
library(quantmod) 
library(lubridate)
library(readxl)

setwd("C:/Users/brs416/Desktop/CSCI-E-116_bigdata/Week3/Data_and_Script_3")

##
## TSA Air Travel
##
tsa = read_excel("W09e_tsa.xlsx") 
tsa_test = read_excel("W09e_tsa.xlsx", sheet="test") 

tsa %>% ggplot() +
  geom_line(aes(x=ds, y=travel_train/1000000), color="blue") +
  ylab('# of daily air travelers (Million)') 

tsa = tsa %>% rename(y=travel_train)
m11 = prophet(tsa)
future = make_future_dataframe(m11, periods = 81) # Size of testset
tail(future)
forecast11 = predict(m11, future)
tail(forecast11[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')])
plot(m11, forecast11)
prophet_plot_components(m11, forecast11)

forecast11a = subset(forecast11, ds>=as.Date("2024-07-01"))
rmse11 = sqrt(mean((forecast11a$yhat-tsa_test$travel_test)^2))

ggplot() +
  geom_line(data=forecast11a, aes(x=ds, y=yhat), color="blue") +  
  geom_line(data=tsa_test, aes(x=ds, y=travel_test), color="red")

# add holiday for covid
covid = data_frame(
  holiday = 'covid',
  ds = seq(as.Date('2020-3-15'), to=as.Date('2022-3-1'), by='days'),
  lower_window = 0,
  upper_window = 1
)
m11cov = prophet(tsa, holidays=covid)
future_cov = make_future_dataframe(m11cov, periods = 81) # Size of testset
tail(future_cov)
forecast11cov = predict(m11cov, future_cov)
tail(forecast11cov[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')])
plot(m11cov, forecast11cov)
prophet_plot_components(m11cov, forecast11cov)

forecast11cova = subset(forecast11cov, ds>=as.Date("2024-07-01"))
rmse11cov = sqrt(mean((forecast11cova$yhat-tsa_test$travel_test)^2))

# add US holidays
m11hol = prophet(holidays=covid)
m11hol = add_country_holidays(m11hol, country_name = 'US')
m11hol = fit.prophet(m11hol, tsa)
future_hol = make_future_dataframe(m11hol, periods = 81) # Size of testset
tail(future_hol)
forecast11hol = predict(m11hol, future_hol)
tail(forecast11hol[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')])
plot(m11hol, forecast11hol)
prophet_plot_components(m11hol, forecast11hol)

forecast11hola = subset(forecast11hol, ds>=as.Date("2024-07-01"))
rmse11hol = sqrt(mean((forecast11hola$yhat-tsa_test$travel_test)^2))

ggplot() +
geom_line(data=forecast11hola, aes(x=ds, y=yhat), color="blue") +  
geom_line(data=tsa_test, aes(x=ds, y=travel_test), color="red") 

# Long-term forecast
m12 = prophet(tsa)
future = make_future_dataframe(m12, periods = 365)
tail(future)
forecast12 = predict(m12, future)
tail(forecast12[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')])
plot(m12, forecast12)
prophet_plot_components(m12, forecast12)

forecast12a = subset(forecast12, ds>=as.Date("2024-07-01"))

ggplot() +
  geom_line(data=forecast12a, aes(x=ds, y=yhat), color="blue") +  
  geom_line(data=tsa_test, aes(x=ds, y=travel_test), color="red")

# add US holidays
m12hol = prophet(holidays=covid)
m12hol = add_country_holidays(m12hol, country_name = 'US')
m12hol = fit.prophet(m12hol, tsa)
future_hol = make_future_dataframe(m12hol, periods = 365) # Size of testset
tail(future_hol)
forecast12hol = predict(m12hol, future_hol)
tail(forecast12hol[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')])
plot(m12hol, forecast12hol)

forecast12hola = subset(forecast12hol, ds>=as.Date("2024-07-01"))
ggplot() +
  geom_line(data=forecast12hola, aes(x=ds, y=yhat), color="blue") +  
  geom_line(data=tsa_test, aes(x=ds, y=travel_test), color="red")

## Keep this for later
## Note: Web data scraping via R
##

# Highlight the website's data, usually in the beginning of a table. Right click the mouse and select "Inspect",
# Find the "<table" and click it and then right click the mouse and select "copy" 
# and choose "copy Xpath" and paste it into html_nodes(xpath=' ')

#library(rvest)
#url = 'https://www.tsa.gov/travel/passenger-volumes'
#webdata = url %>% read_html() %>%
#  html_nodes(xpath = '//*[@id="block-mainpagecontent"]/div/div/div/div/div/div[2]/div/div/div/div/div[2]/table') %>%
#  html_table(fill = TRUE)
#df1 = webdata[[1]]
#write.csv(df1,"tsa.csv")
