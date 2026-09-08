################################################################### 
# H01b: Time Series Basics in R
# By William Yu, Harvard Extension
# 9/4/2023
################################################################### 
setwd("C:/Users/brs416/Desktop/CSCI-E-116_bigdata/Week1/Data and Script 1/")
###################################################################
# Package "quantmod" can import data directly from various sources
# FRED, google, yahoo, Qanda, MySQL
# Get xts (extensible time series) object
###################################################################
library(quantmod) 

##############################
# Quantmod: Get data from Fred (Federal Reserve Economic Data -- https://fred.stlouisfed.org/)
##############################
# "getSymbols" is the function to download the data
# Advance retail sales data, SA (Seasonally Adjusted)
getSymbols("RSAFS", src="FRED")     # Total Retail and Food Services
getSymbols("RSMVPD", src="FRED")    # Motor Vehicle and Parts Dealers
getSymbols("RSNSR", src="FRED")     # Nonstore Retailers --> E-Commerce !!
getSymbols("RSGCS", src="FRED")     # Grocery Stores
getSymbols("RSGMS", src="FRED")     # General Merchandise Stores
getSymbols("RSFSDP", src="FRED")    # Food Services & Drinking Places
getSymbols("RSBMGESD", src="FRED")  # Building Materials, Garden Equipment and Supplies Dealers
getSymbols("RSGASS", src="FRED")    # Gasoline Stations
getSymbols("RSHPCS", src="FRED")    # Health and Personal Care Stores
getSymbols("RSCCAS", src="FRED")    # Clothing and Clothing Accessory Stores
getSymbols("RSMSR", src="FRED")     # Miscellaneous Store Retailers
getSymbols("RSFHFS", src="FRED")    # Furniture and Home Furnishings Stores
getSymbols("RSDSELD", src="FRED")   # Department Stores
getSymbols("RSEAS", src="FRED")     # Electronics and Appliance Stores
getSymbols("RSSGHBMS", src="FRED")  # Sporting Goods, Hobby, Book, and Music Stores

plot(RSAFS)
plot(RSNSR)
plot(RSGCS)
plot(RSGMS)
plot(RSFSDP)
plot(RSGASS)
plot(RSDSELD)

table1 = cbind(RSAFS,RSMVPD,RSNSR,RSGCS,RSGMS,RSFSDP,RSBMGESD,RSGASS,RSHPCS,RSCCAS,RSMSR,RSFHFS,RSDSELD,RSEAS,RSSGHBMS)
period = c("2020-02-01","2022-12-01")
table2 = table1[period]
table3 = table1['2019/2022-12-01']
colnames(table3) = c("Total Retail & Food Services", "Motor Vehicle & Parts Dealers","E-Commerce",
                     "Grocery Stores","General Merchandise Stores","Food & Drinking Places",
                     "Building Materials & Supplies","Gasoline Stations","Health & Personal Care",
                     "Clothing Stores","Miscellaneous Stores","Furniture Stores",
                     "Department Stores","Electronics & Appliance","Sporting Goods, Hobby, Book")
table3a = t(table3)

# Frequency conversion
rs.qrt = to.quarterly(RSAFS)  # End of the quarter
rs.yr = to.yearly(RSAFS)      # End of the year
plot(rs.yr[,4], type="b")

#############################
# Quantmod: Get data from Yahoo Finance
#############################
getSymbols("^GSPC", src="yahoo")  
getSymbols("MSFT", src="yahoo")  
getSymbols("WMT", src="yahoo")  
getSymbols("BA", src="yahoo")  
getSymbols("BTC-USD", src="yahoo") 
getSymbols("CHIR", src="yahoo")  # MSCI China Real Estate ETF
barChart(GSPC)
barChart(GSPC['2022'])
barChart(WMT, theme="white")
barChart(`BTC-USD`)
lineChart(`CHIR`)
head(GSPC)
SP500P = Ad(GSPC); tail(SP500P)
SP500V = Vo(GSPC); head(SP500V)

etf = c( "SPY", # SPDR S&P 500 ETF Trust
         "IVV", # iShares Core S&P 500 ETF
         "VOO", # Vanguard S&P 500 ETF
         "VTI", # Vanguard Total Stock Market ETF
         "QQQ", # Invesco QQQ Trust
         "VTV", # Vanguard Value ETF
         "VEA", # Vanguard FTSE Developed Markets ETF
         "IEFA" # iShares Core MSCI EAFE ETF
         )
start_date = '2019-01-01'
end_date = '2022-12-31'

getSymbols(etf, src="yahoo", from=start_date,to=end_date)  

#############################
# Quantmod: Get data from MySQL
#############################
getSymbols(etf, src="MySQL",dbname=db, user=user, password=password, host=host)

###############################
# Get data from tseries package
###############################
library(tseries)                 

# Import the data from Yahoo finance directly: Change instrument for different stocks
sp500 = get.hist.quote(instrument="^gspc", start="1985-01-01", end="2022-12-01", quote="AdjClose", 
                       provider="yahoo", compression="m", retclass="zoo")

msft = get.hist.quote(instrument="msft", start="1985-01-01", end="2022-12-01", quote="AdjClose", 
                      provider="yahoo", compression="m", retclass="zoo")

wmt = get.hist.quote(instrument="wmt", start="1985-01-01", end="2022-12-01", quote="AdjClose", 
                      provider="yahoo", compression="m", retclass="zoo")

ba = get.hist.quote(instrument="ba", start="1985-01-01", end="2022-12-01", quote="AdjClose", 
                     provider="yahoo", compression="m", retclass="zoo")

par(mfrow=c(2,2))  # Make multiple chart display 2 rows by 2 columns
plot(sp500)
plot(msft)
plot(wmt)
plot(ba)
par(mfrow=c(1,1))  # Change chart setting back to the default

# Frequency Conversion: Monthly data to Quarterly data
# First, use "ts" function to convert the series to another form of time series
sp500.month = ts(sp500, start=c(1985,1), frequency=12)
class(sp500.month)
sp500.quarter = aggregate(sp500.month, nfrequency=4,mean) # Another way is sum
sp500.quarter

sp500.xts = as.xts(sp500)

# Generate continuously compounded returns from stock prices: log(Pt)-log(Pt-1)
sp500.r = diff(log(sp500))
msft.r = diff(log(msft))
wmt.r = diff(log(wmt))

plot(sp500.r,main="S&P 500 Stock Returns",xlab="Year",ylab="returns")  # main="" is for the title of the chart
abline(h=mean(sp500.r))  # horizontal line is the S&P500 return mean. v= for vertical line.
summary(sp500.r)

stocks = cbind(sp500.r,msft.r,wmt.r)            
colnames(stocks) = c("SP500", "MSFT", "WMT")    
plot(stocks, xlab="Monthly Returns")
colMeans(stocks, na.rm=T)   
summary(stocks)
stocks1=na.exclude(stocks)  # na.exclude() function returns the data with incomplete row removed.
                            # na.omit() function is the same as na.exclude

# Show smoothed histogram of the data
hist(sp500.r)
plot(density(sp500.r),xlim=c(-0.4,0.4), main="Smoothed Histogram of Monthly Stock Returns",lty=1)
points(density(msft.r), type="l", lty=1, lwd=5, col="red")  #lty is line style, lwd is line width
points(density(wmt.r), type="l", lty=1, lwd=2, col="blue")
legend(-0.4, 10, legend=c("S&P500", "MSFT","WMT"), lty=c(1,2,3), col=c("black","red","blue"))

##################################################################################################
## Introduction to lubridate
##################################################################################################  
library(tidyverse)
library(lubridate)
library(nycflights13)
library(ggplot2)
today()
now()             

# Convert character to POSIXct
ymd("2021-10-05")
mdy("January 31st, 2017")
dmy("31-Mar-2021")
ymd(20200704)
ymd_hms("2017-01-31 20:11:59") # UTC -- coordinated universal time
mdy_hm("01/31/2017 08:01")     
a = ymd(20170131, tz = "GMT")
a
class(a)  # POSIXct
weekdays(a)
wday(a)
week(a)
yday(a)     # number of days since the start of the year
julian(a)   # the number of days since day 0 on 1970-01-01
as.Date(36526, origin = "1899-12-30") # The timedate from an Excel file

flights = flights
str(flights)
flights %>% select(year, month, day, hour, minute)

flights %>% select(year, month, day, hour, minute) %>% 
  mutate(departure = make_datetime(year, month, day, hour, minute))

make_datetime_100 = function(year, month, day, time) {
  make_datetime(year, month, day, time %/% 100, time %% 100)
}

flights_dt = flights %>% 
  filter(!is.na(dep_time), !is.na(arr_time)) %>% 
  mutate(
    dep_time = make_datetime_100(year, month, day, dep_time),
    arr_time = make_datetime_100(year, month, day, arr_time),
    sched_dep_time = make_datetime_100(year, month, day, sched_dep_time),
    sched_arr_time = make_datetime_100(year, month, day, sched_arr_time)
  ) %>% 
  select(origin, dest, ends_with("delay"), ends_with("time"))

flights_dt

flights_dt %>% ggplot(aes(dep_time)) + geom_freqpoly(binwidth = 86400) # 86400 seconds = 1 day

flights_dt %>% filter(dep_time < ymd(20130102)) %>% 
  ggplot(aes(dep_time)) + geom_freqpoly(binwidth = 600) # 600 s = 10 minutes

flights_dt %>% filter(dep_time < ymd(20130102)) %>% 
  ggplot(aes(dep_time)) + geom_histogram(binwidth = 600) # 600 s = 10 minutes

## Disney World - Pirates of the Caribbean Ride Wait Time Analysis
disney = read.csv("W08h_disney_waitime.csv")
str(disney)
disney$datetime = mdy_hm(disney$datetime)
# disney$datetime = as.POSIXct(disney$datetime, format="%m/%d/%Y %H:%M")
str(disney)

disney = disney %>% subset(waittime!=-999)
ggplot(disney,aes(x=datetime,y=waittime)) + geom_line()

disney1 = disney %>% mutate(hour = make_datetime(year=year(datetime), month=month(datetime), day=day(datetime), hour=hour(datetime)),
                               year_month_day = make_date(year=year(datetime), month=month(datetime), day=day(datetime)),
                               year_month =  make_date(year=year(datetime), month=month(datetime)),
                               year = make_date(year=year(datetime))
                               )
                               
disney1 %>% group_by(year_month) %>% summarize(waittime=sum(waittime)) %>% 
  ggplot(aes(x=year_month,y=waittime)) + geom_bar(stat="identity", fill="red")

disney2 = xts(x=disney$waittime, order.by=disney$datetime)
disney3 = to.monthly(disney2) 
