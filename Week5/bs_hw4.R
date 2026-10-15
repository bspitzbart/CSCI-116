#####################################################################################################
# H04
# Brad Spitzbart
# CSCI-E-116 10/14/2026
##################################################################################################### 
library(data.table)
library(dplyr)
library(prophet)
library(tseries)
library(fpp2)
library(ggplot2)
library(xts)
library(fredr)
library(zoo)
library(corrplot)

setwd("C:/Users/brs416/Desktop/CSCI-E-116_bigdata/Week5/Data_and_Script_5")

fredr_set_key("491105751b9f9553cde56be27af8dd4f") # This is my key. Don't use it. Use your own.
x = fredr_series_observations("TOTALSA")
TOTALSA = xts(x$value, order.by=x$date) # Total vehicles sales

x = fredr_series_observations("PAYEMS") # US payroll jobs
PAYEMS = xts(x$value, order.by=x$date)
x = fredr_series_observations("DFF") # Federal fund rates
DFF = xts(x$value, order.by=x$date)
x = fredr_series_observations("UNRATE") # Unemployment rates
UNRATE = xts(x$value, order.by=x$date)
x = fredr_series_observations("NASDAQCOM") # Stock market prices
NASDAQCOM = xts(x$value, order.by=x$date)
x = fredr_series_observations("USSTHPI") # Housing market prices
USSTHPI = xts(x$value, order.by=x$date)
x = fredr_series_observations("CUSR0000SETB01") # Gasoline price in CPI
GPRICE = xts(x$value, order.by=x$date)

# look into using library or API to get series as time series in bulk with 
# matching time frame and frequencies, for now as an exercise we'll do manually here.

plot(TOTALSM)
par(mfrow = c(2, 3))
# original before converting to ts
#plot(PAYEMS$date, PAYEMS$value)
#plot(DFF$date, DFF$value)
#plot(UNRATE$date, UNRATE$value)
#plot(NASDAQCOM$date, NASDAQCOM$value)
#plot(USSTHPI$date, USSTHPI$value)
#plot(GPRICE$date, GPRICE$value)
# new way after assigning xts - actually other plot looks better, but we need ts.
plot(PAYEMS)
plot(DFF)
plot(UNRATE)
plot(NASDAQCOM)
plot(USSTHPI)
plot(GPRICE)

# combine into dataframe - from GPT 5.6 Luna
# Helper functions
safe_mean <- function(z) {
  if (all(is.na(z))) NA_real_ else mean(z, na.rm = TRUE)
}

safe_last <- function(z) {
  z <- z[!is.na(z)]
  if (length(z) == 0) NA_real_ else as.numeric(tail(z, 1))
}

# Aggregate to quarterly frequency
quarterly_mean <- function(z) {
  y <- period.apply(
    z,
    INDEX = endpoints(z, on = "quarters"),
    FUN = safe_mean
  )
  
  index(y) <- as.Date(as.yearqtr(index(y)), frac = 1)
  y
}

quarterly_last <- function(z) {
  y <- period.apply(
    z,
    INDEX = endpoints(z, on = "quarters"),
    FUN = safe_last
  )
  
  index(y) <- as.Date(as.yearqtr(index(y)), frac = 1)
  y
}

# Quarterly transformations
# Average values for rates, employment, unemployment, and gasoline prices
TOTALSA_q <- quarterly_mean(TOTALSA)
PAYEMS_q <- quarterly_mean(PAYEMS)
DFF_q    <- quarterly_mean(DFF)
UNRATE_q <- quarterly_mean(UNRATE)
GPRICE_q <- quarterly_mean(GPRICE)

# End-of-quarter values for market-price series
NASDAQCOM_q <- quarterly_last(NASDAQCOM)
USSTHPI_q   <- quarterly_last(USSTHPI)

# Merge the quarterly series
quarterly_xts <- merge(
  TOTALSA_q,
  PAYEMS_q,
  DFF_q,
  UNRATE_q,
  NASDAQCOM_q,
  USSTHPI_q,
  GPRICE_q,
  all = TRUE
)

# Create the complete quarterly date sequence:
# 1972 Q1 through 2024 Q2
quarterly_dates <- as.Date(
  as.yearqtr(seq(
    as.yearqtr("1976 Q1"),
    as.yearqtr("2024 Q2"),
    by = 0.25
  )),
  frac = 1
)

# Convert the xts object to a data frame
quarterly_values <- data.frame(
  date = as.Date(index(quarterly_xts)),
  coredata(quarterly_xts)
)

quarterly_df <- merge(
  data.frame(date = quarterly_dates),
  quarterly_values,
  by = "date",
  all.x = TRUE,
  sort = TRUE
)

# Optional: use date as row names
rownames(quarterly_df) <- quarterly_df$date

quarterly_df

# Select only numeric columns
numeric_data <- quarterly_df[, vapply(quarterly_df, is.numeric, logical(1))]

# Correlation matrix
cor_matrix <- cor(
  numeric_data,
  use = "pairwise.complete.obs",
  method = "pearson"
)

# Plot correlation matrix
corrplot(
  cor_matrix,
  method = "color",
  type = "upper",
  order = "hclust",
  addCoef.col = "black",
  tl.col = "black",
  tl.srt = 45,
  number.cex = 0.7,
  diag = FALSE)

# let's try to make all series stationary based on plots
quarterly_df <- quarterly_df %>%arrange(date)
stationary_df <- data.frame(
  date = quarterly_df$date[-1],
  TOTALSA = diff(log(quarterly_df$TOTALSA)),
  PAYEMS = diff(log(quarterly_df$PAYEMS)),
  DFF = diff(quarterly_df$DFF),
  UNRATE = diff(quarterly_df$UNRATE),
  NASDAQCOM = diff(log(quarterly_df$NASDAQCOM)),
  USSTHPI = diff(log(quarterly_df$USSTHPI)),
  GPRICE = diff(log(quarterly_df$GPRICE))
)
stationary_df <- stationary_df[complete.cases(stationary_df),]

par(mfrow = c(2, 3))
plot(stationary_df$PAYEMS)
plot(stationary_df$DFF)
plot(stationary_df$UNRATE)
plot(stationary_df$NASDAQCOM)
plot(stationary_df$USSTHPI)
plot(stationary_df$GPRICE)
# looks pretty good
numeric_data <- stationary_df[, vapply(stationary_df, is.numeric, logical(1))]

# Correlation matrix
cor_matrix <- cor(
  numeric_data,
  use = "pairwise.complete.obs",
  method = "pearson"
)

# Plot correlation matrix
corrplot(
  cor_matrix,
  method = "color",
  type = "upper",
  order = "hclust",
  addCoef.col = "black",
  tl.col = "black",
  tl.srt = 45,
  number.cex = 0.7,
  diag = FALSE)

#PAYEMS     = quarterly payroll growth
#DFF        = change in federal funds rate
#UNRATE     = change in unemployment rate
#NASDAQCOM  = quarterly stock-market return
#USSTHPI    = quarterly housing-price growth
#GPRICE     = quarterly gasoline-price growth

# start training
# Select the estimation period and remove missing values
model_data <- stationary_df %>%
  mutate(quarter = as.yearqtr(date)) %>%
  filter(
    quarter >= as.yearqtr("1976 Q2"),
    quarter <= as.yearqtr("2024 Q2")
  ) %>%
  select(
    date,
    TOTALSA,
    PAYEMS,
    DFF,
    UNRATE,
    NASDAQCOM,
    USSTHPI,
    GPRICE
  ) %>%
  na.omit()

# OLS structural model
model1 <- lm(
  TOTALSA ~ PAYEMS + DFF + UNRATE +
    NASDAQCOM + USSTHPI + GPRICE,
  data = model_data)

# Display results
summary(model1)

model2 <- lm(TOTALSA ~ UNRATE,data = model_data)
summary(model2a)

model2i=auto.arima(UNRATE)
for2i = forecast(model2i,24)  
for2i
plot(for2i)
abline(h=0)
grid()

# test ARMA for TOTALSA
model2ai=auto.arima(TOTALSA)
for2ai = forecast(model2ai,24)  
for2ai
plot(for2ai)
abline(h=0)
grid()