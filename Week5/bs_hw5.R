################################################################### 
# H05
# Sales vs. Marketing Channels
# Brad Spitzbart
# CSCI-E-116 10/21/2026
################################################################### 
setwd("C:/Users/brs416/Desktop/CSCI-E-116_bigdata/Week6")

library(readxl)  
library(dplyr)
library(stringr)
library(fixest)
library(corrplot)
library(prophet)
library(forecast)
library(tseries)
library(xts)
library(quantmod)

#########################################
# Part I:
# examine data
#########################################
tv =  read_xlsx("TVlift.xlsx")

str(tv)
head(tv)

plot(tv$sales)
# looks like there is some weekly variation, let's see what prophet says
dates <- as.Date(tv$date, format = "%m/%d/%Y")

# Prepare Prophet input: ds = date, y = sales
prophet_data <- data.frame(
  ds = dates,
  y = as.numeric(tv$sales)
)

# Remove incomplete rows and sort by date
prophet_data <- prophet_data[complete.cases(prophet_data), ]
prophet_data <- prophet_data[order(prophet_data$ds), ]

# Fit the model
model <- prophet::prophet(prophet_data)

future <- prophet::make_future_dataframe(
  model,
  periods = 30,
  freq = "day"
)
forecast <- predict(model, future)

# Plot the forecast and its components
plot(model, forecast)
prophet::prophet_plot_components(model, forecast)

# so tues and wed are the low sales days
# move on to regressions
cutoff <- as.Date("2017-03-01")
train <- tv[tv$date < cutoff, ]
test  <- tv[tv$date >= cutoff, ]

# Fit multiple linear regression
model <- lm(sales ~ sb + snb + tv, data = train)

# Model summary and test-set predictions
print(summary(model))

test$predicted_sales <- predict(model, newdata = test)
print(test[, c("date", "sales", "predicted_sales")])

# Test-set error metrics
mae  <- mean(abs(test$sales - test$predicted_sales))
rmse <- sqrt(mean((test$sales - test$predicted_sales)^2))

cat("Test MAE:", mae, "\n")
cat("Test RMSE:", rmse, "\n")

# Plot actual vs. predicted sales for the test period
plot(
  test$date, test$sales,
  type = "l", lwd = 2, col = "black",
  xlab = "Date", ylab = "Sales",
  main = "Actual vs. Predicted Sales"
)
lines(test$date, test$predicted_sales, lwd = 2, col = "blue")
legend(
  "topleft",
  legend = c("Actual", "Predicted"),
  col = c("black", "blue"),
  lty = 1, lwd = 2
)

# look for lag
# Keep rows with valid date, sales, and tv values
plot_data <- model_data[complete.cases(model_data[, c("date", "sales", "tv")]), ]

# Scale TV values to the sales axis for drawing
tv_range <- range(plot_data$tv)
sales_range <- range(plot_data$sales)

if (diff(tv_range) == 0) {
  tv_scaled <- rep(mean(sales_range), nrow(plot_data))
} else {
  tv_scaled <- (plot_data$tv - tv_range[1]) / diff(tv_range) * diff(sales_range) +
    sales_range[1]
}

plot(
  plot_data$date, plot_data$sales,
  type = "l", col = "black", lwd = 2,
  xlab = "Date", ylab = "Sales",
  main = "Sales and TV Over Time"
)
lines(plot_data$date, tv_scaled, col = "blue", lwd = 2)

# Map the right axis back to the original TV values
axis(
  side = 4,
  at = seq(sales_range[1], sales_range[2], length.out = 5),
  labels = round(seq(tv_range[1], tv_range[2], length.out = 5))
)
mtext("TV", side = 4, line = 3)

legend(
  "topleft",
  legend = c("Sales", "TV"),
  col = c("black", "blue"),
  lty = 1, lwd = 2
)

# Keep rows with valid date, sales, and sb values
plot_data <- tv[complete.cases(tv[, c("date", "sales", "sb")]), ]

# Scale TV values to the sales axis for drawing
sb_range <- range(plot_data$sb)
sales_range <- range(plot_data$sales)

if (diff(sb_range) == 0) {
  sb_scaled <- rep(mean(sales_range), nrow(plot_data))
} else {
  sb_scaled <- (plot_data$sb - sb_range[1]) / diff(sb_range) * diff(sales_range) +
    sales_range[1]
}

plot(
  plot_data$date, plot_data$sales,
  type = "l", col = "black", lwd = 2,
  xlab = "Date", ylab = "Sales",
  main = "Sales and SB Over Time"
)
lines(plot_data$date, sb_scaled, col = "blue", lwd = 2)

# Map the right axis back to the original TV values
axis(
  side = 4,
  at = seq(sales_range[1], sales_range[2], length.out = 5),
  labels = round(seq(sb_range[1], sb_range[2], length.out = 5))
)
mtext("SB", side = 4, line = 3)

legend(
  "topleft",
  legend = c("Sales", "SB"),
  col = c("black", "blue"),
  lty = 1, lwd = 2
)
# Keep rows with valid date, sales, and snb values
plot_data <- tv[complete.cases(tv[, c("date", "sales", "snb")]), ]

# Scale TV values to the sales axis for drawing
snb_range <- range(plot_data$snb)
sales_range <- range(plot_data$sales)

if (diff(snb_range) == 0) {
  snb_scaled <- rep(mean(sales_range), nrow(plot_data))
} else {
  snb_scaled <- (plot_data$snb - snb_range[1]) / diff(snb_range) * diff(sales_range) +
    sales_range[1]
}

plot(
  plot_data$date, plot_data$sales,
  type = "l", col = "black", lwd = 2,
  xlab = "Date", ylab = "Sales",
  main = "Sales and SNB Over Time"
)
lines(plot_data$date, snb_scaled, col = "blue", lwd = 2)

# Map the right axis back to the original TV values
axis(
  side = 4,
  at = seq(sales_range[1], sales_range[2], length.out = 5),
  labels = round(seq(snb_range[1], snb_range[2], length.out = 5))
)
mtext("TV", side = 4, line = 3)

legend(
  "topleft",
  legend = c("Sales", "SNB"),
  col = c("black", "blue"),
  lty = 1, lwd = 2
)

# build a lag model
# first include past sales in lag
# ----> it is not interpreting date right to add +k, I have messed up date_num
# Sort the data by date
model_data <- tv[order(tv$date), ]
#
## Add a numeric date trend, if it isn't already present
#model_data$date_num <- as.numeric(
  #model_data$date - min(model_data$date, na.rm = TRUE)
#)
#
## Create calendar-day lags
#lag_vars <- c("sales", "sb", "snb", "tv")
#
#for (k in c(1, 2)) {
  #lag_lookup <- model_data[, c("date", lag_vars)]
  #lag_lookup$date_num <- lag_lookup$date_num + k
  #names(lag_lookup)[-1] <- paste0(lag_vars, "_lag", k)
  #
  #model_data <- merge(
    #model_data, lag_lookup,
    #by = "date", all.x = TRUE, sort = FALSE
  #)
#}
#
#model_data <- model_data[order(model_data$date), ]
#
#
#
## Split by date
cutoff <- as.Date("2017-03-01")
train <- model_data[model_data$date < cutoff, ]
test  <- model_data[model_data$date >= cutoff, ]
#
## Fit the linear model using current values and lags
#m_sales_lag <- lm(
  #sales ~ sb + snb + tv +
    #sales_lag1 + sb_lag1 + snb_lag1 + tv_lag1 +
    #sales_lag7 + sb_lag7 + snb_lag7 + tv_lag7,
  #data = train,
#)
#
#print(summary(model_sales_lag))
#
## Predict test dates with complete lag information
#test$predicted_sales <- predict(model_sales_lag, newdata = test)
#
#valid_test <- test[complete.cases(test[, c("sales", "predicted_sales")]), ]
#
## Test-set error metrics
#mae  <- mean(abs(valid_test$sales - valid_test$predicted_sales))
#rmse <- sqrt(mean((valid_test$sales - valid_test$predicted_sales)^2))
#
#cat("Test MAE:", mae, "\n")
#cat("Test RMSE:", rmse, "\n")
#
## Plot actual vs. predicted sales
#plot(
  #valid_test$date, valid_test$sales,
  #type = "l", col = "black", lwd = 2,
  #xlab = "Date", ylab = "Sales",
  #main = "Actual vs. Predicted Sales w/ Lag"
#)
#lines(valid_test$date, valid_test$predicted_sales, col = "blue", lwd = 2)
#legend(
  #"topleft",
  #legend = c("Actual", "Predicted"),
  #col = c("black", "blue"),
  #lty = 1, lwd = 2
#)
#
## Fit the linear model using current values and only media lags
#m_media_lag <- lm(
  #sales ~ sb + snb + tv +
    #sb_lag1 + snb_lag1 + tv_lag1 +
    #sb_lag7 + snb_lag7 + tv_lag7,
  #data = train,
  #na.action = na.exclude
#)
#
#print(summary(m_media_lag))
#
## Predict test dates with complete lag information
#test$predicted_sales <- predict(m_media_lag, newdata = test)
#
#valid_test <- test[complete.cases(test[, c("sales", "predicted_sales")]), ]
#
## Test-set error metrics
#mae  <- mean(abs(valid_test$sales - valid_test$predicted_sales))
#rmse <- sqrt(mean((valid_test$sales - valid_test$predicted_sales)^2))
#
#cat("Test MAE:", mae, "\n")
#cat("Test RMSE:", rmse, "\n")
#
## Plot actual vs. predicted sales
#plot(
  #valid_test$date, valid_test$sales,
  #type = "l", col = "black", lwd = 2,
  #xlab = "Date", ylab = "Sales",
  #main = "Actual vs. Predicted Sales w/ Media Lag"
#)
#lines(valid_test$date, valid_test$predicted_sales, col = "blue", lwd = 2)
#legend(
  #"topleft",
  #legend = c("Actual", "Predicted"),
  #col = c("black", "blue"),
  #lty = 1, lwd = 2
####)

# Calculate and plot correlations
numeric_data <- tv[, c("sb", "snb", "tv", "sales")]
cor_matrix <- cor(numeric_data, use = "pairwise.complete.obs")

corrplot::corrplot(
  cor_matrix,
  method = "color",
  type = "upper",
  addCoef.col = "black",
  tl.col = "black",
  tl.srt = 45
)
full = tv

full$predict1 = predict(fit01, newdata=model_data)
#fit02 = lm(sales ~ sb + snb + tv + factor(dow), data=train) # dow is day of the week seasonality

#fit04 = lm(sales ~ lag(sales,1) + sb + snb + tv + factor(dow), data=train)
fit05 = auto.arima(train$sales, xreg = cbind(train$sb, train$snb, train$tv))

fit01 = lm(sales ~ sb + snb + tv, data=train)
test$predicted_sales <- predict(fit01, newdata = test)
print(test[, c("date", "sales", "predicted_sales")])
mae  <- mean(abs(test$sales - test$predicted_sales))
rmse <- sqrt(mean((test$sales - test$predicted_sales)^2))

summary(fit01)
cat("Test 1 MAE:", mae, "\n")
cat("Test 1 RMSE:", rmse, "\n")

fit03 = lm(sales ~ lag(sales,1) + sb + snb + tv, data=train)
test$predicted_sales <- predict(fit03, newdata = test)
valid_test <- test[complete.cases(test[, c("sales", "predicted_sales")]), ]

# Test-set error metrics
mae  <- mean(abs(valid_test$sales - valid_test$predicted_sales))
rmse <- sqrt(mean((valid_test$sales - valid_test$predicted_sales)^2))

summary(fit03)
cat("Test 3 lag MAE:", mae, "\n")
cat("Test 3 lag RMSE:", rmse, "\n")