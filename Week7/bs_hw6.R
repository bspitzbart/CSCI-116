################################################################### 
# H06
# Predicting Macroeconomic Variables and VAR
# Brad Spitzbart
# CSCI-E-116 10/28/2026
# Created with GPT-6 Luna
################################################################### 
setwd("C:/Users/brs416/Desktop/CSCI-E-116_bigdata/Week7")

library(fredr)
library(vars)
library(forecast)

# Read back to work offline 
#monthly_data <- readRDS("monthly_data.rds")

# Set your FRED API key in the environment before running:
# Sys.setenv(FRED_API_KEY = "your_api_key")
fredr_set_key("491105751b9f9553cde56be27af8dd4f")

series <- c(
  oil_price       = "DCOILWTICO",
  nasdaq          = "NASDAQCOM",
  nonfarm_payrolls = "PAYEMS",
  core_cpi        = "CPILFESL",
  fed_funds_rate  = "DFF"
)

monthly_data <- lapply(names(series), function(name) {
  x <- fredr(
    series_id = unname(series[name]),
    observation_start = as.Date("1986-02-01"),
    observation_end   = as.Date("2025-08-31")
  )
  
  # Assign each observation to the first day of its month
  x$month <- as.Date(format(x$date, "%Y-%m-01"))
  
  # Average observations within each month, ignoring missing values
  out <- aggregate(
    value ~ month,
    data = x,
    FUN = function(v) {
      if (all(is.na(v))) NA_real_ else mean(v, na.rm = TRUE)
    }
  )
  
  names(out)[names(out) == "value"] <- name
  out
})

names(monthly_data) <- names(series)

# Full monthly index: February 1986 through August 2025
month_seq <- seq(
  as.Date("1986-02-01"),
  as.Date("2025-08-01"),
  by = "month"
)

# Complete any missing months with NA, then convert each series to ts
ts_data <- lapply(names(monthly_data), function(name) {
  df <- monthly_data[[name]]
  
  aligned <- merge(
    data.frame(month = month_seq),
    df,
    by = "month",
    all.x = TRUE,
    sort = TRUE
  )
  
  ts(
    aligned[[name]],
    start = c(1986, 2),
    frequency = 12
  )
})

names(ts_data) <- names(monthly_data)

# Plot each series in its own panel and on its own scale
old_par <- par(mfrow = c(3, 2), mar = c(3, 4, 3, 1))
for (name in names(ts_data)) {
  plot(ts_data[[name]], main = name, xlab = "Year", ylab = "")
}
par(old_par)

# Give the series the requested names
oil   <- ts_data$oil_price
stock <- ts_data$nasdaq
job   <- ts_data$nonfarm_payrolls
cpi   <- ts_data$core_cpi
frr   <- ts_data$fed_funds_rate

# Original series in levels
data0 <- cbind(oil, stock, job, cpi, frr)

# Continuously compounded growth rates for the first four series
oil_g   <- diff(log(oil))
stock_g <- diff(log(stock))
job_g   <- diff(log(job))
cpi_g   <- diff(log(cpi))

# Match the federal funds rate to the growth-rate date range
frr_aligned <- window(frr, start = start(oil_g), end = end(oil_g))

# Transformed series: first four in log differences; fed funds rate in levels
data1 <- cbind(oil_g, stock_g, job_g, cpi_g, frr = frr_aligned)

colnames(data0) <- c("oil", "stock", "job", "cpi", "frr")
colnames(data1) <- c("oil_g", "stock_g", "job_g", "cpi_g", "frr")

start(data1)  # Should begin one month after data0
frequency(data1)

data00 <- window(data0, start = c(1986, 3), end = c(2025, 6))
data10 <- window(data1, start = c(1986, 3), end = c(2025, 6))

# Confirm the training sets contain no missing observations
colSums(is.na(data00))
colSums(is.na(data10))

# Estimate VARs with 12 monthly lags
var_level <- VAR(data00, p = 12, type = "both")  # Constant and trend
var_growth <- VAR(data10, p = 12, type = "const") # Constant only

# View estimation results
summary(var_level)
summary(var_growth)

# Refit through November 2024 for forecasts from December 2024
train0 <- window(data0, start = c(1986, 3), end = c(2024, 11))
train1 <- window(data1, start = c(1986, 3), end = c(2024, 11))

stopifnot(!anyNA(train0), !anyNA(train1))

var_level <- VAR(train0, p = 12, type = "both")
var_growth <- VAR(train1, p = 12, type = "const")

pred_level  <- predict(var_level,  n.ahead = 12)
pred_growth <- predict(var_growth, n.ahead = 12)

# Forecasts from the levels VAR
vars_level <- colnames(train0)
fc_level <- sapply(vars_level, function(v) {
  pred_level$fcst[[v]][, "fcst"]
})
fc_level <- ts(fc_level, start = c(2024, 12), frequency = 12)
colnames(fc_level) <- vars_level

# Convert log-difference forecasts back to levels
fc_growth_level <- matrix(
  NA_real_, nrow = 12, ncol = length(vars_level),
  dimnames = list(NULL, vars_level)
)

growth_names <- c(oil = "oil_g", stock = "stock_g",
                  job = "job_g", cpi = "cpi_g")

for (v in names(growth_names)) {
  last_observed <- as.numeric(
    tail(window(data0[, v], end = c(2024, 11)), 1)
  )
  forecast_growth <- pred_growth$fcst[[growth_names[[v]]]][, "fcst"]
  
  fc_growth_level[, v] <- last_observed * exp(cumsum(forecast_growth))
}

# The federal funds rate stays in levels in the growth VAR
fc_growth_level[, "frr"] <- pred_growth$fcst[["frr"]][, "fcst"]

fc_growth_level <- ts(
  fc_growth_level, start = c(2024, 12), frequency = 12
)

# Plot recent observed values and both forecasts, one panel per variable
old_par <- par(mfrow = c(3, 2), mar = c(3, 4, 3, 1))

for (v in vars_level) {
  history <- window(data0[, v], start = c(2023, 12), end = c(2024, 11))
  y_range <- range(c(history, fc_level[, v], fc_growth_level[, v]),
                   na.rm = TRUE)
  
  plot(history, xlim = c(2023.9, 2025.95), ylim = y_range,
       main = v, xlab = "Year", ylab = "", col = "black")
  lines(fc_level[, v], col = "blue", lwd = 2)
  lines(fc_growth_level[, v], col = "red", lwd = 2)
  
  legend("topleft",
         legend = c("Observed", "Level VAR", "Growth VAR, converted to levels"),
         col = c("black", "blue", "red"), lty = 1, bty = "n", cex = 0.8)
}

par(old_par)

stopifnot(!anyNA(data00), !anyNA(data10))

h <- 12
vars_level <- colnames(data00)

# Fit one ARIMA model per series and forecast 12 months
arima_models <- lapply(vars_level, function(v) {
  forecast::auto.arima(data00[, v])
})
names(arima_models) <- vars_level

arima_fc <- sapply(vars_level, function(v) {
  as.numeric(forecast::forecast(arima_models[[v]], h = h)$mean)
})
arima_fc <- ts(
  arima_fc,
  start = c(2025, 7),
  frequency = 12
)
colnames(arima_fc) <- vars_level

# Refit both VARs on the same training period
var_level <- vars::VAR(data00, p = 12, type = "both")
var_growth <- vars::VAR(data10, p = 12, type = "const")

pred_level  <- predict(var_level, n.ahead = h)
pred_growth <- predict(var_growth, n.ahead = h)

# Level-VAR forecasts
var_level_fc <- sapply(vars_level, function(v) {
  pred_level$fcst[[v]][, "fcst"]
})
var_level_fc <- ts(var_level_fc, start = c(2025, 7), frequency = 12)
colnames(var_level_fc) <- vars_level

# Convert growth-VAR forecasts back to levels
var_growth_fc <- matrix(
  NA_real_,
  nrow = h,
  ncol = length(vars_level),
  dimnames = list(NULL, vars_level)
)

growth_names <- c(
  oil = "oil_g", stock = "stock_g",
  job = "job_g", cpi = "cpi_g"
)

for (v in names(growth_names)) {
  last_value <- as.numeric(tail(data00[, v], 1))
  growth_forecast <- pred_growth$fcst[[growth_names[[v]]]][, "fcst"]
  var_growth_fc[, v] <- last_value * exp(cumsum(growth_forecast))
}

# Federal funds rate is already in levels in the growth VAR
var_growth_fc[, "frr"] <- pred_growth$fcst[["frr"]][, "fcst"]

var_growth_fc <- ts(var_growth_fc, start = c(2025, 7), frequency = 12)

# Plot recent observations and all three forecasts
old_par <- par(mfrow = c(3, 2), mar = c(3, 4, 3, 1))

for (v in vars_level) {
  history <- window(data00[, v], start = c(2024, 7))
  y_range <- range(
    history, arima_fc[, v], var_level_fc[, v], var_growth_fc[, v],
    na.rm = TRUE
  )
  
  plot(history, xlim = c(2024.5, 2026.5), ylim = y_range,
       main = v, xlab = "Year", ylab = "", col = "black")
  lines(arima_fc[, v], col = "darkgreen", lwd = 2)
  lines(var_level_fc[, v], col = "blue", lwd = 2)
  lines(var_growth_fc[, v], col = "red", lwd = 2)
  
  legend("topleft",
         legend = c("Observed", "auto.arima", "Level VAR", "Growth VAR"),
         col = c("black", "darkgreen", "blue", "red"),
         lty = 1, bty = "n", cex = 0.8)
}

par(old_par)

# Optional: print the selected ARIMA models
#lapply(arima_models, forecast::summary)

# Re-estimate the models on the training sets
var_level  <- VAR(data00, p = 12, type = "both")
var_growth <- VAR(data10, p = 12, type = "const")

# IRFs for 24 months, with 95% bootstrap confidence bands
irf_level <- irf(
  var_level,
  n.ahead = 24,
  ortho = TRUE,
  boot = TRUE,
  runs = 200,
  ci = 0.95,
  seed = 123
)

irf_growth <- irf(
  var_growth,
  n.ahead = 24,
  ortho = TRUE,
  boot = TRUE,
  runs = 200,
  ci = 0.95,
  seed = 123
)

# Plot all responses to each variable's shock.
# This produces five plots for each model, each with response panels.
for (shock in colnames(data00)) {
  plot(
    irf_level,
    names = shock,
    plot.type = "multiple",
    nc = 2,
    main = paste("Level VAR: shock to", shock)
  )
}

for (shock in colnames(data10)) {
  plot(
    irf_growth,
    names = shock,
    plot.type = "multiple",
    nc = 2,
    main = paste("Growth VAR: shock to", shock)
  )
}

# Equivalent responses in each model:
# Growth VAR uses log differences for oil, stock, jobs, and CPI.
# Level VAR uses those series in levels.
pairs_growth <- list(
  imp01 = c(impulse = "oil_g", response = "cpi_g"),
  imp02 = c(impulse = "cpi_g", response = "frr"),
  imp03 = c(impulse = "frr",   response = "job_g"),
  imp04 = c(impulse = "frr",   response = "stock_g")
)

pairs_level <- list(
  imp01 = c(impulse = "oil",   response = "cpi"),
  imp02 = c(impulse = "cpi",   response = "frr"),
  imp03 = c(impulse = "frr",   response = "job"),
  imp04 = c(impulse = "frr",   response = "stock")
)

make_irfs <- function(model, pairs) {
  lapply(pairs, function(pair) {
    vars::irf(
      model,
      impulse  = unname(pair["impulse"]),
      response = unname(pair["response"]),
      n.ahead  = 36,
      ortho    = FALSE,
      boot     = TRUE,
      runs     = 1000,
      ci       = 0.95,
      seed     = 123
    )
  })
}

irfs_growth <- make_irfs(var_growth, pairs_growth)
irfs_level  <- make_irfs(var_level, pairs_level)

# Plot each response function
plot_irfs <- function(irfs, pairs, model_name) {
  for (nm in names(irfs)) {
    plot(
      irfs[[nm]],
      plot.type = "single",
      main = paste(
        model_name, "- shock to", pairs[[nm]]["impulse"],
        "response of", pairs[[nm]]["response"]
      ),
      xlab = "Months after shock"
    )
  }
}

plot_irfs(irfs_growth, pairs_growth, "Growth VAR")
plot_irfs(irfs_level,  pairs_level,  "Level VAR")

vars_order <- c("oil", "stock", "job", "cpi", "frr")
h <- 2

# Training data: through June 2025
train_level  <- window(data0, start = c(1986, 3), end = c(2025, 6))
train_growth <- window(data1, start = c(1986, 3), end = c(2025, 6))

# Test observations: July and August 2025
test <- window(data0, start = c(2025, 7), end = c(2025, 8))

stopifnot(
  nrow(as.matrix(test)) == 2,
  identical(colnames(train_level), vars_order),
  !anyNA(train_level),
  !anyNA(train_growth),
  !anyNA(test)
)

# 1. Level VAR forecast
var_level <- VAR(train_level, p = 12, type = "both")
pred_level <- predict(var_level, n.ahead = h)

fc_level <- sapply(vars_order, function(v) {
  pred_level$fcst[[v]][, "fcst"]
})
fc_level <- as.matrix(fc_level)
colnames(fc_level) <- vars_order

# 2. Growth VAR forecast, converted back to levels
var_growth <- VAR(train_growth, p = 12, type = "const")
pred_growth <- predict(var_growth, n.ahead = h)

fc_growth_level <- matrix(
  NA_real_, nrow = h, ncol = length(vars_order),
  dimnames = list(NULL, vars_order)
)

growth_names <- c(
  oil = "oil_g", stock = "stock_g",
  job = "job_g", cpi = "cpi_g"
)

for (v in names(growth_names)) {
  last_level <- as.numeric(tail(train_level[, v], 1))
  growth_fc <- pred_growth$fcst[[growth_names[[v]]]][, "fcst"]
  
  # Invert log differences: level forecast = last level * exp(cumulative growth)
  fc_growth_level[, v] <- last_level * exp(cumsum(growth_fc))
}

# Federal funds rate was modeled in levels
fc_growth_level[, "frr"] <- pred_growth$fcst[["frr"]][, "fcst"]

# 3. Individual auto.arima forecasts for the five level series
arima_models <- lapply(vars_order, function(v) {
  forecast::auto.arima(train_level[, v])
})
names(arima_models) <- vars_order

fc_arima <- sapply(vars_order, function(v) {
  as.numeric(forecast::forecast(arima_models[[v]], h = h)$mean)
})
fc_arima <- as.matrix(fc_arima)
colnames(fc_arima) <- vars_order

# Collect forecasts
forecasts <- list(
  Level_VAR = fc_level,
  Growth_VAR_converted = fc_growth_level,
  auto_ARIMA = fc_arima
)

# Test actuals as a matrix
actual <- as.matrix(test)[, vars_order, drop = FALSE]

# Per-variable errors
error_table <- do.call(rbind, lapply(names(forecasts), function(model) {
  do.call(rbind, lapply(vars_order, function(v) {
    errors <- actual[, v] - forecasts[[model]][, v]
    
    # MASE denominator: mean absolute one-month-naive training error
    scale <- mean(abs(diff(as.numeric(train_level[, v]))), na.rm = TRUE)
    
    data.frame(
      Model = model,
      Variable = v,
      MAE = mean(abs(errors)),
      RMSE = sqrt(mean(errors^2)),
      MASE = mean(abs(errors)) / scale
    )
  }))
}))

# Per-variable results and model ranking
error_table <- error_table[order(error_table$Variable, error_table$MASE), ]
print(error_table, row.names = FALSE)

mean_mase <- aggregate(MASE ~ Model, data = error_table, FUN = mean)
mean_mase <- mean_mase[order(mean_mase$MASE), ]
cat("\nMean MASE across the five variables (lower is better):\n")
print(mean_mase, row.names = FALSE)

cat("\nBest by mean MASE:", mean_mase$Model[1], "\n")

vars_order <- c("oil", "stock", "job", "cpi", "frr")
growth_names <- c(
  oil = "oil_g", stock = "stock_g",
  job = "job_g", cpi = "cpi_g"
)
lags <- 1:12

# Training data through June 2025; test data are July and August 2025
train_level  <- window(data0, start = c(1986, 3), end = c(2025, 6))
train_growth <- window(data1, start = c(1986, 3), end = c(2025, 6))
actual <- as.matrix(window(data0, start = c(2025, 7), end = c(2025, 8)))[,
                                                                         vars_order, drop = FALSE
]

stopifnot(nrow(actual) == 2, !anyNA(actual), all(actual > 0))

# Return forecasts for both models in the original level units
forecast_var_levels <- function(p, model_type = c("level", "growth")) {
  model_type <- match.arg(model_type)
  
  if (model_type == "level") {
    fit <- VAR(train_level, p = p, type = "both")
    pred <- predict(fit, n.ahead = 2)
    
    fc <- sapply(vars_order, function(v) pred$fcst[[v]][, "fcst"])
    return(matrix(fc, nrow = 2, dimnames = list(NULL, vars_order)))
  }
  
  fit <- VAR(train_growth, p = p, type = "const")
  pred <- predict(fit, n.ahead = 2)
  
  # Convert the four log-growth forecasts back into level forecasts
  fc <- matrix(NA_real_, nrow = 2, ncol = length(vars_order),
               dimnames = list(NULL, vars_order))
  
  for (v in names(growth_names)) {
    last_level <- as.numeric(tail(train_level[, v], 1))
    growth_fc <- pred$fcst[[growth_names[[v]]]][, "fcst"]
    fc[, v] <- last_level * exp(cumsum(growth_fc))
  }
  
  # Federal funds rate was modeled in levels
  fc[, "frr"] <- pred$fcst[["frr"]][, "fcst"]
  fc
}

# MSLE across all 2 months x 5 variables; invalid nonpositive forecasts score Inf
score_msle <- function(fc, actual) {
  if (any(!is.finite(fc)) || any(fc <= 0)) return(c(MSLE = Inf, SSE = Inf))
  
  squared_log_errors <- (log(fc) - log(actual))^2
  c(
    MSLE = mean(squared_log_errors),
    SSE  = sum(squared_log_errors)
  )
}

# Evaluate each lag for each VAR specification
results <- do.call(rbind, lapply(c("level", "growth"), function(model_type) {
  do.call(rbind, lapply(lags, function(p) {
    fc <- tryCatch(
      forecast_var_levels(p, model_type),
      error = function(e) NULL
    )
    
    scores <- if (is.null(fc)) {
      c(MSLE = Inf, SSE = Inf)
    } else {
      score_msle(fc, actual)
    }
    
    data.frame(Model = model_type, Lag = p,
               MSLE = scores["MSLE"], SSE = scores["SSE"])
  }))
}))

results <- results[order(results$Model, results$MSLE), ]
print(results, row.names = FALSE)

# Best lag for each VAR and the corresponding p = 12 baseline
best_by_model <- results[!duplicated(results$Model), ]
p12_baseline <- results[results$Lag == 12, ]

cat("\nBest lag by model:\n")
print(best_by_model, row.names = FALSE)

cat("\nLag-12 baseline:\n")
print(p12_baseline, row.names = FALSE)

# Export - save everything I want to work on the plane later
#saveRDS(monthly_data, "monthly_data.rds")

#monthly_wide <- Reduce(
#  function(x, y) merge(x, y, by = "month", all = TRUE),
#  monthly_data
#)
#
#monthly_wide <- monthly_wide[order(monthly_wide$month), ]
#
#write.csv(monthly_wide, "monthly_data.csv", row.names = FALSE)
#
# Read back; convert month from character to Date
#monthly_wide_read <- read.csv("monthly_data.csv")
#monthly_wide_read$month <- as.Date(monthly_wide_read$month)
