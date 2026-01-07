# Ukraine Arrivals Forecasting Simulation
# Predicts arrivals for next 3 months based on visa data

library(tidyverse)
library(forecast)
library(lubridate)

# Load actual data
data <- read_csv("data/visas-weekly-training.csv")

# Reformat data to match what the code below is expecting
data <- data |> 
  mutate(week = week(date)) |>
  rename(
    app = applications,
    issued = visas_issued
  ) |> 
  mutate(visa_type = case_match(
    visa_type,
    "Ukraine Family Scheme"     ~ "family",
    "Ukraine Sponsorship Scheme" ~ "sponsor",
    "Government sponsored"       ~ "govt"
  )) |>
  pivot_wider(
    names_from = visa_type,
    values_from = c(app, issued, arrivals),
    names_sep = "_"
  )

# Generate synthetic historical data (52 weeks)
# set.seed(42)
# n_weeks <- 52
n_weeks <- length(unique(data$date))

# data <- tibble(
#   week = 1:n_weeks,
#   date = seq(today() - weeks(n_weeks), by = "week", length.out = n_weeks),
  
#   # Visa applications by type
#   app_family = rpois(n_weeks, lambda = 150) + 50 * sin(2*pi*week/52),
#   app_sponsor = rpois(n_weeks, lambda = 200) + 70 * sin(2*pi*week/52),
#   app_govt = rpois(n_weeks, lambda = 80) + 30 * sin(2*pi*week/52),
  
#   # Visas issued (with processing lag and approval rate)
#   issued_family = NA,
#   issued_sponsor = NA,
#   issued_govt = NA,
  
#   # Arrivals (with travel lag)
#   arrivals_family = NA,
#   arrivals_sponsor = NA,
#   arrivals_govt = NA
# )

# # Simulate visas issued (2-4 week lag, 85-95% approval rate)
# for(i in 4:n_weeks) {
#   data$issued_family[i] <- rbinom(1, round(data$app_family[i-3]), 0.90)
#   data$issued_sponsor[i] <- rbinom(1, round(data$app_sponsor[i-3]), 0.88)
#   data$issued_govt[i] <- rbinom(1, round(data$app_govt[i-2]), 0.92)
# }

# # Simulate arrivals (1-6 week lag after visa issued, 70-80% travel rate)
# for(i in 7:n_weeks) {
#   data$arrivals_family[i] <- rbinom(1, data$issued_family[i-4], 0.75)
#   data$arrivals_sponsor[i] <- rbinom(1, data$issued_sponsor[i-5], 0.72)
#   data$arrivals_govt[i] <- rbinom(1, data$issued_govt[i-3], 0.78)
# }

# Calculate totals
data <- data %>%
  mutate(
    total_applications = app_family + app_sponsor + app_govt,
    total_issued = issued_family + issued_sponsor + issued_govt,
    total_arrivals = arrivals_family + arrivals_sponsor + arrivals_govt
  ) %>%
  replace_na(list(
    issued_family = 0, issued_sponsor = 0, issued_govt = 0,
    arrivals_family = 0, arrivals_sponsor = 0, arrivals_govt = 0,
    total_issued = 0, total_arrivals = 0
  ))

# FEATURE ENGINEERING
data_features <- data %>%
  mutate(
    # Lagged variables
    issued_lag1 = lag(total_issued, 1),
    issued_lag2 = lag(total_issued, 2),
    issued_lag4 = lag(total_issued, 4),
    app_lag1 = lag(total_applications, 1),
    
    # Moving averages
    issued_ma4 = zoo::rollmean(total_issued, k = 4, fill = NA, align = "right"),
    app_ma4 = zoo::rollmean(total_applications, k = 4, fill = NA, align = "right"),
    
    # Conversion rates
    approval_rate = total_issued / lag(total_applications, 3),
    travel_rate = total_arrivals / lag(total_issued, 4),
    
    # Cumulative features
    cum_issued = cumsum(total_issued),
    cum_arrivals = cumsum(total_arrivals),
    
    # Time trends
    trend = row_number(),
    quarter = quarter(date),
    
    # Application-to-issued ratio
    app_issued_ratio = total_applications / (total_issued + 1)
  )

# Split into train/test (keep last 4 weeks for validation)
train_end <- n_weeks - 4
train_data <- data_features[1:train_end, ]
test_data <- data_features[(train_end+1):n_weeks, ]

cat("=== DATA SUMMARY ===\n")
cat(sprintf("Training weeks: %d\n", nrow(train_data)))
cat(sprintf("Test weeks: %d\n", nrow(test_data)))
cat(sprintf("Mean weekly arrivals: %.1f\n", mean(train_data$total_arrivals, na.rm=T)))

# MODEL 1: ARIMA on arrivals time series
cat("\n=== MODEL 1: ARIMA ===\n")
arrivals_ts <- ts(train_data$total_arrivals, frequency = 52)
arima_model <- auto.arima(arrivals_ts, seasonal = TRUE)
print(arima_model)

# MODEL 2: Multiple Linear Regression with engineered features
cat("\n=== MODEL 2: Regression with Engineered Features ===\n")
reg_formula <- total_arrivals ~ issued_lag4 + issued_ma4 + 
                                approval_rate + travel_rate + 
                                trend + app_issued_ratio

reg_data <- train_data %>% 
  select(total_arrivals, issued_lag4, issued_ma4, approval_rate, 
         travel_rate, trend, app_issued_ratio) %>%
  drop_na()

reg_data_clean <- reg_data |>
  filter(if_all(everything(), ~ !is.na(.) & is.finite(.)))

reg_model <- lm(reg_formula, data = reg_data_clean)
print(summary(reg_model))

# MODEL 3: Ensemble combining ARIMA and features
cat("\n=== MODEL 3: Hybrid Ensemble ===\n")
# Use ARIMA predictions as a feature
arima_fitted <- fitted(arima_model)
train_data$arima_pred <- c(arima_fitted, rep(NA, nrow(train_data) - length(arima_fitted)))

ensemble_formula <- total_arrivals ~ arima_pred + issued_lag4 + 
                                      issued_ma4 + travel_rate + trend

ensemble_data <- train_data %>% 
  select(total_arrivals, arima_pred, issued_lag4, issued_ma4, 
         travel_rate, trend) %>%
  drop_na()

#ensemble_model <- lm(ensemble_formula, data = ensemble_data)

ensemble_data_clean <- ensemble_data |>
  filter(if_all(everything(), ~ !is.na(.) & is.finite(.)))

ensemble_model <- lm(ensemble_formula, data = ensemble_data_clean)

print(summary(ensemble_model))

# FORECAST NEXT 12 WEEKS (3 months)
forecast_horizon <- 12
cat("\n=== FORECASTING NEXT 12 WEEKS ===\n")

# Generate forecast scenarios
forecast_data <- tibble(
  week = (n_weeks + 1):(n_weeks + forecast_horizon),
  date = seq(max(data$date) + weeks(1), by = "week", length.out = forecast_horizon)
)

# Assume applications continue at recent average with slight decline
recent_apps <- mean(tail(data$total_applications, 8))
forecast_data$total_applications <- round(recent_apps * exp(-0.01 * (1:forecast_horizon)))

# Project issuances based on recent approval rate
recent_approval <- mean(tail(data_features$approval_rate, 8), na.rm = TRUE)
forecast_data$total_issued <- round(forecast_data$total_applications * recent_approval)

# ARIMA forecast
arima_forecast <- forecast(arima_model, h = forecast_horizon)

# For regression models, need to build features
last_issued <- tail(data$total_issued, 8)
forecast_data$issued_lag4 <- c(last_issued[5:8], forecast_data$total_issued[1:8])
forecast_data$issued_ma4 <- zoo::rollmean(c(last_issued, forecast_data$total_issued), 
                                           k = 4, fill = NA, align = "right")[(length(last_issued)+1):
                                                                               (length(last_issued)+forecast_horizon)]
forecast_data$approval_rate <- recent_approval
forecast_data$travel_rate <- mean(tail(data_features$travel_rate, 8), na.rm = TRUE)
forecast_data$trend <- (n_weeks + 1):(n_weeks + forecast_horizon)
forecast_data$app_issued_ratio <- forecast_data$total_applications / 
                                   (forecast_data$total_issued + 1)

# Generate predictions
forecast_data$arima_pred <- as.numeric(arima_forecast$mean)
forecast_data$reg_pred <- predict(reg_model, newdata = forecast_data)
forecast_data$ensemble_pred <- predict(ensemble_model, 
                                        newdata = forecast_data %>% 
                                          mutate(arima_pred = arima_pred))

# Combine predictions (ensemble average)
forecast_data <- forecast_data %>%
  mutate(
    predicted_arrivals = round((arima_pred + reg_pred + ensemble_pred) / 3),
    lower_95 = round(predicted_arrivals * 0.75),
    upper_95 = round(predicted_arrivals * 1.25)
  )

# RESULTS SUMMARY
cat("\n=== FORECAST SUMMARY ===\n")
cat(sprintf("Total predicted arrivals (12 weeks): %d\n", 
            sum(forecast_data$predicted_arrivals)))
cat(sprintf("Range: %d - %d\n", 
            sum(forecast_data$lower_95), 
            sum(forecast_data$upper_95)))
cat(sprintf("Average weekly arrivals: %.1f\n", 
            mean(forecast_data$predicted_arrivals)))

print(forecast_data %>% 
  select(week, date, predicted_arrivals, lower_95, upper_95) %>%
  mutate(date = format(date, "%Y-%m-%d")))

# VISUALIZATION
par(mfrow = c(2, 2), mar = c(4, 4, 2, 1))

# Plot 1: Historical arrivals
plot(data$week, data$total_arrivals, type = "l", col = "blue", lwd = 2,
     xlab = "Week", ylab = "Arrivals", main = "Historical Arrivals")
grid()

# Plot 2: Applications vs Issued
plot(data$week, data$total_applications, type = "l", col = "red", lwd = 2,
     xlab = "Week", ylab = "Count", main = "Applications vs Issued Visas")
lines(data$week, data$total_issued, col = "green", lwd = 2)
legend("topleft", legend = c("Applications", "Issued"), 
       col = c("red", "green"), lty = 1, lwd = 2)
grid()

# Plot 3: Forecast with confidence intervals
all_weeks <- c(data$week, forecast_data$week)
all_arrivals <- c(data$total_arrivals, forecast_data$predicted_arrivals)
plot(all_weeks, all_arrivals, type = "n", 
     xlab = "Week", ylab = "Arrivals", 
     main = "Forecast with 95% CI")
lines(data$week, data$total_arrivals, col = "blue", lwd = 2)
lines(forecast_data$week, forecast_data$predicted_arrivals, 
      col = "red", lwd = 2, lty = 2)
polygon(c(forecast_data$week, rev(forecast_data$week)),
        c(forecast_data$lower_95, rev(forecast_data$upper_95)),
        col = rgb(1, 0, 0, 0.2), border = NA)
abline(v = n_weeks, lty = 2, col = "gray")
legend("topleft", legend = c("Historical", "Forecast", "95% CI"), 
       col = c("blue", "red", rgb(1, 0, 0, 0.2)), 
       lty = c(1, 2, 1), lwd = c(2, 2, 10))
grid()

# Plot 4: Feature importance (conversion rates)
plot(data_features$week, data_features$approval_rate, type = "l", 
     col = "purple", lwd = 2,
     xlab = "Week", ylab = "Rate", main = "Approval & Travel Rates",
     ylim = c(0, 1))
lines(data_features$week, data_features$travel_rate, col = "orange", lwd = 2)
legend("topleft", legend = c("Approval Rate", "Travel Rate"), 
       col = c("purple", "orange"), lty = 1, lwd = 2)
grid()

cat("\n=== ANALYSIS COMPLETE ===\n")
cat("Models used:\n")
cat("1. ARIMA for time series patterns\n")
cat("2. Multiple regression with engineered features\n")
cat("3. Hybrid ensemble combining both approaches\n")
cat("\nKey features:\n")
cat("- Lagged visa issuances (4 weeks)\n")
cat("- Moving averages (4 weeks)\n")
cat("- Approval and travel rates\n")
cat("- Time trends\n")

write_rds(forecast_data, "simulations/data/claude-sonnet-4.5.rds")
