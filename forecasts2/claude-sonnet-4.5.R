# ============================================================================
# UKRAINE ARRIVALS FORECASTING SYSTEM
# Forecasts arrivals for next 3 months (13 weeks) using 22 weeks historical data
# ============================================================================

library(tidyverse)
library(forecast)
library(lubridate)
library(xgboost)
library(caret)

set.seed(123)

# ============================================================================
# 1. DATA SIMULATION (Replace with your actual data)
# ============================================================================

simulate_data <- function(n_weeks = 22) {
  # Simulate realistic Ukraine visa data
  weeks <- 1:n_weeks
  
  # Base trends with some seasonality
  base_applications <- 1000 + weeks * 20 + rnorm(n_weeks, 0, 100)
  base_issued <- 800 + weeks * 15 + rnorm(n_weeks, 0, 80)
  base_arrivals <- 600 + weeks * 12 + rnorm(n_weeks, 0, 70)
  
  # Visa type proportions (roughly matching real patterns)
  data <- tibble(
    week = weeks,
    date = today() - weeks(n_weeks - weeks),
    
    # Applications by type
    apps_family = pmax(0, round(base_applications * 0.3 + rnorm(n_weeks, 0, 50))),
    apps_sponsor = pmax(0, round(base_applications * 0.60 + rnorm(n_weeks, 0, 80))),
    apps_govt = pmax(0, round(base_applications * 0.10 + rnorm(n_weeks, 0, 30))),
    
    # Visas issued by type (with 1-2 week lag)
    visas_family = pmax(0, round(lag(apps_family, 2) * 0.85 + rnorm(n_weeks, 0, 30))),
    visas_sponsor = pmax(0, round(lag(apps_sponsor, 2) * 0.80 + rnorm(n_weeks, 0, 50))),
    visas_govt = pmax(0, round(lag(apps_govt, 1) * 0.90 + rnorm(n_weeks, 0, 20))),
    
    # Arrivals by type (with 2-4 week lag from visa issuance)
    arrivals_family = pmax(0, round(lag(visas_family, 3) * 0.75 + rnorm(n_weeks, 0, 25))),
    arrivals_sponsor = pmax(0, round(lag(visas_sponsor, 3) * 0.70 + rnorm(n_weeks, 0, 35))),
    arrivals_govt = pmax(0, round(lag(visas_govt, 2) * 0.85 + rnorm(n_weeks, 0, 15)))
  ) %>%
    replace_na(list(
      visas_family = 0, visas_sponsor = 0, visas_govt = 0,
      arrivals_family = 0, arrivals_sponsor = 0, arrivals_govt = 0
    )) %>%
    mutate(
      total_applications = apps_family + apps_sponsor + apps_govt,
      total_visas = visas_family + visas_sponsor + visas_govt,
      total_arrivals = arrivals_family + arrivals_sponsor + arrivals_govt
    )
  
  return(data)
}

# Load your actual data here
# df <- read_csv("your_data.csv")
df <- simulate_data(22)

cat("Data Summary:\n")
print(summary(df[, c("total_applications", "total_visas", "total_arrivals")]))

# ============================================================================
# 2. FEATURE ENGINEERING
# ============================================================================

engineer_features <- function(data) {
  data %>%
    arrange(week) %>%
    mutate(
      # ---- Lag Features (captures recent trends) ----
      arrivals_lag1 = lag(total_arrivals, 1),
      arrivals_lag2 = lag(total_arrivals, 2),
      arrivals_lag3 = lag(total_arrivals, 3),
      arrivals_lag4 = lag(total_arrivals, 4),
      
      # ---- Moving Averages (smooths volatility) ----
      arrivals_ma3 = zoo::rollmean(total_arrivals, k = 3, fill = NA, align = "right"),
      arrivals_ma4 = zoo::rollmean(total_arrivals, k = 4, fill = NA, align = "right"),
      
      # ---- Visa Pipeline Indicators (lead indicators) ----
      visas_lag1 = lag(total_visas, 1),
      visas_lag2 = lag(total_visas, 2),
      visas_lag3 = lag(total_visas, 3),
      apps_lag2 = lag(total_applications, 2),
      apps_lag3 = lag(total_applications, 3),
      apps_lag4 = lag(total_applications, 4),
      
      # ---- Conversion Rates (captures changing patterns) ----
      visa_to_arrival_rate = ifelse(lag(total_visas, 2) > 0, 
                                      total_arrivals / lag(total_visas, 2), NA),
      app_to_visa_rate = ifelse(lag(total_applications, 2) > 0,
                                 total_visas / lag(total_applications, 2), NA),
      
      # ---- Visa Type Proportions ----
      prop_family = visas_family / (total_visas + 1),
      prop_sponsor = visas_sponsor / (total_visas + 1),
      prop_govt = visas_govt / (total_visas + 1),
      
      # ---- Trend Features ----
      trend = row_number(),
      trend_sq = trend^2,
      
      # ---- Velocity (rate of change) ----
      arrivals_velocity = total_arrivals - lag(total_arrivals, 1),
      visas_velocity = total_visas - lag(total_visas, 1),
      
      # ---- Week-over-Week Growth ----
      arrivals_growth = ifelse(lag(total_arrivals, 1) > 0,
                               (total_arrivals - lag(total_arrivals, 1)) / lag(total_arrivals, 1),
                               0),
      
      # ---- Cumulative Metrics ----
      cum_arrivals = cumsum(total_arrivals),
      cum_visas = cumsum(total_visas)
    )
}

df_features <- engineer_features(df)

# ============================================================================
# 3. MULTIPLE FORECASTING MODELS
# ============================================================================

# ---- Model 1: ARIMA (Auto-regressive Integrated Moving Average) ----
fit_arima <- function(data) {
  ts_data <- ts(data$total_arrivals, frequency = 1)
  model <- auto.arima(ts_data, seasonal = FALSE, stepwise = FALSE, approximation = FALSE)
  return(model)
}

# ---- Model 2: ETS (Exponential Smoothing) ----
fit_ets <- function(data) {
  ts_data <- ts(data$total_arrivals, frequency = 1)
  model <- ets(ts_data)
  return(model)
}

# ---- Model 3: Linear Regression with Engineered Features ----
fit_lm <- function(data) {
  df_complete <- data %>%
    filter(!is.na(arrivals_lag1) & !is.na(visas_lag2) & !is.na(arrivals_ma3))
  
  model <- lm(total_arrivals ~ arrivals_lag1 + arrivals_lag2 + arrivals_lag3 + 
                visas_lag1 + visas_lag2 + visas_lag3 + 
                arrivals_ma3 + trend + visa_to_arrival_rate +
                prop_family + prop_sponsor,
              data = df_complete)
  return(model)
}

# ---- Model 4: XGBoost (Gradient Boosting) ----
fit_xgboost <- function(data) {
  df_complete <- data %>%
    filter(!is.na(arrivals_lag1) & !is.na(visas_lag2) & !is.na(arrivals_ma3))
  
  features <- c("arrivals_lag1", "arrivals_lag2", "arrivals_lag3", "arrivals_lag4",
                "visas_lag1", "visas_lag2", "visas_lag3",
                "apps_lag2", "apps_lag3",
                "arrivals_ma3", "arrivals_ma4",
                "visa_to_arrival_rate", "app_to_visa_rate",
                "prop_family", "prop_sponsor", "prop_govt",
                "trend", "arrivals_velocity", "visas_velocity")
  
  train_x <- as.matrix(df_complete[, features])
  train_y <- df_complete$total_arrivals
  
  model <- xgboost(
    data = train_x,
    label = train_y,
    nrounds = 100,
    max_depth = 3,
    eta = 0.1,
    verbose = 0,
    objective = "reg:squarederror"
  )
  
  return(list(model = model, features = features))
}

# ---- Model 5: Pipeline Model (captures visa→arrival relationship) ----
fit_pipeline_model <- function(data) {
  # This model explicitly uses the pipeline: recent visas predict arrivals
  df_complete <- data %>%
    filter(!is.na(visas_lag2) & !is.na(visas_lag3))
  
  model <- lm(total_arrivals ~ visas_lag2 + visas_lag3 + 
                visa_to_arrival_rate + trend +
                prop_family + prop_sponsor,
              data = df_complete)
  return(model)
}

# ============================================================================
# 4. TIME SERIES CROSS-VALIDATION
# ============================================================================

time_series_cv <- function(data, n_folds = 4, horizon = 13) {
  n <- nrow(data)
  min_train <- n - n_folds * 2  # Minimum training size
  
  results <- list()
  
  for (fold in 1:n_folds) {
    train_end <- min_train + fold - 1
    test_start <- train_end + 1
    test_end <- min(test_start + horizon - 1, n)
    
    train_data <- data[1:train_end, ]
    test_data <- data[test_start:test_end, ]
    
    results[[fold]] <- list(
      train = train_data,
      test = test_data,
      train_idx = 1:train_end,
      test_idx = test_start:test_end
    )
  }
  
  return(results)
}

# ============================================================================
# 5. GENERATE FORECASTS
# ============================================================================

generate_forecast <- function(data, horizon = 13) {
  
  cat("\n=== Fitting Models ===\n")
  
  # Fit all models
  model_arima <- fit_arima(data)
  model_ets <- fit_ets(data)
  model_lm <- fit_lm(df_features)
  model_xgb <- fit_xgboost(df_features)
  model_pipeline <- fit_pipeline_model(df_features)
  
  cat("✓ ARIMA fitted\n")
  cat("✓ ETS fitted\n")
  cat("✓ Linear Regression fitted\n")
  cat("✓ XGBoost fitted\n")
  cat("✓ Pipeline Model fitted\n")
  
  # Generate forecasts
  forecast_arima <- forecast(model_arima, h = horizon)
  forecast_ets <- forecast(model_ets, h = horizon)
  
  # For regression models, need to create future data
  future_data <- create_future_data(df_features, horizon)
  
  # Linear model forecast
  forecast_lm <- predict(model_lm, newdata = future_data)
  
  # XGBoost forecast
  future_x <- as.matrix(future_data[, model_xgb$features])
  forecast_xgb <- predict(model_xgb$model, future_x)
  
  # Pipeline model forecast
  forecast_pipeline <- predict(model_pipeline, newdata = future_data)
  
  # Ensemble: weighted average of all models
  # Weight based on model sophistication and theoretical fit
  weights <- c(0.15, 0.15, 0.20, 0.30, 0.20)  # ARIMA, ETS, LM, XGB, Pipeline
  
  forecast_ensemble <- (
    as.numeric(forecast_arima$mean) * weights[1] +
    as.numeric(forecast_ets$mean) * weights[2] +
    as.numeric(forecast_lm) * weights[3] +
    as.numeric(forecast_xgb) * weights[4] +
    as.numeric(forecast_pipeline) * weights[5]
  )
  
  # Compile results
  results <- tibble(
    week = (nrow(data) + 1):(nrow(data) + horizon),
    forecast_date = max(data$date) + weeks(1:horizon),
    arima = as.numeric(forecast_arima$mean),
    ets = as.numeric(forecast_ets$mean),
    lm = as.numeric(forecast_lm),
    xgboost = as.numeric(forecast_xgb),
    pipeline = as.numeric(forecast_pipeline),
    ensemble = forecast_ensemble,
    lower_80 = as.numeric(forecast_arima$lower[, 1]),
    upper_80 = as.numeric(forecast_arima$upper[, 1]),
    lower_95 = as.numeric(forecast_arima$lower[, 2]),
    upper_95 = as.numeric(forecast_arima$upper[, 2])
  )
  
  return(list(
    forecasts = results,
    models = list(arima = model_arima, ets = model_ets, lm = model_lm,
                  xgboost = model_xgb, pipeline = model_pipeline)
  ))
}

# Helper function to create future data for regression models
create_future_data <- function(data, horizon) {
  last_row <- tail(data, 1)
  future_data <- tibble()
  
  for (h in 1:horizon) {
    if (h == 1) {
      base_data <- last_row
    } else {
      base_data <- tail(future_data, 1)
    }
    
    new_row <- tibble(
      week = last_row$week + h,
      trend = last_row$trend + h,
      
      # Use forecasted/carried forward values
      arrivals_lag1 = if (h == 1) last_row$total_arrivals else base_data$total_arrivals,
      arrivals_lag2 = if (h <= 2) lag(data$total_arrivals, 2 - h + 1)[nrow(data)] else 
        future_data$total_arrivals[h - 2],
      arrivals_lag3 = if (h <= 3) lag(data$total_arrivals, 3 - h + 1)[nrow(data)] else 
        future_data$total_arrivals[h - 3],
      arrivals_lag4 = if (h <= 4) lag(data$total_arrivals, 4 - h + 1)[nrow(data)] else 
        future_data$total_arrivals[h - 4],
      
      # Assume visa numbers remain stable or follow recent trend
      visas_lag1 = last_row$total_visas,
      visas_lag2 = last_row$visas_lag1,
      visas_lag3 = last_row$visas_lag2,
      apps_lag2 = last_row$total_applications,
      apps_lag3 = last_row$apps_lag2,
      
      # Use recent averages
      arrivals_ma3 = last_row$arrivals_ma3,
      arrivals_ma4 = last_row$arrivals_ma4,
      visa_to_arrival_rate = last_row$visa_to_arrival_rate,
      app_to_visa_rate = last_row$app_to_visa_rate,
      prop_family = last_row$prop_family,
      prop_sponsor = last_row$prop_sponsor,
      prop_govt = last_row$prop_govt,
      arrivals_velocity = last_row$arrivals_velocity,
      visas_velocity = last_row$visas_velocity,
      total_arrivals = NA  # To be predicted
    )
    
    future_data <- bind_rows(future_data, new_row)
  }
  
  return(future_data)
}

# ============================================================================
# 6. VISUALIZATION
# ============================================================================

plot_forecast <- function(historical, forecasts) {
  historical_plot <- historical %>%
    select(week, date, total_arrivals) %>%
    mutate(type = "Historical")
  
  forecast_plot <- forecasts %>%
    select(week, forecast_date, ensemble) %>%
    rename(date = forecast_date, total_arrivals = ensemble) %>%
    mutate(type = "Forecast")
  
  combined <- bind_rows(historical_plot, forecast_plot)
  
  p <- ggplot() +
    geom_line(data = combined, aes(x = week, y = total_arrivals, color = type), 
              linewidth = 1) +
    geom_point(data = combined, aes(x = week, y = total_arrivals, color = type),
               size = 2) +
    geom_ribbon(data = forecasts,
                aes(x = week, ymin = lower_80, ymax = upper_80),
                alpha = 0.2, fill = "blue") +
    geom_ribbon(data = forecasts,
                aes(x = week, ymin = lower_95, ymax = upper_95),
                alpha = 0.1, fill = "blue") +
    scale_color_manual(values = c("Historical" = "darkblue", "Forecast" = "red")) +
    labs(
      title = "Ukraine Arrivals: Historical + 13-Week Forecast",
      subtitle = "Ensemble model with 80% and 95% prediction intervals",
      x = "Week",
      y = "Total Arrivals",
      color = ""
    ) +
    theme_minimal() +
    theme(legend.position = "top")
  
  print(p)
  return(p)
}

# ============================================================================
# 7. MAIN EXECUTION
# ============================================================================

cat("\n" %+% strrep("=", 70) %+% "\n")
cat("UKRAINE ARRIVALS FORECASTING SYSTEM\n")
cat(strrep("=", 70) %+% "\n")

# Generate forecasts
results <- generate_forecast(df, horizon = 13)

# Display results
cat("\n=== 13-Week Forecast Summary ===\n")
forecast_summary <- results$forecasts %>%
  select(week, forecast_date, ensemble, lower_95, upper_95) %>%
  mutate(
    ensemble = round(ensemble, 0),
    lower_95 = round(lower_95, 0),
    upper_95 = round(upper_95, 0)
  )

print(forecast_summary)

cat("\n=== 3-Month Total Forecast ===\n")
cat(sprintf("Expected Total Arrivals: %d\n", round(sum(results$forecasts$ensemble))))
cat(sprintf("95%% Confidence Interval: [%d, %d]\n",
            round(sum(results$forecasts$lower_95)),
            round(sum(results$forecasts$upper_95))))

# Plot
plot_forecast(df, results$forecasts)

# Model comparison
cat("\n=== Model Comparison (Sample Week) ===\n")
model_comparison <- results$forecasts %>%
  slice(1) %>%
  select(week, arima, ets, lm, xgboost, pipeline, ensemble) %>%
  pivot_longer(-week, names_to = "model", values_to = "prediction") %>%
  mutate(prediction = round(prediction, 0))

print(model_comparison)

cat("\n" %+% strrep("=", 70) %+% "\n")
cat("Analysis Complete!\n")
cat(strrep("=", 70) %+% "\n")

# ============================================================================
# 8. EXPORT RESULTS
# ============================================================================

# Save forecasts
write_csv(results$forecasts, "ukraine_arrivals_forecast.csv")
cat("\nForecasts saved to: ukraine_arrivals_forecast.csv\n")

# Return results
results