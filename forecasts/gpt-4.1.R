# Manual edits:
# - Loaded the correct data
# - Renamed `visa_apps` to `applications`
# - Added missing columns to `pivot_wider()`
# - Fixed names of dummy variables
# - Fixed column names for `future_xreg`
# - Saved a copy of the forecast

library(dplyr)
library(tidyr)
library(lubridate)
library(forecast)

# Suppose 'data' is your dataframe with: week, visa_apps, visas_issued, visa_type, arrivals
# For illustration, create mock data
set.seed(123)
# weeks <- seq Date(from = as Date("2023-01-01"), by = "week", length out = 52)
# visa_types <- c("Ukraine Family Scheme", "Ukraine Sponsorship Scheme", "Government sponsored")
# data <- expand grid(week = weeks, visa_type = visa_types)
# data$visa_apps <- sample(50:500, nrow(data), replace=TRUE)
# data$visas_issued <- data$visa_apps - sample(1:20, nrow(data), replace=TRUE)
# data$arrivals <- round(pmax(0, data$visas_issued - sample(5:30, nrow(data), replace=TRUE)))

data <- read.csv("data/visas-weekly-training.csv")
data <- rename(data, week = date)
data$week <- as.Date(data$week)

# Create lag features and ratios
data <- data %>%
  arrange(visa_type, week) %>%
  group_by(visa_type) %>%
  mutate(
    visas_issued_lag1 = lag(visas_issued, 1),
    visas_issued_lag2 = lag(visas_issued, 2),
    arrivals_lag1 = lag(arrivals, 1),
    ratio_issued_to_apps = visas_issued / applications,
    week_num = as.numeric(format(week, "%U"))
  ) %>% 
  ungroup()

# Dummy variables for 'visa_type'
data <- data %>%
  mutate(visa_type = as.factor(visa_type)) %>%
  tidyr::pivot_wider(
    names_from = visa_type, 
    values_from = c(applications, visas_issued, arrivals, visas_issued_lag1, visas_issued_lag2),
    values_fill = 0
  )

# Fill NA lagged values with 0 or impute
data[is.na(data)] <- 0

# Aggregate arrivals per week
weekly <- data %>%
  group_by(week) %>%
  summarise(
    total_arrivals = sum(`arrivals_Ukraine Family Scheme` + `arrivals_Ukraine Sponsorship Scheme` + `arrivals_Government sponsored`),
    total_visas_issued = sum(`visas_issued_Ukraine Family Scheme` + `visas_issued_Ukraine Sponsorship Scheme` + `visas_issued_Government sponsored`),
    total_visas_issued_lag1 = sum(`visas_issued_lag1_Ukraine Family Scheme` + `visas_issued_lag1_Ukraine Sponsorship Scheme` + `visas_issued_lag1_Government sponsored`),
    total_visas_issued_lag2 = sum(`visas_issued_lag2_Ukraine Family Scheme` + `visas_issued_lag2_Ukraine Sponsorship Scheme` + `visas_issued_lag2_Government sponsored`)
  )

# Prepare time series object
ts_arrivals <- ts(weekly$total_arrivals, frequency = 52)

# Simple ARIMAX model: arrivals ~ visas_issued (current, lag1, lag2)
xreg <- as.matrix(weekly[, c("total_visas_issued", "total_visas_issued_lag1", "total_visas_issued_lag2")])

fit <- auto.arima(ts_arrivals, xreg=xreg)

# Forecast next 13 weeks (that's about 3 months)
# Predictors for next periods: you need to estimate or assume future visas_issued
# Here, we will simply use the mean of recent weeks for predictors
total_visas_issued <- rep(mean(weekly$total_visas_issued, na.rm=TRUE), 13)
total_visas_issued_lag1 <- rep(mean(weekly$total_visas_issued_lag1, na.rm=TRUE), 13)
total_visas_issued_lag2 <- rep(mean(weekly$total_visas_issued_lag2, na.rm=TRUE), 13)

future_xreg <- cbind(total_visas_issued, total_visas_issued_lag1, total_visas_issued_lag2)

forecasted <- forecast(fit, xreg=future_xreg, h=13)

# Plot
plot(forecasted, main="Forecast of Ukrainian Arrivals (next 3 months)")
print(forecasted)

readr::write_rds(forecasted, "data/forecast-gpt-4.1.rds")
