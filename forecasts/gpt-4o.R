# Manual edits:
# - Loaded the correct data
# - Renamed `visa_applications` to `applications`
# - Renamed `type_of_visa` to `visa_type`
# - Saved a copy of the forecast

library(tidyverse)
library(lubridate)

# Example of loading data (replace with actual data)
# data <- read.csv("ukraine_visa_data.csv")
data <- read.csv("data/visas-weekly-training.csv")
data$date <- ymd(data$date)

data <- data %>%
  mutate(conversion_rate_applications = visas_issued / applications,
         conversion_rate_visas = arrivals / visas_issued,
         lag_1_arrivals = lag(arrivals),
         lag_2_arrivals = lag(arrivals, 2),
         lag_3_arrivals = lag(arrivals, 3))

data <- data %>%
  mutate(ukraine_family_scheme = if_else(visa_type == "Ukraine Family Scheme", 1, 0),
         ukraine_sponsorship_scheme = if_else(visa_type == "Ukraine Sponsorship Scheme", 1, 0),
         government_sponsored = if_else(visa_type == "Government sponsored", 1, 0))

library(forecast)

# Prepare the time series data
ts_data <- ts(data$arrivals, frequency = 52)  # Weekly data

# Fit the ARIMA model
arima_model <- auto.arima(ts_data)

# Forecast the next 12 weeks (3 months)
forecasted_values <- forecast(arima_model, h = 12)

# Plot the forecast
plot(forecasted_values)

write_rds(forecasted_values, "data/forecast-gpt-4o.rds")
