library(forecast)
library(tidyr)
library(dplyr)

data <- read.csv("data/visas-weekly-training.csv")

data <- data |> 
  rename(
    Date = date,
    visa_applications = applications,
    visas_issued = visas_issued,
    people_arriving = arrivals
  )

# Convert date column to Date format
data$Date <- as.Date(data$Date)

# Create new features by aggregating weekly data
data_aggregated <- data %>%
  group_by(week = as.numeric(format(Date, "%U"))) %>%
  summarise(
    total_visa_applications = sum(visa_applications),
    total_visas_issued = sum(visas_issued),
    total_people_arriving = sum(people_arriving)
  )

# Convert data to time series
ts_data <- ts(data_aggregated$total_people_arriving, frequency = 52)

# Fit ARIMA model
fit <- auto.arima(ts_data)

# Forecast for the next 12 weeks (3 months)
forecast_values <- forecast(fit, h = 12)

# Print the forecasted values
print(forecast_values)

readr::write_rds(forecast_values, "forecasts2/data/gpt-3.5-turbo-forecast.rds")
