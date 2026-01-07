# Load necessary libraries
library(dplyr)
library(forecast)
library(tidyr)

# Example data (replace with your actual data)
set.seed(123)
weeks <- 1:20
visa_applications <- sample(500:1000, 20, replace = TRUE)
visas_issued <- sample(400:900, 20, replace = TRUE)
visa_types <- sample(c("Ukraine Family Scheme", "Ukraine Sponsorship Scheme", "Government sponsored"), 20, replace = TRUE)
arrivals <- sample(300:800, 20, replace = TRUE)

# Create a data frame
data <- data.frame(
  week = weeks,
  visa_applications = visa_applications,
  visas_issued = visas_issued,
  visa_type = visa_types,
  arrivals = arrivals
)

# Feature engineering: Calculate the difference and proportions
data <- data %>% 
  mutate(
    application_issued_diff = visa_applications - visas_issued,
    issued_to_arrival_ratio = visas_issued / arrivals
  )

# Aggregate data by week with types
aggregated_data <- data %>%
  group_by(week) %>%
  summarise(
    total_visa_applications = sum(visa_applications),
    total_visas_issued = sum(visas_issued),
    total_arrivals = sum(arrivals),
    avg_application_issued_diff = mean(application_issued_diff),
    avg_issued_arrival_ratio = mean(issued_to_arrival_ratio)
  )

# Create a time series object
ts_data <- ts(aggregated_data$total_arrivals, frequency = 52)

# Fit an ARIMA model
fit <- auto.arima(ts_data)

# Forecast the next 12 weeks (3 months)
forecasted_values <- forecast(fit, h = 12)

# Plot the forecast
plot(forecasted_values)

# Print the forecast
print(forecasted_values)