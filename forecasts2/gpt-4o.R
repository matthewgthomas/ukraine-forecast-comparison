# Load necessary libraries
library(tidyverse)
library(lubridate) # for date manipulation
library(forecast) # for time series modeling

# Load the data
# Assume data is in a CSV file named 'ukraine_data.csv'
data <- read.csv('ukraine_data.csv')

# Check the structure of the data
str(data)

# Convert to time series format
data$Date <- as.Date(data$Date, format = "%Y-%m-%d")
data_ts <- ts(data, start=c(2023, 1), frequency=52) # Adjust start date as necessary

# Calculate rolling averages or differences if needed
data <- data %>% 
  mutate(
    VisaRatio = visas.issued / visa.applications,
    ArrivalRatio = people.arrived / visas.issued,
    VisaApplicationGrowth = visa.applications - lag(visa.applications, 1),
    ArrivalsGrowth = people.arrived - lag(people.arrived, 1)
  )

# Handle NA values caused by lag
data <- na.omit(data)

# Use ARIMA or an Exponential Smoothing State Space Model
model <- auto.arima(data$people.arrived, xreg=data[c("VisaRatio", "ArrivalRatio")])

# Check model accuracy
summary(model)

# Forecast for the next 12 weeks (3 months) using the best model selected
forecast_period <- 12
future_data <- data.frame(
  VisaRatio = tail(data$VisaRatio, forecast_period),
  ArrivalRatio = tail(data$ArrivalRatio, forecast_period)
)

forecast_results <- forecast(model, xreg=future_data, h=forecast_period)

# Plot the forecast
autoplot(forecast_results)

# Split dataset into training and testing to validate
train_data <- head(data, round(0.8 * nrow(data)))
test_data <- tail(data, round(0.2 * nrow(data)))

# Build model on training data
model_validation <- auto.arima(train_data$people.arrived, xreg=train_data[c("VisaRatio", "ArrivalRatio")])

# Forecast on test data
forecast_validation <- forecast(model_validation, xreg=test_data[c("VisaRatio", "ArrivalRatio")], h=nrow(test_data))

# Compare with actual
accuracy(forecast_validation, test_data$people.arrived)