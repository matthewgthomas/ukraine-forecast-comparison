# Manual edits:
# - Changed data filename
# - Renamed `Type_of_visa` to `visa_type`
# - Renamed `visa_applications` to `applications`
# - Renamed `people_arriving_with_visas` to `arrivals`
# - Fixed the `start` parameter of `ts()` to use the year/week based on data$date
# - Saved a copy of the forecast

# Load required libraries
library(fpp2) # For time series forecasting
library(dplyr) # For data manipulation

# Read the data
# data <- read.csv("data.csv") # Replace "data.csv" with the actual file name
data <- read.csv("data/visas-weekly-training.csv")

# Engineer new features
data$approval_rate <- data$visas_issued / data$applications

# Create dummy variables for visa types
data <- data %>%
  mutate(
    Ukraine_Family_Scheme = ifelse(visa_type == "Ukraine Family Scheme", 1, 0),
    Ukraine_Sponsorship_Scheme = ifelse(visa_type == "Ukraine Sponsorship Scheme", 1, 0),
    Government_sponsored = ifelse(visa_type == "Government sponsored", 1, 0)
  )

# Calculate average arrivals per week
average_arrivals <- mean(data$arrivals)

# Time series forecasting
ts_data <- ts(
  data$arrivals, 
  start = c(min(year(data$date)), min(week(data$date))), 
  frequency = 52
)
model <- auto.arima(ts_data)
forecast <- forecast(model, h = 12) # Forecast for the next 3 months (12 weeks)

# Print forecast
print(forecast)

readr::write_rds(forecast, "data/forecast-gpt-3.5-turbo.rds")
