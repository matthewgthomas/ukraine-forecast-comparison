# Load packages
library(dplyr)
library(forecast)

# Load data
df <- read.csv("your_data.csv")

# Data exploration
glimpse(df)

# Organize data
df <- df %>%
  mutate(Week = as.Date(Week))

# Feature engineering
df <- df %>%
  mutate(IssueYesNo = ifelse(Issue == "Yes", 1, 0),
         FamilyScheme = ifelse(Type == "Ukraine Family Scheme", 1, 0),
         SponsorshipScheme = ifelse(Type == "Ukraine Sponsorship Scheme", 1, 0),
         GovernmentSponsored = ifelse(Type == "Government sponsored", 1, 0))

# ARIMA model for prediction, the model is built on weekly data
fit <- auto.arima(df$Application)

# Forecast for next 3 months (about 12 weeks)
forecast_values <- forecast(fit, h = 12)
print(forecast_values)