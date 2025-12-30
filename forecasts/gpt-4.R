library(dplyr)
library(lubridate)
library(forecast)

# Assuming visaData is a dataframe containing weekly totals of visa data

# Feature Engineering
visaData$v_acceptance_rate <- visaData$visas_issued / visaData$visa_applications

visaData <- mutate(visaData,  
                   ukraine_family_scheme = ifelse(type_of_visa == "Ukraine Family Scheme", 1, 0),
                   ukraine_sponsorship_scheme = ifelse(type_of_visa == "Ukraine Sponsorship Scheme", 1, 0),
                   government_sponsored = ifelse(type_of_visa == "Government sponsored", 1, 0),
                   )

visaData$arrival_rate <- visaData$people_arriving / visaData$visas_issued

# Create lagged features
lags <- 1:3
for (i in seq_along(lags)) {
  visaData <- visaData %>%
    mutate(!!paste0("lag_", lags[i], "_applications") := lag(visaData$visa_applications, n = lags[i])) %>%
    mutate(!!paste0("lag_", lags[i], "_issued") := lag(visaData$visas_issued, n = lags[i])) %>%
    mutate(!!paste0("lag_", lags[i], "_arrival_rate") := lag(visaData$arrival_rate, n = lags[i])) %>%
    mutate(!!paste0("lag_", lags[i], "_acceptance_rate") := lag(visaData$v_acceptance_rate, n = lags[i]))
}

# Create moving averages
visaData <- visaData %>% mutate(ma_applications = rollmean(visaData$visa_applications, 3, fill = NA))
visaData <- visaData %>% mutate(ma_issues = rollmean(visaData$visas_issued, 3, fill = NA))

# Using the transformed dataset to build a forecasting model
predictors <- visaData[!(colnames(visaData) %in% 'people_arriving')]
people_arriving_ts <- ts(visaData$people_arriving, start = c(year(min(visaData$week)), 1), frequency = 52)

auto_arima_model <- auto.arima(people_arriving_ts, xreg = predictors)
forecast(auto_arima_model, xreg = tail(predictors, 3))