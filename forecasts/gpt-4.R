# Manual edits:
# - Loaded the correct data
# - Renamed `visa_applications` to `applications`
# - Renamed `type_of_visa` to `visa_type`
# - Renamed `people_arriving` to `arrivals`
# - Renamed `visa_applications` to `applications`
# - Used the correct date column in `ts()`
# - Removed non-numeric columns from `predictors` before passing it into `auto.arima()`

# `auto.arima()` throws an error. Tried fixing it in multiple ways (see code below) to no avail
# Also tried reducing the number of lags, but that didn't help.

library(dplyr)
library(lubridate)
library(forecast)

# Assuming visaData is a dataframe containing weekly totals of visa data
visaData <- read.csv("data/visas-weekly-training.csv")

# Feature Engineering
visaData$v_acceptance_rate <- visaData$visas_issued / visaData$applications

visaData <- mutate(visaData,  
                   ukraine_family_scheme = ifelse(visa_type == "Ukraine Family Scheme", 1, 0),
                   ukraine_sponsorship_scheme = ifelse(visa_type == "Ukraine Sponsorship Scheme", 1, 0),
                   government_sponsored = ifelse(visa_type == "Government sponsored", 1, 0),
                   )

visaData$arrival_rate <- visaData$arrivals / visaData$visas_issued

# Create lagged features
lags <- 1:3
for (i in seq_along(lags)) {
  visaData <- visaData %>%
    mutate(!!paste0("lag_", lags[i], "_applications") := lag(visaData$applications, n = lags[i])) %>%
    mutate(!!paste0("lag_", lags[i], "_issued") := lag(visaData$visas_issued, n = lags[i])) %>%
    mutate(!!paste0("lag_", lags[i], "_arrival_rate") := lag(visaData$arrival_rate, n = lags[i])) %>%
    mutate(!!paste0("lag_", lags[i], "_acceptance_rate") := lag(visaData$v_acceptance_rate, n = lags[i]))
}

# Create moving averages
visaData <- visaData %>% mutate(ma_applications = rollmean(visaData$applications, 3, fill = NA))
visaData <- visaData %>% mutate(ma_issues = rollmean(visaData$visas_issued, 3, fill = NA))

# Using the transformed dataset to build a forecasting model
predictors <- visaData[!(colnames(visaData) %in% 'arrivals')]
arrivals_ts <- ts(visaData$arrivals, start = c(year(min(visaData$date)), 1), frequency = 52)

auto_arima_model <- auto.arima(arrivals_ts, xreg = predictors)
#--> Throws an error: xreg should be a numeric matrix or a numeric vector

# ---- Try removing non-numeric columns (e.g., date, visa_type) ----
predictors_numeric <- predictors %>%
  select(where(is.numeric)) %>%
  as.matrix()

auto_arima_model <- auto.arima(arrivals_ts, xreg = predictors_numeric)
#--> Throws an error: infinite or missing values in 'x'

# ---- Only use complete cases (no NA, NaN, or Inf) ----
# Find complete cases
valid_rows <- complete.cases(predictors_numeric) & 
              apply(predictors_numeric, 1, function(x) all(is.finite(x))) &
              is.finite(visaData$arrivals)

# Filter predictors and target
arrivals_ts_clean <- ts(visaData$arrivals[valid_rows], start = c(year(min(visaData$date[valid_rows])), 1), frequency = 52)
predictors_numeric_clean <- predictors_numeric[valid_rows, , drop = FALSE]

auto_arima_model <- auto.arima(arrivals_ts_clean, xreg = predictors_numeric_clean)
#--> Still throws an error: xreg is rank deficient

# ---- Try removing linearly dependent columns ----
library(caret)

# Remove columns with zero variance
nzv <- apply(predictors_numeric_clean, 2, function(x) var(x, na.rm = TRUE) != 0)
predictors_numeric_clean_fullrank <- predictors_numeric_clean[, nzv, drop = FALSE]

# Remove linearly dependent columns
combos <- findLinearCombos(predictors_numeric_clean_fullrank)
if (!is.null(combos$remove)) {
  predictors_numeric_clean_fullrank <- predictors_numeric_clean_fullrank[, -combos$remove, drop = FALSE]
}

auto_arima_model <- auto.arima(arrivals_ts_clean, xreg = predictors_numeric_clean_fullrank)
#--> Still throws an error: xreg is rank deficient

forecast(auto_arima_model, xreg = tail(predictors, 3))