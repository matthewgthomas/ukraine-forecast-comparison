Below is one example of how you might approach forecasting the number of incoming arrivals from Ukraine over the next 3 months (12 weeks). The method proceeds in three main steps:

1) Data Preparation and Feature Engineering  
2) Model Training (using a time series regression/ARIMAX)  
3) Forecasting the next 12 weeks  

--------------------------------------------------------------------------------
1) Data Preparation and Feature Engineering
--------------------------------------------------------------------------------

Given weekly data for:  
• visa_applications (weekly total)  
• visas_issued (weekly total)  
• visa_type (“Ukraine Family Scheme”, “Ukraine Sponsorship Scheme”, “Government-sponsored”)  
• arrivals (people with visas arriving per week)  

You can enrich the data with additional features that might help the model better predict arrivals:  

• Lagged Values:  
  – arrivals_lag1: the previous week’s arrivals.  
  – visas_issued_lag1, visas_issued_lag2, etc.  
  – visa_applications_lag1, etc.  

• Conversion Ratios:  
  – issue_rate = visas_issued / visa_applications (the proportion of applications that get converted into visas).  
  – arrival_rate = arrivals / visas_issued (the proportion of issued visas that lead to actual arrivals in a given week).  

• Cumulative/Backlog Features:  
  – backlog_of_issued = cumulative sum of visas_issued − cumulative sum of arrivals, indicating how many people hold a visa but have not arrived yet.  

• Visa Type "Shares":  
  – For each type of visa, compute the fraction of that visa type out of total visas issued. For instance, fraction_family = number of “Ukraine Family Scheme” visas / total visas issued in that week.  

Include (or exclude) features based on correlation with the target (arrivals) or by experimenting in model selection.

--------------------------------------------------------------------------------
2) Model Training
--------------------------------------------------------------------------------

A good baseline approach is a time series model (e.g., auto.arima in R) that incorporates exogenous regressors (the newly engineered features). This is often called ARIMAX (ARIMA with eXogenous variables).  

High-level steps:  
1. Convert your data (weekly) to a time series or tsibble.  
2. Split your data into a training set (all but the last N weeks) and test set (last N weeks) to assess performance. Alternatively, use time-series cross-validation.  
3. Fit a model (e.g., auto.arima with xreg = your chosen features).  
4. Evaluate the model using MAPE or RMSE on the test set.  
5. Once satisfied, refit the final model on all the available data and forecast forward 12 weeks.  

--------------------------------------------------------------------------------
3) Forecasting
--------------------------------------------------------------------------------

Use the trained model to produce forecasts of arrivals in future weeks. You will need forecasts (or assumptions) of your exogenous regressors (e.g., expected visa applications, expected visas_issued etc.) over the same forecast horizon. If you do not have direct forecasts for these regressors, you can model them separately or make reasoned assumptions (e.g. stable trend, seasonality) and feed them in.  

Below is an illustrative R script (using fictional data) that shows how you might implement an ARIMAX-based approach.

--------------------------------------------------------------------------------
Example R Script
--------------------------------------------------------------------------------

###############################################################################
# 1) Simulate or load your data
###############################################################################

# For demonstration, we will simulate a small, fictional weekly dataset.
# In practice, replace the simulated data with your actual dataset.

# Install/load packages as needed
# install.packages(c("forecast", "dplyr", "lubridate"))
library(dplyr)
library(forecast)
library(lubridate)

set.seed(123)

# Suppose we have 50 weeks of historical data
n_weeks <- 50
dates <- seq(ymd("2022-01-01"), by = "week", length.out = n_weeks)

# Simulated data
visa_applications <- rpois(n_weeks, lambda = 500)
visas_issued      <- round(0.8 * visa_applications + rnorm(n_weeks, sd = 30))
arrivals          <- round(0.6 * visas_issued       + rnorm(n_weeks, sd = 20))

# Simulate the proportion of each type of visa (they sum to 1 each week)
frac_family       <- runif(n_weeks, 0.3, 0.5)
frac_sponsorship  <- runif(n_weeks, 0.2, 0.4)
frac_gov          <- 1 - frac_family - frac_sponsorship

# Create a data frame
df <- data.frame(
  week = dates,
  visa_applications = visa_applications,
  visas_issued      = visas_issued,
  arrivals          = arrivals,
  frac_family       = frac_family,
  frac_sponsorship  = frac_sponsorship,
  frac_gov          = frac_gov
)

###############################################################################
# 2) Feature Engineering
###############################################################################

df <- df %>%
  mutate(
    # Example new features:
    issue_rate        = ifelse(visa_applications > 0,
                               visas_issued / visa_applications, 0),
    backlog_of_issued = cumsum(visas_issued) - cumsum(arrivals),
    arrivals_lag1     = dplyr::lag(arrivals, 1),
    visas_issued_lag1 = dplyr::lag(visas_issued, 1)
  ) %>%
  # Remove the first row(s) with NA from lagging
  filter(!is.na(arrivals_lag1) & !is.na(visas_issued_lag1))

# We'll forecast "arrivals" using these regressors:
#  xreg = cbind(issue_rate, backlog_of_issued, arrivals_lag1,
#               visas_issued_lag1, frac_family, frac_sponsorship, frac_gov)

###############################################################################
# 3) Convert to time series and split into train/test (optional)
###############################################################################

# Convert df to time series objects
# We'll use a weekly frequency. The forecast package typically uses ts() where
# frequency=52 or 53 can approximate weekly data.

start_week <- c(year(min(df$week)), week(min(df$week)))
freq <- 52  # approximate weekly frequency

# Our target variable
y <- ts(df$arrivals, start = start_week, frequency = freq)

# Exogenous regressors
X <- cbind(df$issue_rate,
           df$backlog_of_issued,
           df$arrivals_lag1,
           df$visas_issued_lag1,
           df$frac_family,
           df$frac_sponsorship,
           df$frac_gov)

# Split (optional): here we just use all data to demonstrate
train_size <- nrow(df) - 12  # keep last 12 observations for test
y_train <- window(y, end = c(year(df$week[train_size]),
                             week(df$week[train_size])))
y_test  <- window(y, start = c(year(df$week[train_size + 1]),
                               week(df$week[train_size + 1])))

X_train <- X[1:train_size, ]
X_test  <- X[(train_size+1):nrow(df), ]

###############################################################################
# 4) Fit ARIMAX model on the training set
###############################################################################

# auto.arima can select p, d, q automatically; we supply exogenous regressor X.
fit <- auto.arima(y_train, xreg = X_train, stepwise = FALSE, approximation = FALSE)

# Check model summary
summary(fit)

###############################################################################
# 5) Forecast on test set (optional performance check)
###############################################################################

# Forecast for the test period
fcast_test <- forecast(fit, xreg = X_test, h = length(y_test))

# Evaluate performance metrics on test
actual_test <- as.numeric(y_test)
pred_test   <- as.numeric(fcast_test$mean)

MAE_test  <- mean(abs(actual_test - pred_test))
RMSE_test <- sqrt(mean((actual_test - pred_test)^2))
MAPE_test <- mean(abs((actual_test - pred_test)/actual_test)) * 100

cat("Test MAE = ", MAE_test, "\n")
cat("Test RMSE = ", RMSE_test, "\n")
cat("Test MAPE = ", MAPE_test, "%\n")

###############################################################################
# 6) Refit on all available data and forecast the next 12 weeks
###############################################################################

# If satisfied, refit model on all data
fit_final <- auto.arima(y, xreg = X, stepwise = FALSE, approximation = FALSE)

# For the next 12 weeks, you need to predict (or assume) exogenous variables
# like future visa_applications, future visas_issued, fraction of visa types, etc.
# Sample approach: replicate the last row or assume stable patterns
future_weeks <- 12
X_future <- matrix(rep(X[nrow(X), ], future_weeks),
                   ncol = ncol(X), byrow = TRUE)

# Optionally modify X_future if you have better estimates
# E.g., X_future[ ,1] = your forecast of 'issue_rate' over next 12 weeks, etc.

# Forecast 12 weeks ahead
fcast_final <- forecast(fit_final, xreg = X_future, h = future_weeks)

# Print final forecast of arrivals
print(fcast_final)

################################################################################
# End of Example
################################################################################

Notes & Next Steps:
• In a real-world scenario, you would forecast or estimate the future exogenous variables (visa_applications, visas_issued, etc.), rather than simply replicating the last known historical values.  
• Evaluate multiple models (e.g., different ARIMAX, TBATS with regressors, or neural-network based models using the "forecast" or "fable" ecosystems) to see which model best fits the data.  
• Continuously update your model(s) with the latest weekly data and reassess performance.  

This general workflow – building meaningful features, fitting a time series model with exogenous regressors, and then forecasting – should give you a solid starting point for predicting the number of arrivals from Ukraine in the coming weeks.