# Manual edits:
# - Loaded the correct data
# - Changed column names

# `auto.arima()` throws an error ("xreg is rank deficient"). Haven't bothered trying to fix it.

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
# n_weeks <- 50
# dates <- seq(ymd("2022-01-01"), by = "week", length.out = n_weeks)

# # Simulated data
# visa_applications <- rpois(n_weeks, lambda = 500)
# visas_issued      <- round(0.8 * visa_applications + rnorm(n_weeks, sd = 30))
# arrivals          <- round(0.6 * visas_issued       + rnorm(n_weeks, sd = 20))

# # Simulate the proportion of each type of visa (they sum to 1 each week)
# frac_family       <- runif(n_weeks, 0.3, 0.5)
# frac_sponsorship  <- runif(n_weeks, 0.2, 0.4)
# frac_gov          <- 1 - frac_family - frac_sponsorship

# # Create a data frame
# df <- data.frame(
#   week = dates,
#   visa_applications = visa_applications,
#   visas_issued      = visas_issued,
#   arrivals          = arrivals,
#   frac_family       = frac_family,
#   frac_sponsorship  = frac_sponsorship,
#   frac_gov          = frac_gov
# )

df <- read.csv("data/visas-weekly-training.csv")
df <- rename(df, week = date)
df$week <- as.Date(df$week)

# Calculate the proportions of visa applications/issued and arrivals for each type of visa
df <- df |> 
  tidyr::pivot_wider(
    names_from = visa_type, 
    values_from = c(applications, visas_issued, arrivals),
    values_fill = 0
  ) |> 

  group_by(week) |> 
  summarise(
    applications =  `applications_Ukraine Family Scheme` + `applications_Government sponsored` + `applications_Ukraine Sponsorship Scheme`,
    visas_issued = `visas_issued_Ukraine Family Scheme` + `visas_issued_Ukraine Sponsorship Scheme` + `visas_issued_Government sponsored`,
    arrivals = `arrivals_Ukraine Family Scheme` + `arrivals_Ukraine Sponsorship Scheme` + `arrivals_Government sponsored`,

    frac_family = `visas_issued_Ukraine Family Scheme` / visas_issued,
    frac_sponsorship = `visas_issued_Ukraine Sponsorship Scheme` / visas_issued,
    frac_gov = `visas_issued_Government sponsored` / visas_issued
  )



###############################################################################
# 2) Feature Engineering
###############################################################################

df <- df %>%
  mutate(
    # Example new features:
    issue_rate        = ifelse(applications > 0,
                               visas_issued / applications, 0),
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
#--> Throwing an error: xreg is rank deficient

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
