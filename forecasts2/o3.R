############################################################
## Ukraine-arrivals forecast – minimal, reproducible script
############################################################
library(tidyverse)
library(lubridate)
library(forecast)     # ARIMA / Arima()
library(tsibble)      # to keep the index tidy
library(fable.tools)  # contains new_data()

## ---------------------------------------------------------
## 0. Load data  (expected structure shown below)
## ---------------------------------------------------------
## Columns: week_end  scheme  applications  issued  arrivals
## One row per (week,scheme).  If your file is different,
## adapt the read_*() call and the names in mutate() below.
df_raw <- read_csv("ukr_visas_weekly.csv",
                   col_types = cols(
                     week_end     = col_date(),
                     scheme       = col_factor(),
                     applications = col_integer(),
                     issued       = col_integer(),
                     arrivals     = col_integer())
                   )

## ---------------------------------------------------------
## 1. Collapse to total numbers (sum over the 3 schemes)
## ---------------------------------------------------------
df_tot <- df_raw %>%
  group_by(week_end) %>%
  summarise(across(c(applications, issued, arrivals), sum), .groups = "drop") %>%
  arrange(week_end) %>%
  mutate(week = yearweek(week_end)) %>%   # nice weekly index
  as_tsibble(index = week)

## ---------------------------------------------------------
## 2. Feature engineering
##    a) Visa backlog
##    b) Lagged visas-issued (1 and 2 weeks; can add more)
## ---------------------------------------------------------
df_tot <- df_tot %>%
  mutate(backlog = cumsum(issued) - cumsum(arrivals),
         issued_lag1 = lag(issued,  1, default = 0),
         issued_lag2 = lag(issued,  2, default = 0),
         backlog_lag1 = lag(backlog, 1, default = 0))

## ---------------------------------------------------------
## 3. Split last 5 weeks off for a (tiny) test set
## ---------------------------------------------------------
test_weeks <- tail(df_tot$week, 5)
train <- df_tot %>% filter(!(week %in% test_weeks))
test  <- df_tot %>% filter( week %in% test_weeks)

## ---------------------------------------------------------
## 4. Model visas-issued so we can extrapolate it
## ---------------------------------------------------------
fit_issued <- Arima(train$issued, order = c(1,0,1))   # or auto.arima()
fc_issued  <- forecast(fit_issued, h = 13)
## Put the forecasts back into a tsibble
issued_future <- new_data(train, 13) %>%
  mutate(issued      = as.numeric(fc_issued$mean),
         issued_lag1 = lag(issued, 1, default = tail(train$issued, 1)),
         issued_lag2 = lag(issued, 2, default = tail(train$issued, 2)[1]))

## ---------------------------------------------------------
## 5. Fit dynamic-regression (ARIMAX) for arrivals
## ---------------------------------------------------------
xreg_train <- as.matrix(train %>% select(backlog_lag1, issued_lag1, issued_lag2))
fit_arr <- Arima(train$arrivals,
                 xreg  = xreg_train,
                 order = c(1,0,0))      # keep it simple with only 22 obs
summary(fit_arr)

## ---------------------------------------------------------
## 6. One-step-ahead test accuracy (sanity check)
## ---------------------------------------------------------
xreg_test <- as.matrix(test %>% select(backlog_lag1, issued_lag1, issued_lag2))
acc       <- accuracy(forecast(fit_arr, xreg = xreg_test, h = 5), test$arrivals)
print(acc)

## ---------------------------------------------------------
## 7. Create forecasts for arrivals 13 weeks ahead
##    We need backlog_t-1 inside the horizon, so recurse:
## ---------------------------------------------------------
h <- 13
backlog_sim   <- numeric(h + 1)
arrival_fc    <- numeric(h)
## initialise with last observed backlog
backlog_sim[1] <- tail(train$backlog, 1)

for (i in 1:h) {
  ## exogenous regressors for step i
  x_i <- cbind(backlog_lag1 = backlog_sim[i],
               issued_lag1  = issued_future$issued_lag1[i],
               issued_lag2  = issued_future$issued_lag2[i])
  ## 1-step forecast from the regression
  arrival_fc[i] <- forecast(fit_arr, xreg = x_i, h = 1)$mean
  ## update backlog
  backlog_sim[i + 1] <- backlog_sim[i] + issued_future$issued[i] - arrival_fc[i]
}

arrival_future <- issued_future %>%
  select(week) %>%
  mutate(arrivals_fc = arrival_fc,
         backlog_fc  = backlog_sim[-1])   # drop initial value

## ---------------------------------------------------------
## 8. Combine history and forecast; aggregate over 3 months
## ---------------------------------------------------------
all_arrivals <- bind_rows(
  df_tot %>% select(week, arrivals) %>% rename(value = arrivals) %>% mutate(type = "history"),
  arrival_future %>% select(week, value = arrivals_fc) %>% mutate(type = "forecast")
)

three_month_total <- sum(arrival_future$arrivals_fc)
cat("\nProjected arrivals over the next 13 weeks ≈ 3 months:",
    round(three_month_total), "\n")

## Optional: quick plot
library(ggplot2)
ggplot(all_arrivals,
       aes(x = week, y = value, colour = type)) +
  geom_line() + geom_point() +
  labs(y = "People arriving", x = "Week",
       title = "Observed and forecast Ukrainian arrivals") +
  theme_minimal()