# Sample structure of df:
# week: int or date
# applications: int
# issued: int
# arrivals: int
# visa_type: factor ("Ukraine Family Scheme", "Ukraine Sponsorship Scheme", "Government sponsored")

# Your data should look something like:
# df <- data.frame(
#   week = 1:20,
#   applications = ...,
#   issued = ...,
#   arrivals = ...,
#   visa_type = ...
# )

library(dplyr)
library(tidyr)
library(zoo)
library(ggplot2)

# 1. Feature engineering
df <- df %>%
  group_by(visa_type) %>%
  arrange(week) %>%
  mutate(
    issued_lag1 = lag(issued, 1),
    issued_lag2 = lag(issued, 2),
    issued_rolling2 = rollmean(issued, 2, fill = NA, align = "right"),
    applications_lag1 = lag(applications, 1),
    issued_to_applications = issued / applications,
    arrival_rate = arrivals / issued,               # Proportion of issued visas who arrived that week
    arrival_lag1 = lag(arrivals, 1)
  ) %>%
  ungroup()

# 2. Model typical lag from issued visas to arrivals
# For each visa_type, find the empirical arrival pattern after issue:
lag_distribution <- df %>%
  group_by(visa_type) %>%
  summarise(
    lag1 = mean(lag(arrivals, -1)/issued, na.rm = TRUE),  # Proportion arriving 1 week after issue
    lag2 = mean(lag(arrivals, -2)/issued, na.rm = TRUE),  # ...2 weeks after
    lag3 = mean(lag(arrivals, -3)/issued, na.rm = TRUE)
  )

# 3. Project future arrivals (simulate for next 12 weeks)
n_future <- 12
future_weeks <- (max(df$week) + 1):(max(df$week) + n_future)

# For simulation we'll use last available issuance, and trend or average (per visa type)
issued_avg <- df %>%
  group_by(visa_type) %>%
  summarise(issued_mean = mean(issued, na.rm = TRUE),
            issued_last = last(issued))

# Compute predicted arrivals per week per type
forecast <- expand.grid(week = future_weeks, visa_type = unique(df$visa_type)) %>%
  left_join(issued_avg, by = "visa_type") %>%
  left_join(lag_distribution, by = "visa_type")

# In this simulation, we assume issued visas per week is constant at last observed.
forecast <- forecast %>%
  mutate(
    # Use lag coefficients to project arrivals from issued
    pred_arrivals = issued_last * (lag1 + lag2 + lag3) / 3  # crude: average proportion per week
  )

# Sum over types to get total predicted arrivals per week
predicted <- forecast %>%
  group_by(week) %>%
  summarise(total_pred_arrivals = sum(pred_arrivals))

ggplot(predicted, aes(x = week, y = total_pred_arrivals)) +
  geom_line() +
  labs(title = "Predicted Ukraine Arrivals per Week (next 12 weeks)")

# Optional: output summed arrivals for 3 months
cat("Total predicted arrivals over next 3 months:", sum(predicted$total_pred_arrivals), "\n")