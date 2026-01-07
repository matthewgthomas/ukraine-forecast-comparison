# Numerical simulation of arrivals using weekly visa data
# - Simulate future applications, visas issued, and arrivals by visa type
# - Produce 12-week ahead (3 month) forecast distributions

library(tidyverse)
library(lubridate)
library(forecast)

set.seed(42)

horizon_weeks <- 12
n_sims <- 2000

visa_data <- read_csv("data/visas-weekly.csv", show_col_types = FALSE) |>
  mutate(date = ymd(date)) |>
  arrange(visa_type, date) |>
  group_by(visa_type) |>
  mutate(
    lag_1_arrivals = lag(arrivals, 1),
    lag_4_arrivals = lag(arrivals, 4),
    rolling_4wk_apps = zoo::rollmean(applications, k = 4, fill = NA, align = "right"),
    rolling_4wk_issued = zoo::rollmean(visas_issued, k = 4, fill = NA, align = "right"),
    rolling_4wk_arrivals = zoo::rollmean(arrivals, k = 4, fill = NA, align = "right"),
    rate_issue = if_else(applications > 0, visas_issued / applications, NA_real_),
    rate_arrive = if_else(visas_issued > 0, arrivals / visas_issued, NA_real_)
  ) |>
  ungroup()

latest_date <- max(visa_data$date, na.rm = TRUE)
future_dates <- latest_date + weeks(seq_len(horizon_weeks))

safe_beta_params <- function(x) {
  x <- x[is.finite(x) & x > 0 & x < 1]
  if (length(x) < 5) {
    return(c(alpha = 2, beta = 2))
  }
  m <- mean(x)
  v <- var(x)
  if (!is.finite(v) || v <= 0 || m <= 0 || m >= 1) {
    return(c(alpha = 2, beta = 2))
  }
  common <- m * (1 - m) / v - 1
  if (!is.finite(common) || common <= 0) {
    return(c(alpha = 2, beta = 2))
  }
  c(alpha = m * common, beta = (1 - m) * common)
}

simulate_type <- function(type_data) {
  type_name <- unique(type_data$visa_type)
  apps_ts <- ts(type_data$applications, frequency = 52)

  apps_model <- auto.arima(apps_ts)
  apps_forecast <- forecast(apps_model, h = horizon_weeks)

  issue_params <- safe_beta_params(type_data$rate_issue)
  arrive_params <- safe_beta_params(type_data$rate_arrive)

  sim_results <- map_dfr(seq_len(n_sims), function(sim_id) {
    apps_sim <- rnorm(
      horizon_weeks,
      mean = as.numeric(apps_forecast$mean),
      sd = as.numeric(apps_forecast$se)
    ) |>
      round() |>
      pmax(0)

    issue_rates <- rbeta(horizon_weeks, issue_params["alpha"], issue_params["beta"])
    issued_sim <- rbinom(horizon_weeks, size = apps_sim, prob = issue_rates)

    arrive_rates <- rbeta(horizon_weeks, arrive_params["alpha"], arrive_params["beta"])
    arrivals_sim <- rbinom(horizon_weeks, size = issued_sim, prob = arrive_rates)

    tibble(
      simulation = sim_id,
      visa_type = type_name,
      date = future_dates,
      applications = apps_sim,
      visas_issued = issued_sim,
      arrivals = arrivals_sim
    )
  })

  sim_results
}

simulations <- visa_data |>
  group_by(visa_type) |>
  group_split() |>
  map_dfr(simulate_type)

weekly_summary <- simulations |>
  group_by(date, visa_type) |>
  summarise(
    arrivals_median = median(arrivals),
    arrivals_p10 = quantile(arrivals, 0.1),
    arrivals_p90 = quantile(arrivals, 0.9),
    .groups = "drop"
  )

total_summary <- simulations |>
  group_by(simulation, date) |>
  summarise(total_arrivals = sum(arrivals), .groups = "drop") |>
  group_by(date) |>
  summarise(
    total_arrivals_median = median(total_arrivals),
    total_arrivals_p10 = quantile(total_arrivals, 0.1),
    total_arrivals_p90 = quantile(total_arrivals, 0.9),
    .groups = "drop"
  )

write_csv(weekly_summary, "data/forecast-simulation-by-scheme.csv")
write_csv(total_summary, "data/forecast-simulation-total.csv")
write_rds(simulations, "data/forecast-simulation-samples.rds")
