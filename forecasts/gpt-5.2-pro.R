# Install if needed:
# install.packages(c("dplyr","tidyr","lubridate","tsibble","fable","feasts","ggplot2"))

library(dplyr)
library(tidyr)
library(lubridate)
library(tsibble)
library(fable)
library(feasts)

logit <- function(p) log(p / (1 - p))
inv_logit <- function(x) 1 / (1 + exp(-x))

forecast_ukraine_arrivals <- function(df,
                                      h_weeks = 13,     # ~ 3 months
                                      nsim   = 2000,   # simulation paths for intervals
                                      seed   = 123) {

  stopifnot(all(c("week","visa_type","applications","visas_issued","arrivals") %in% names(df)))

  # --- 1) Make a regular weekly tsibble (one series per visa_type) ---
  df_ts <- df %>%
    mutate(
      week = tsibble::yearweek(as.Date(week)),
      visa_type = as.character(visa_type),
      applications = as.numeric(applications),
      visas_issued = as.numeric(visas_issued),
      arrivals = as.numeric(arrivals)
    ) %>%
    arrange(visa_type, week) %>%
    as_tsibble(key = visa_type, index = week) %>%
    fill_gaps() %>%
    mutate(across(c(applications, visas_issued, arrivals), ~replace_na(.x, 0)))

  # --- 2) Feature engineering: backlog stocks + smoothed propensities ---
  df_feat <- df_ts %>%
    group_by(visa_type) %>%
    arrange(week) %>%
    mutate(
      # Application backlog A_t = A_{t-1} + Apps_t - Issued_t
      bal_app = cumsum(applications - visas_issued),
      init_app_backlog = pmax(0, -min(bal_app, na.rm = TRUE)), # minimal to keep backlog >= 0
      app_backlog_end = init_app_backlog + bal_app,
      app_backlog_start = lag(app_backlog_end, default = init_app_backlog),
      app_available = app_backlog_start + applications,

      # Issuance propensity q_t = Issued_t / (A_{t-1} + Apps_t)
      # Jeffreys smoothing avoids exact 0/1 and stabilises early weeks
      issue_rate = (visas_issued + 0.5) / (app_available + 1),
      issue_rate = pmin(pmax(issue_rate, 1e-4), 1 - 1e-4),
      issue_logit = logit(issue_rate),

      # Visa-holder stock V_t = V_{t-1} + Issued_t - Arrivals_t
      bal_visa = cumsum(visas_issued - arrivals),
      init_visa_stock = pmax(0, -min(bal_visa, na.rm = TRUE)),
      visa_stock_end = init_visa_stock + bal_visa,
      visa_stock_start = lag(visa_stock_end, default = init_visa_stock),
      visa_available = visa_stock_start + visas_issued,

      # Arrival propensity p_t = Arrivals_t / (V_{t-1} + Issued_t)
      arrival_rate = (arrivals + 0.5) / (visa_available + 1),
      arrival_rate = pmin(pmax(arrival_rate, 1e-4), 1 - 1e-4),
      arrival_logit = logit(arrival_rate),

      # Transform for applications model
      log1p_applications = log1p(applications)
    ) %>%
    ungroup()

  # Last observed pipeline states per type (end-of-week)
  state_last <- df_feat %>%
    group_by(visa_type) %>%
    slice_max(order_by = week, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    select(visa_type, app_backlog_end, visa_stock_end)

  # --- 3) Fit models ---
  # We fit ARIMA and ETS then pick the best by AICc (per visa_type).
  # (You can force ARIMA-only by removing ETS if you prefer.)
  app_models <- df_feat %>%
    model(
      ARIMA = ARIMA(log1p_applications),
      ETS   = ETS(log1p_applications)
    ) %>%
    select_best("AICc")

  issue_models <- df_feat %>%
    model(
      ARIMA = ARIMA(issue_logit),
      ETS   = ETS(issue_logit)
    ) %>%
    select_best("AICc")

  arrive_models <- df_feat %>%
    model(
      ARIMA = ARIMA(arrival_logit),
      ETS   = ETS(arrival_logit)
    ) %>%
    select_best("AICc")

  # --- 4) Simulate future paths for uncertainty ---
  set.seed(seed)

  apps_sim <- app_models %>%
    generate(h = h_weeks, times = nsim, bootstrap = TRUE) %>%
    as_tibble() %>%
    transmute(
      visa_type,
      week,
      .sim,
      applications = pmax(expm1(log1p_applications), 0)
    )

  issue_sim <- issue_models %>%
    generate(h = h_weeks, times = nsim, bootstrap = TRUE) %>%
    as_tibble() %>%
    transmute(
      visa_type,
      week,
      .sim,
      q_issue = inv_logit(issue_logit)
    )

  arrive_sim <- arrive_models %>%
    generate(h = h_weeks, times = nsim, bootstrap = TRUE) %>%
    as_tibble() %>%
    transmute(
      visa_type,
      week,
      .sim,
      p_arrive = inv_logit(arrival_logit)
    )

  sim_inputs <- apps_sim %>%
    left_join(issue_sim,  by = c("visa_type","week",".sim")) %>%
    left_join(arrive_sim, by = c("visa_type","week",".sim")) %>%
    left_join(state_last, by = "visa_type") %>%
    arrange(visa_type, .sim, week)

  # --- 5) Push each simulation through the pipeline (stock-flow recursion) ---
  sim_paths <- sim_inputs %>%
    group_by(visa_type, .sim) %>%
    group_modify(~{
      backlog <- .x$app_backlog_end[1]   # A_T
      stock   <- .x$visa_stock_end[1]    # V_T

      H <- nrow(.x)
      issued   <- numeric(H)
      arrivals <- numeric(H)

      for (i in seq_len(H)) {
        # Applications -> Issued
        avail_apps <- backlog + .x$applications[i]
        issued[i]  <- .x$q_issue[i] * avail_apps
        backlog    <- avail_apps - issued[i]

        # Issued -> Arrivals
        avail_visas <- stock + issued[i]
        arrivals[i] <- .x$p_arrive[i] * avail_visas
        stock       <- avail_visas - arrivals[i]
      }

      tibble(
        week = .x$week,
        applications = .x$applications,
        visas_issued = issued,
        arrivals = arrivals
      )
    }) %>%
    ungroup()

  # --- 6) Summarise into point forecast + intervals ---
  fc_by_type <- sim_paths %>%
    group_by(visa_type, week) %>%
    summarise(
      arrivals = median(arrivals),
      lo80 = quantile(arrivals, 0.10),
      hi80 = quantile(arrivals, 0.90),
      lo95 = quantile(arrivals, 0.025),
      hi95 = quantile(arrivals, 0.975),
      .groups = "drop"
    )

  total_sim <- sim_paths %>%
    group_by(.sim, week) %>%
    summarise(arrivals = sum(arrivals), .groups = "drop")

  fc_total <- total_sim %>%
    group_by(week) %>%
    summarise(
      visa_type = "Total",
      arrivals = median(arrivals),
      lo80 = quantile(arrivals, 0.10),
      hi80 = quantile(arrivals, 0.90),
      lo95 = quantile(arrivals, 0.025),
      hi95 = quantile(arrivals, 0.975),
      .groups = "drop"
    )

  list(
    data_with_features = df_feat,
    forecast_by_type   = fc_by_type,
    forecast_total     = fc_total
  )
}
