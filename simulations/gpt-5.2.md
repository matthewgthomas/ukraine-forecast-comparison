```r
# Numerical simulation (Monte Carlo) of arrivals over the next 3 months (~13 weeks)
# using only weekly totals of:
#  - visa applications
#  - visas issued
#  - visa type
#  - arrivals
#
# Core idea (per visa type):
#   applications  --(processing delay distribution)-->  issued
#   issued        --(travel delay distribution)----->  arrivals
#
# Both delay distributions are estimated from aggregated weekly data via
# non-negative least squares (NNLS) on lagged regressors (a discrete convolution).
# Future applications are forecast with ARIMA on log1p counts, then we propagate
# uncertainty via Negative Binomial sampling for issued/arrivals.

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(lubridate)
  library(forecast)
  library(nnls)
  library(MASS)
  library(purrr)
})

#------------------------------- helpers ---------------------------------------

lag_matrix <- function(x, L) {
  # Returns n x (L+1) matrix with columns lag0..lagL (lag0 = current week)
  n <- length(x)
  A <- sapply(0:L, function(l) dplyr::lag(x, l))
  A <- as.matrix(A)
  colnames(A) <- paste0("lag", 0:L)
  A
}

estimate_delay_weights_nnls <- function(x, y, L) {
  # Fit y_t ≈ sum_{l=0..L} w_l * x_{t-l}, with w_l >= 0
  stopifnot(length(x) == length(y))
  A <- lag_matrix(x, L)

  # Use only rows where all lags are observed
  idx <- (L + 1):length(y)
  A_fit <- A[idx, , drop = FALSE]
  y_fit <- y[idx]

  # Replace any remaining NA (shouldn't be present after idx) defensively
  A_fit[is.na(A_fit)] <- 0

  fit <- nnls::nnls(A_fit, y_fit)
  w <- as.numeric(coef(fit))
  names(w) <- colnames(A_fit)

  # Fitted mean for all time points (first L weeks are partial -> treat NA lags as 0)
  A0 <- A
  A0[is.na(A0)] <- 0
  mu_all <- as.numeric(A0 %*% w)

  list(weights = w, fitted = mu_all, nnls = fit)
}

fit_nb_theta <- function(y, mu) {
  # Estimate Negative Binomial theta given known mean mu_t.
  # Model: y_t ~ NB(mean = mu_t, theta)
  df <- data.frame(y = as.integer(pmax(0, round(y))), mu = pmax(mu, 1e-6))
  df <- df[is.finite(df$y) & is.finite(df$mu) & df$mu > 0, ]
  if (nrow(df) < 20) return(Inf)  # fall back to Poisson-ish

  # Offset-only NB fit: log(E[y]) = log(mu)
  fit <- try(MASS::glm.nb(y ~ 0 + offset(log(mu)), data = df), silent = TRUE)
  if (inherits(fit, "try-error")) return(Inf)
  as.numeric(fit$theta)
}

simulate_from_arima_log1p <- function(x, h, n_sims, frequency = 52) {
  # Fit ARIMA on log1p(x) and simulate future paths on that scale, then invert.
  x_ts <- ts(log1p(pmax(x, 0)), frequency = frequency)
  fit <- forecast::auto.arima(x_ts, seasonal = TRUE)

  sims <- replicate(n_sims, {
    sim_log <- as.numeric(simulate(fit, nsim = h, future = TRUE))
    pmax(0L, as.integer(round(expm1(sim_log))))
  })

  # sims is h x n_sims; return n_sims x h for convenience
  t(sims)
}

#--------------------------- main simulation -----------------------------------

simulate_ukraine_arrivals <- function(data,
                                      horizon_weeks = 13,
                                      n_sims = 5000,
                                      max_lag_processing = 8,
                                      max_lag_travel = 8,
                                      recent_window = NULL,   # e.g. 52 to fit delays on last year only
                                      seed = 1) {
  set.seed(seed)

  required <- c("week", "visa_type", "applications", "issued", "arrivals")
  stopifnot(all(required %in% names(data)))

  df <- data %>%
    mutate(
      week = as.Date(week),
      visa_type = as.character(visa_type),
      applications = as.numeric(applications),
      issued = as.numeric(issued),
      arrivals = as.numeric(arrivals)
    ) %>%
    arrange(visa_type, week)

  # Complete weekly grid per type (fill missing weeks with zeros)
  all_types <- sort(unique(df$visa_type))
  min_week <- min(df$week)
  max_week <- max(df$week)
  full_weeks <- seq(min_week, max_week, by = "7 days")

  dfc <- df %>%
    group_by(visa_type) %>%
    tidyr::complete(week = full_weeks,
                    fill = list(applications = 0, issued = 0, arrivals = 0)) %>%
    ungroup() %>%
    arrange(visa_type, week)

  last_week <- max(dfc$week)
  future_weeks <- seq(last_week + 7, by = "7 days", length.out = horizon_weeks)

  # Fit per type: delay weights + dispersion + applications forecast model
  per_type_models <- map(all_types, function(vt) {
    d <- dfc %>% filter(visa_type == vt) %>% arrange(week)

    if (!is.null(recent_window) && recent_window < nrow(d)) {
      d_fit <- tail(d, recent_window)
      # But keep full history for sim convolution base; delays estimated on recent
      d_full <- d
    } else {
      d_fit <- d
      d_full <- d
    }

    # Estimate processing delay: applications -> issued
    proc <- estimate_delay_weights_nnls(
      x = d_fit$applications,
      y = d_fit$issued,
      L = max_lag_processing
    )

    # Estimate travel delay: issued -> arrivals
    trav <- estimate_delay_weights_nnls(
      x = d_fit$issued,
      y = d_fit$arrivals,
      L = max_lag_travel
    )

    # Dispersion (theta) for NB around fitted means (on the fit window)
    proc_theta <- fit_nb_theta(d_fit$issued, proc$fitted)
    trav_theta <- fit_nb_theta(d_fit$arrivals, trav$fitted)

    # Applications simulator (use full history of applications)
    apps_vec <- d_full$applications

    list(
      visa_type = vt,
      history = d_full,
      proc_w = proc$weights,
      trav_w = trav$weights,
      proc_theta = proc_theta,
      trav_theta = trav_theta,
      apps_history = apps_vec
    )
  })
  names(per_type_models) <- all_types

  # Run Monte Carlo
  sim_arrivals_by_type <- list()
  sim_arrivals_total <- matrix(0L, nrow = n_sims, ncol = horizon_weeks)

  for (vt in all_types) {
    mod <- per_type_models[[vt]]
    d <- mod$history
    Tn <- nrow(d)

    # Simulate future applications (n_sims x horizon_weeks)
    apps_future_sims <- simulate_from_arima_log1p(
      x = mod$apps_history,
      h = horizon_weeks,
      n_sims = n_sims,
      frequency = 52
    )

    # Allocate result matrices
    arrivals_future <- matrix(0L, nrow = n_sims, ncol = horizon_weeks)

    # Precompute lag lengths
    Lp <- max_lag_processing
    Lt <- max_lag_travel
    wP <- as.numeric(mod$proc_w)  # length Lp+1
    wT <- as.numeric(mod$trav_w)  # length Lt+1

    # For each simulation path, propagate pipeline forward
    for (s in seq_len(n_sims)) {

      apps_full <- c(d$applications, apps_future_sims[s, ])

      issued_full <- c(d$issued, rep(NA_integer_, horizon_weeks))
      arr_full   <- c(d$arrivals, rep(NA_integer_, horizon_weeks))

      # Track in-transit "stock" for a hard constraint: cumulative arrivals <= cumulative issued
      # (This is a minimal constraint; the delay convolution still drives the timing.)
      stock <- cumsum(issued_full[1:Tn]) - cumsum(arr_full[1:Tn])
      stock_now <- stock[Tn]
      if (!is.finite(stock_now)) stock_now <- 0
      stock_now <- max(0, stock_now)

      # Simulate issued and arrivals week by week
      for (h in seq_len(horizon_weeks)) {
        t <- Tn + h  # index in full series

        # Mean issued based on lagged applications (convolution)
        mu_issue <- 0
        for (l in 0:Lp) {
          idx <- t - l
          if (idx >= 1) mu_issue <- mu_issue + wP[l + 1] * apps_full[idx]
        }
        mu_issue <- max(mu_issue, 0)

        issued_t <- if (is.finite(mod$proc_theta) && mod$proc_theta < Inf) {
          rnbinom(1, mu = mu_issue, size = mod$proc_theta)
        } else {
          rpois(1, lambda = mu_issue)
        }
        issued_t <- max(0L, as.integer(issued_t))
        issued_full[t] <- issued_t

        # Mean arrivals based on lagged issued (convolution)
        mu_arr <- 0
        for (l in 0:Lt) {
          idx <- t - l
          if (idx >= 1) mu_arr <- mu_arr + wT[l + 1] * issued_full[idx]
        }
        mu_arr <- max(mu_arr, 0)

        arr_t <- if (is.finite(mod$trav_theta) && mod$trav_theta < Inf) {
          rnbinom(1, mu = mu_arr, size = mod$trav_theta)
        } else {
          rpois(1, lambda = mu_arr)
        }
        arr_t <- max(0L, as.integer(arr_t))

        # Hard feasibility constraint: can't arrive more people than have been issued minus already arrived
        stock_now <- stock_now + issued_t
        if (arr_t > stock_now) arr_t <- stock_now
        stock_now <- stock_now - arr_t

        arr_full[t] <- arr_t
        arrivals_future[s, h] <- arr_t
      }
    }

    sim_arrivals_by_type[[vt]] <- arrivals_future
    sim_arrivals_total <- sim_arrivals_total + arrivals_future
  }

  summarise_sims <- function(sim_mat, weeks) {
    tibble(
      week = weeks,
      mean = as.numeric(colMeans(sim_mat)),
      p05 = as.numeric(apply(sim_mat, 2, quantile, 0.05)),
      p50 = as.numeric(apply(sim_mat, 2, quantile, 0.50)),
      p95 = as.numeric(apply(sim_mat, 2, quantile, 0.95))
    )
  }

  # Build summary outputs
  summary_total <- summarise_sims(sim_arrivals_total, future_weeks)

  summary_by_type <- map_dfr(names(sim_arrivals_by_type), function(vt) {
    summarise_sims(sim_arrivals_by_type[[vt]], future_weeks) %>%
      mutate(visa_type = vt, .before = 1)
  })

  list(
    horizon_weeks = horizon_weeks,
    n_sims = n_sims,
    future_weeks = future_weeks,
    models = per_type_models,
    sims_arrivals_by_type = sim_arrivals_by_type,  # each is n_sims x horizon_weeks
    sims_arrivals_total = sim_arrivals_total,      # n_sims x horizon_weeks
    summary_by_type = summary_by_type,
    summary_total = summary_total
  )
}

#----------------------------- example usage -----------------------------------
# Your input data must look like:
# df <- tibble(
#   week = as.Date(...),                     # week start date
#   visa_type = c("Ukraine Family Scheme", "Ukraine Sponsorship Scheme", "Government sponsored"),
#   applications = ..., issued = ..., arrivals = ...
# )
#
# res <- simulate_ukraine_arrivals(
#   data = df,
#   horizon_weeks = 13,      # ~ next 3 months
#   n_sims = 5000,
#   max_lag_processing = 10, # tune based on your observed delays
#   max_lag_travel = 10,
#   recent_window = 52,      # optional: estimate delays using last year only
#   seed = 42
# )
#
# res$summary_total
# res$summary_by_type
#
# # Total arrivals distribution over entire horizon:
# total_13w <- rowSums(res$sims_arrivals_total)
# quantile(total_13w, c(0.05, 0.5, 0.95))
```

If you paste a small sample of your data (a few rows per visa type, including column names), I can adapt the function to your exact schema (e.g., whether “week” is week-ending vs week-starting, how missing weeks are represented, and how many weeks of history you have) and suggest reasonable `max_lag_processing` / `max_lag_travel` values based on the empirical cross-correlation.