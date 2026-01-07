### Method (works with only the 4 weekly series you have)

You can treat the system as a **pipeline**:

1. **Applications → Visas issued** (with a short lag)
2. **Visas issued → Arrivals** (with a longer, distributed lag)

Because you only have **22 weeks**, you want a **parsimonious** approach that:
- uses visa **type** separately (different delays/behaviours),
- learns the **delay pattern** from “issued” to “arrivals”,
- forecasts “applications” and “issued” forward, then converts those into “arrivals”.

#### Key engineered features (derived from your data)
For each visa type, engineer:
- **Lagged applications** as predictors of issued visas: `apps_t, apps_{t-1}, apps_{t-2}` (on log scale)
- **A learned “arrival-delay distribution”**: weights \(w_0,\dots,w_K\) such that  
  \[
  arrivals_t \approx \sum_{k=0}^{K} w_k \cdot issued_{t-k}
  \]
  with constraints \(w_k \ge 0\) and \(\sum w_k \le 1\).  
  This is a very data-efficient way to encode the (unknown) time-to-travel delay using only weekly aggregates.

#### Forecasting steps (per visa type)
1. Fit an ARIMA model to `log1p(applications)` to forecast applications 13 weeks ahead.
2. Fit an ARIMAX model to `log1p(issued)` using lagged `log1p(applications)` as regressors; forecast issued 13 weeks ahead.
3. Estimate delay weights \(w\) from historical `issued` → `arrivals` using **non-negative least squares (NNLS)** on lagged issued.
4. Convert forecast issued into forecast arrivals by **convolution** with the learned weights.
5. Use **Monte Carlo simulation** to produce prediction intervals:
   - simulate future applications paths,
   - simulate issued conditional on simulated applications,
   - convert issued to arrivals via the delay weights,
   - (optionally) add Poisson noise to represent arrival-count randomness.

Finally, sum across the 3 visa types to get the total arrivals forecast.

---

## R implementation

Assume your data frame `df` has columns:

- `week` (Date, week start)
- `type` (character/factor: `"Ukraine Family Scheme"`, `"Ukraine Sponsorship Scheme"`, `"Government sponsored"`)
- `applications` (weekly total)
- `issued` (weekly total)
- `arrivals` (weekly total)

```r
# Packages
library(dplyr)
library(tidyr)
library(lubridate)
library(forecast)
library(nnls)

#----------------------------
# Helpers
#----------------------------

prepare_weekly <- function(df) {
  df %>%
    mutate(
      week = as.Date(week),
      type = as.character(type),
      applications = pmax(0, as.numeric(applications)),
      issued        = pmax(0, as.numeric(issued)),
      arrivals      = pmax(0, as.numeric(arrivals))
    ) %>%
    group_by(type, week) %>%
    summarise(
      applications = sum(applications),
      issued = sum(issued),
      arrivals = sum(arrivals),
      .groups = "drop"
    ) %>%
    complete(
      type,
      week = seq(min(week), max(week), by = "week"),
      fill = list(applications = 0, issued = 0, arrivals = 0)
    ) %>%
    arrange(type, week)
}

make_xreg_lags <- function(x, L) {
  # x: vector length n
  n <- length(x)
  m <- sapply(0:L, function(k) c(rep(NA, k), x[1:(n - k)]))
  colnames(m) <- paste0("app_lag", 0:L)
  m
}

make_future_xreg <- function(apps_log_hist, apps_log_future, L) {
  apps_path <- c(apps_log_hist, apps_log_future)
  n <- length(apps_log_hist)
  H <- length(apps_log_future)

  xreg_f <- matrix(NA_real_, nrow = H, ncol = L + 1)
  for (h in 1:H) {
    t <- n + h
    xreg_f[h, ] <- apps_path[t - (0:L)]
  }
  colnames(xreg_f) <- paste0("app_lag", 0:L)
  xreg_f
}

estimate_delay_weights_nnls <- function(issued, arrivals, K = 6) {
  # arrivals_t ~ sum_{k=0..K} w_k * issued_{t-k}, w_k >= 0, sum(w) <= 1
  n <- length(issued)
  if (n <= K + 4 || sum(issued) == 0 || sum(arrivals) == 0) {
    # fallback: no signal
    w <- rep(0, K + 1)
    return(list(w = w, residuals = arrivals * 0))
  }

  idx <- (K + 1):n
  y <- arrivals[idx]
  X <- sapply(0:K, function(k) issued[idx - k])

  fit <- nnls::nnls(X, y)
  w <- pmax(as.numeric(coef(fit)), 0)

  # enforce sum(w) <= 1 (cap; do not force to equal 1)
  s <- sum(w)
  if (s > 1) w <- w / s

  res <- as.numeric(y - X %*% w)
  list(w = w, residuals = res)
}

safe_auto_arima <- function(y, xreg = NULL) {
  # Keep models simple due to 22 points
  auto.arima(
    y,
    xreg = xreg,
    seasonal = FALSE,
    max.p = 2, max.q = 2, max.d = 1,
    stepwise = TRUE, approximation = FALSE
  )
}

summarise_sims <- function(sim_mat, probs = c(0.05, 0.2, 0.8, 0.95)) {
  # sim_mat: H x nsim
  out <- tibble(
    mean = rowMeans(sim_mat),
    q05  = apply(sim_mat, 1, quantile, probs = probs[1]),
    q20  = apply(sim_mat, 1, quantile, probs = probs[2]),
    q80  = apply(sim_mat, 1, quantile, probs = probs[3]),
    q95  = apply(sim_mat, 1, quantile, probs = probs[4])
  )
  out
}

#----------------------------
# Core forecaster for one type
#----------------------------

forecast_one_type <- function(dat,
                              H = 13,
                              K = 6,
                              L = 2,
                              nsim = 2000,
                              seed = 1,
                              add_poisson_noise = TRUE) {

  set.seed(seed)

  apps <- dat$applications
  issued <- dat$issued
  arrivals <- dat$arrivals
  n <- nrow(dat)

  # Learn issued->arrival delay weights on count scale
  w_fit <- estimate_delay_weights_nnls(issued, arrivals, K = K)
  w <- w_fit$w

  # If everything is zero, forecast zeros
  if (sum(apps) == 0 && sum(issued) == 0 && sum(arrivals) == 0) {
    future_weeks <- seq(max(dat$week) + 7, by = "week", length.out = H)
    sim <- matrix(0, nrow = H, ncol = nsim)
    return(list(
      week = future_weeks,
      sim_arrivals = sim,
      weights = w
    ))
  }

  # Model applications on log scale
  apps_log <- log1p(apps)
  fit_apps <- safe_auto_arima(apps_log)

  # Model issued with ARIMAX on log scale using lagged applications
  issued_log <- log1p(issued)
  xreg_hist_full <- make_xreg_lags(apps_log, L)
  keep <- complete.cases(xreg_hist_full)

  # If not enough rows after lagging, reduce L
  if (sum(keep) < 8) {
    L2 <- max(0, min(L, 1))
    xreg_hist_full <- make_xreg_lags(apps_log, L2)
    keep <- complete.cases(xreg_hist_full)
    L <- L2
  }

  xreg_hist <- xreg_hist_full[keep, , drop = FALSE]
  fit_issued <- safe_auto_arima(issued_log[keep], xreg = xreg_hist)

  # Simulation store
  sim_arrivals <- matrix(0, nrow = H, ncol = nsim)

  # Pre-store history issued for convolution
  issued_hist <- issued

  # Simulate nsim future paths
  for (s in 1:nsim) {
    # simulate applications forward on log scale
    apps_sim_log <- as.numeric(simulate(fit_apps, nsim = H, bootstrap = TRUE))

    # build xreg for issued forecast using simulated apps
    xreg_f <- make_future_xreg(apps_log, apps_sim_log, L = L)

    # simulate issued forward on log scale (conditional on xreg)
    issued_sim_log <- as.numeric(simulate(fit_issued, nsim = H, xreg = xreg_f, bootstrap = TRUE))
    issued_sim <- pmax(expm1(issued_sim_log), 0)

    # convert issued -> arrivals via learned lag weights (convolution)
    issued_full <- c(issued_hist, issued_sim)
    conv_mean <- numeric(H)
    for (h in 1:H) {
      t <- length(issued_hist) + h
      conv_mean[h] <- sum(w * issued_full[t - (0:K)])
    }
    conv_mean <- pmax(conv_mean, 0)

    if (add_poisson_noise) {
      sim_arrivals[, s] <- rpois(H, lambda = conv_mean)
    } else {
      sim_arrivals[, s] <- conv_mean
    }
  }

  future_weeks <- seq(max(dat$week) + 7, by = "week", length.out = H)

  list(
    week = future_weeks,
    sim_arrivals = sim_arrivals,
    weights = w
  )
}

#----------------------------
# Wrapper: forecast by type + total
#----------------------------

forecast_ukraine_arrivals <- function(df,
                                      horizon_weeks = 13,  # ~ next 3 months
                                      max_delay = 6,
                                      app_lags = 2,
                                      nsim = 2000,
                                      seed = 1) {

  df0 <- prepare_weekly(df)

  types <- sort(unique(df0$type))

  by_type_sims <- list()
  by_type_fc <- list()

  for (i in seq_along(types)) {
    ty <- types[i]
    dat_ty <- df0 %>% filter(type == ty) %>% arrange(week)

    res <- forecast_one_type(
      dat_ty,
      H = horizon_weeks,
      K = max_delay,
      L = app_lags,
      nsim = nsim,
      seed = seed + i
    )

    summ <- summarise_sims(res$sim_arrivals) %>%
      mutate(week = res$week, type = ty) %>%
      select(type, week, everything())

    by_type_sims[[ty]] <- res$sim_arrivals
    by_type_fc[[ty]] <- summ
  }

  by_type_fc_df <- bind_rows(by_type_fc)

  # Total: sum simulated arrivals across types (keeps coherent intervals)
  # Assumes all types produce the same future week index
  weeks <- by_type_fc_df %>% filter(type == types[1]) %>% pull(week)

  total_sim <- Reduce(`+`, by_type_sims)  # H x nsim
  total_fc <- summarise_sims(total_sim) %>%
    mutate(week = weeks, type = "TOTAL") %>%
    select(type, week, everything())

  list(
    by_type = by_type_fc_df,
    total = total_fc
  )
}

#----------------------------
# Example usage:
#----------------------------
# result <- forecast_ukraine_arrivals(df, horizon_weeks = 13, max_delay = 6, app_lags = 2, nsim = 3000)
# result$total
# result$by_type
```

### What you get
- `result$by_type`: weekly arrivals forecast for each visa type with mean and quantiles (`q05/q20/q80/q95`)
- `result$total`: total arrivals forecast with coherent uncertainty (summing simulations, not summing quantiles)

---

### Notes / practical tuning (important with only 22 weeks)
- `max_delay` (K): try 4–8; with 22 points you usually shouldn’t go much higher than ~8.
- `app_lags` (L): 1–2 is usually enough.
- If there are known policy shocks (rule changes), this data-only method cannot “know” them—expect forecast error around structural breaks.

If you share a sample of your 22-week table (even anonymized), I can adapt the code to your exact column names and add a small rolling-origin validation loop to choose `max_delay` and `app_lags` automatically.