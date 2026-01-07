Below is a full **Monte‑Carlo, cohort (queue) simulation** in **R** that uses only your weekly totals by visa type:

* **Applications → Visas issued → Arrivals**
* Learns (from history) **delay distributions** and **attrition** at each stage
* Forecasts (simulates) **future applications** per visa type
* Runs a **numerical simulation** of cohorts moving through the pipeline week by week
* Outputs **predictive distributions** (median + intervals) for weekly arrivals and the 13‑week total

This approach “feature engineers” the most important hidden state you don’t observe directly:

* **Backlogs** (applications not yet issued; visas not yet arrived)
* **Stage conversion probabilities**
* **Delay / processing-time distributions**
* **Week-to-week operational noise** (processing capacity variability)

---

```r
###############################################################################
# Ukraine arrivals pipeline simulation (Applications -> Issued -> Arrivals)
# Cohort-based Monte Carlo, estimated from weekly aggregates by visa type.
#
# Expected input data frame columns (weekly totals):
#   week            : Date (or coercible to Date), week ending/starting
#   visa_type       : character/factor (e.g. "Ukraine Family Scheme", ...)
#   applications    : integer >= 0
#   visas_issued    : integer >= 0
#   arrivals        : integer >= 0
#
# Output:
#   - weekly predictive summaries (per visa_type and total)
#   - 13-week total arrivals predictive summary
###############################################################################

# ---- Packages ----
pkgs <- c("dplyr", "tidyr", "lubridate", "forecast", "nnls", "purrr")
to_install <- pkgs[!vapply(pkgs, requireNamespace, quietly = TRUE, FUN.VALUE = logical(1))]
if (length(to_install) > 0) install.packages(to_install)

library(dplyr)
library(tidyr)
library(lubridate)
library(forecast)
library(nnls)
library(purrr)

# ---- Utilities ----

round_preserve_sum <- function(x, target_sum) {
  # Rounds nonnegative numeric vector x to nonnegative integers summing to target_sum.
  if (target_sum <= 0 || length(x) == 0) return(rep.int(0L, length(x)))
  x <- pmax(x, 0)
  s <- sum(x)
  if (s <= 0) return(rep.int(0L, length(x)))

  y <- x * (target_sum / s)
  y_floor <- floor(y)
  remainder <- as.integer(target_sum - sum(y_floor))
  if (remainder > 0) {
    frac <- y - y_floor
    idx <- order(frac, decreasing = TRUE)[seq_len(min(remainder, length(frac)))]
    y_floor[idx] <- y_floor[idx] + 1
  }
  as.integer(pmax(y_floor, 0))
}

build_lag_matrix <- function(x, L) {
  # X[t, l] = x[t - l] for l=0..L (aligned so all indices valid)
  n <- length(x)
  if (n <= L) stop("Not enough history to build lag matrix.")
  m <- n - L
  X <- matrix(0, nrow = m, ncol = L + 1)
  for (l in 0:L) {
    X[, l + 1] <- x[(L + 1 - l):(n - l)]
  }
  X
}

smooth_weights_ma <- function(w, k = 3) {
  # Simple moving-average smoothing while preserving nonnegativity and sum.
  if (length(w) < k) return(w)
  filt <- rep(1 / k, k)
  w2 <- as.numeric(stats::filter(w, filt, sides = 2, method = "convolution"))
  w2[is.na(w2)] <- w[is.na(w2)]
  w2 <- pmax(w2, 0)
  if (sum(w2) > 0) w2 <- w2 * (sum(w) / sum(w2))
  w2
}

estimate_delay_weights <- function(y_downstream,
                                  x_upstream,
                                  L = 26,
                                  smooth = TRUE,
                                  smooth_k = 3,
                                  cap_sum = 0.999) {
  # Estimate discrete delay weights w[0..L] such that:
  #   y_t ≈ sum_{l=0..L} w_l * x_{t-l}
  # with w_l >= 0. Sum(w) approximates overall conversion probability.
  #
  # Returns:
  #   w         : numeric length L+1
  #   resid_log : vector of log-multipliers from history for week-to-week noise
  #   yhat      : fitted values (aligned with y_sub)
  #   y_sub     : aligned observed downstream
  #   L_used    : actual L used

  x <- as.numeric(x_upstream)
  y <- as.numeric(y_downstream)
  n <- length(x)

  # shrink L if needed
  L_used <- min(L, max(0, n - 2))
  if (L_used == 0) {
    # crude: all effect at lag 0
    alpha <- if (sum(x) > 0) min(sum(y) / sum(x), cap_sum) else 0
    w <- c(alpha)
    y_sub <- y
    yhat <- alpha * x
    resid_log <- log((y_sub + 1) / (yhat + 1))
    resid_log <- resid_log[is.finite(resid_log)]
    return(list(w = w, resid_log = resid_log, yhat = yhat, y_sub = y_sub, L_used = 0))
  }

  # Align series
  X <- build_lag_matrix(x, L_used)
  y_sub <- y[(L_used + 1):n]

  # NNLS: w >= 0
  fit <- nnls::nnls(X, y_sub)
  w <- as.numeric(coef(fit))
  w <- pmax(w, 0)

  if (smooth) w <- smooth_weights_ma(w, k = smooth_k)

  # Cap total probability mass to <= cap_sum
  sw <- sum(w)
  if (sw > cap_sum && sw > 0) w <- w * (cap_sum / sw)

  yhat <- as.numeric(X %*% w)
  resid_log <- log((y_sub + 1) / (yhat + 1))
  resid_log <- resid_log[is.finite(resid_log)]

  list(w = w, resid_log = resid_log, yhat = yhat, y_sub = y_sub, L_used = L_used)
}

weights_to_hazard <- function(w) {
  # Convert delay weights w[0..L] into hazards h[0..L]:
  # hazard at age a = P(event at age a | not yet happened before a)
  w <- as.numeric(w)
  L <- length(w) - 1
  h <- rep(0, length(w))
  cum_before <- 0
  for (a in 0:L) {
    remain <- 1 - cum_before
    if (remain <= 1e-12) {
      h[a + 1] <- 0
    } else {
      h[a + 1] <- min(1, max(0, w[a + 1] / remain))
    }
    cum_before <- cum_before + w[a + 1]
  }
  h
}

init_cohorts_from_history <- function(x_history, w, backlog_total) {
  # Build initial cohort state for "pending" items at the simulation start.
  # We approximate age distribution using survival implied by w and recent upstream volumes,
  # then rescale to match observed backlog_total = (cum upstream - cum downstream).
  #
  # Cohort vector is length (L+1) for ages 0..L at the *start* of a week, before processing.
  # At simulation start (week after last observed), age0 is 0; age1..L get mass.

  x <- as.numeric(x_history)
  n <- length(x)
  L <- length(w) - 1

  cohorts_raw <- numeric(L + 1)
  cohorts_raw[1] <- 0  # age 0 at sim start

  # survival to enter age a (a >= 1) = 1 - sum_{j=0..a-1} w_j
  # In R indexing w[1]=w0, so sum_{j=0..a-1} w_j = cumsum(w)[a]
  cw <- cumsum(w)

  for (a in 1:L) {
    idx <- n - (a - 1) # age1 uses last observation, age2 uses second last, etc.
    if (idx >= 1) {
      survival_a <- max(0, 1 - cw[a])  # 1 - sum_{j=0..a-1} w_j
      cohorts_raw[a + 1] <- x[idx] * survival_a
    } else {
      cohorts_raw[a + 1] <- 0
    }
  }

  backlog_total <- as.integer(max(0, round(backlog_total)))
  cohorts_int <- round_preserve_sum(cohorts_raw, backlog_total)
  cohorts_int
}

simulate_future_applications <- function(app_series, h, nsim, seasonal_freq = 52) {
  # Simulate future applications paths using ARIMA on log1p scale.
  # Returns matrix [h x nsim], integer counts >= 0.
  y <- as.numeric(app_series)

  # Fallback for tiny history
  if (length(y) < 8 || all(y == y[1])) {
    mu <- if (length(y) > 0) mean(tail(y, min(8, length(y)))) else 0
    sims <- matrix(rpois(h * nsim, lambda = max(mu, 0)), nrow = h, ncol = nsim)
    return(sims)
  }

  y_ts <- ts(log1p(y), frequency = seasonal_freq)

  fit <- tryCatch(
    forecast::auto.arima(y_ts, seasonal = TRUE, stepwise = TRUE, approximation = FALSE),
    error = function(e) NULL
  )

  if (is.null(fit)) {
    # fallback: bootstrap recent weeks
    recent <- tail(y, min(12, length(y)))
    sims <- replicate(nsim, sample(recent, size = h, replace = TRUE))
    return(matrix(as.integer(pmax(round(sims), 0)), nrow = h, ncol = nsim))
  }

  sims <- replicate(nsim, {
    sim_log <- as.numeric(forecast::simulate(fit, nsim = h, future = TRUE, bootstrap = TRUE))
    as.integer(pmax(round(expm1(sim_log)), 0))
  })

  matrix(sims, nrow = h, ncol = nsim)
}

# ---- Core simulator for one visa type ----

simulate_pipeline_one_type <- function(weeks,
                                       apps,
                                       issued,
                                       arrivals,
                                       horizon_weeks = 13,
                                       nsim = 2000,
                                       L_issue = 26,
                                       L_arrive = 26,
                                       seed = NULL) {
  if (!is.null(seed)) set.seed(seed)

  apps <- as.integer(pmax(apps, 0))
  issued <- as.integer(pmax(issued, 0))
  arrivals <- as.integer(pmax(arrivals, 0))

  # Feature engineering: observed backlogs at end of history
  backlog_apps_end <- max(0, sum(apps) - sum(issued))
  backlog_visas_end <- max(0, sum(issued) - sum(arrivals))

  # Estimate delay weights + weekly multiplicative noise from residuals
  issue_model <- estimate_delay_weights(y_downstream = issued, x_upstream = apps, L = L_issue, smooth = TRUE)
  arrive_model <- estimate_delay_weights(y_downstream = arrivals, x_upstream = issued, L = L_arrive, smooth = TRUE)

  w_issue <- issue_model$w
  w_arrive <- arrive_model$w

  h_issue <- weights_to_hazard(w_issue)
  h_arrive <- weights_to_hazard(w_arrive)

  resid_issue <- issue_model$resid_log
  resid_arrive <- arrive_model$resid_log

  # Center residual multipliers so E[mult] ~ 1
  resid_issue <- resid_issue - mean(resid_issue, na.rm = TRUE)
  resid_arrive <- resid_arrive - mean(resid_arrive, na.rm = TRUE)

  # Init cohort states from history + observed backlog totals
  app_coh0 <- init_cohorts_from_history(apps, w_issue, backlog_apps_end)
  visa_coh0 <- init_cohorts_from_history(issued, w_arrive, backlog_visas_end)

  # Simulate future applications
  app_sims <- simulate_future_applications(apps, h = horizon_weeks, nsim = nsim)

  # Storage
  arrivals_sims <- matrix(0L, nrow = horizon_weeks, ncol = nsim)
  issued_sims <- matrix(0L, nrow = horizon_weeks, ncol = nsim)
  apps_used <- app_sims

  n_issue_age <- length(h_issue)
  n_arr_age  <- length(h_arrive)

  for (s in seq_len(nsim)) {
    app_coh <- app_coh0
    visa_coh <- visa_coh0

    for (t in seq_len(horizon_weeks)) {
      # Add new applications to age 0
      app_coh[1] <- app_coh[1] + as.integer(app_sims[t, s])

      # Week-to-week processing multipliers (captures capacity swings)
      mult_issue <- if (length(resid_issue) > 5) exp(sample(resid_issue, 1, replace = TRUE)) else 1
      mult_arrive <- if (length(resid_arrive) > 5) exp(sample(resid_arrive, 1, replace = TRUE)) else 1

      # Issue step (vectorized binomial across ages)
      p_issue <- pmin(1, pmax(0, h_issue * mult_issue))
      issued_by_age <- rbinom(n_issue_age, size = app_coh, prob = p_issue)
      app_coh <- app_coh - issued_by_age
      total_issued <- as.integer(sum(issued_by_age))

      # Add issued visas to visa cohorts age 0
      visa_coh[1] <- visa_coh[1] + total_issued

      # Arrival step (vectorized binomial across ages)
      p_arr <- pmin(1, pmax(0, h_arrive * mult_arrive))
      arrive_by_age <- rbinom(n_arr_age, size = visa_coh, prob = p_arr)
      visa_coh <- visa_coh - arrive_by_age
      total_arrivals <- as.integer(sum(arrive_by_age))

      issued_sims[t, s] <- total_issued
      arrivals_sims[t, s] <- total_arrivals

      # Age cohorts (drop anything older than L)
      app_coh <- c(0L, app_coh[1:(n_issue_age - 1)])
      visa_coh <- c(0L, visa_coh[1:(n_arr_age - 1)])
    }
  }

  list(
    apps_sims = apps_used,
    issued_sims = issued_sims,
    arrivals_sims = arrivals_sims,
    models = list(
      w_issue = w_issue, h_issue = h_issue,
      w_arrive = w_arrive, h_arrive = h_arrive,
      backlog_apps_end = backlog_apps_end,
      backlog_visas_end = backlog_visas_end
    )
  )
}

# ---- Main wrapper (all visa types + totals) ----

simulate_ukraine_arrivals <- function(df,
                                     horizon_weeks = 13,
                                     nsim = 2000,
                                     L_issue = 26,
                                     L_arrive = 26,
                                     seed = 123) {

  required <- c("week", "visa_type", "applications", "visas_issued", "arrivals")
  missing <- setdiff(required, names(df))
  if (length(missing) > 0) stop("Missing required columns: ", paste(missing, collapse = ", "))

  df <- df %>%
    mutate(
      week = as.Date(week),
      visa_type = as.character(visa_type),
      applications = as.integer(replace_na(applications, 0)),
      visas_issued = as.integer(replace_na(visas_issued, 0)),
      arrivals = as.integer(replace_na(arrivals, 0))
    ) %>%
    arrange(visa_type, week)

  # Make complete weekly sequences per type
  all_weeks <- seq(min(df$week, na.rm = TRUE), max(df$week, na.rm = TRUE), by = "week")

  dfc <- df %>%
    group_by(visa_type) %>%
    tidyr::complete(
      week = all_weeks,
      fill = list(applications = 0L, visas_issued = 0L, arrivals = 0L)
    ) %>%
    arrange(visa_type, week) %>%
    ungroup()

  last_week <- max(dfc$week, na.rm = TRUE)
  future_weeks <- seq(last_week + weeks(1), by = "week", length.out = horizon_weeks)

  types <- sort(unique(dfc$visa_type))

  # Run per-type simulations
  per_type <- map2(types, seq_along(types), function(vt, k) {
    d <- dfc %>% filter(visa_type == vt) %>% arrange(week)

    sim <- simulate_pipeline_one_type(
      weeks = d$week,
      apps = d$applications,
      issued = d$visas_issued,
      arrivals = d$arrivals,
      horizon_weeks = horizon_weeks,
      nsim = nsim,
      L_issue = L_issue,
      L_arrive = L_arrive,
      seed = seed + k
    )

    list(visa_type = vt, sim = sim)
  })

  # Build long paths: (sim, week, visa_type, arrivals)
  paths <- map_dfr(per_type, function(obj) {
    vt <- obj$visa_type
    arr <- obj$sim$arrivals_sims  # [h x nsim]

    tibble(
      week = rep(future_weeks, times = nsim),
      sim = rep(seq_len(nsim), each = horizon_weeks),
      visa_type = vt,
      arrivals = as.integer(as.vector(arr))
    )
  })

  # Summaries per type per week
  summarise_quantiles <- function(x) {
    qs <- quantile(x, probs = c(0.05, 0.25, 0.50, 0.75, 0.95), na.rm = TRUE, names = FALSE)
    tibble(p05 = qs[1], p25 = qs[2], p50 = qs[3], p75 = qs[4], p95 = qs[5])
  }

  weekly_by_type <- paths %>%
    group_by(visa_type, week) %>%
    summarise_quantiles(arrivals) %>%
    ungroup()

  # Weekly totals across types (sum within sim-week first)
  weekly_total_paths <- paths %>%
    group_by(sim, week) %>%
    summarise(arrivals = sum(arrivals), .groups = "drop")

  weekly_total <- weekly_total_paths %>%
    group_by(week) %>%
    summarise_quantiles(arrivals) %>%
    ungroup()

  # 13-week total across types
  total_13w_paths <- weekly_total_paths %>%
    group_by(sim) %>%
    summarise(total_arrivals_13w = sum(arrivals), .groups = "drop")

  total_13w_summary <- summarise_quantiles(total_13w_paths$total_arrivals_13w) %>%
    mutate(horizon_weeks = horizon_weeks)

  list(
    future_weeks = future_weeks,
    weekly_by_type = weekly_by_type,
    weekly_total = weekly_total,
    total_13w_summary = total_13w_summary,
    paths = paths,                      # full simulated paths (can be large)
    weekly_total_paths = weekly_total_paths
  )
}

###############################################################################
# Example usage (you provide df):
#
# df <- read.csv("your_data.csv")
# df$week <- as.Date(df$week)
#
# result <- simulate_ukraine_arrivals(
#   df,
#   horizon_weeks = 13,   # ~3 months
#   nsim = 5000,          # increase for smoother intervals
#   L_issue = 26,         # max issue delay (weeks)
#   L_arrive = 26,        # max arrival delay (weeks)
#   seed = 123
# )
#
# result$total_13w_summary
# result$weekly_total
# result$weekly_by_type
###############################################################################
```

---

### How to interpret the outputs

* `result$weekly_total`: for each of the next 13 weeks, predicted arrivals **total across all visa types** with:

  * `p50` = median forecast
  * `p05`/`p95` = 90% interval (5th–95th percentile)
  * `p25`/`p75` = middle 50% interval

* `result$total_13w_summary`: forecast distribution for the **sum of arrivals over the next 13 weeks**.

* `result$weekly_by_type`: same as weekly_total, but split by visa type.

---

### Why this simulation is “numerical” (not just a regression)

It explicitly simulates the pipeline as a **stochastic dynamical system** (a cohort queue) where:

* new applications enter each week,
* some portion are issued after a learned processing delay,
* some issued visas convert into arrivals after another learned delay,
* and week-to-week volatility is injected in a controlled way.

It’s the closest you can get to an “agent-like” simulation while only having weekly totals.

---

### Practical knobs to tune (the ones that actually matter)

* `L_issue`, `L_arrive`: if your historical processing delays sometimes exceed 26 weeks, increase to 40–52.
* `nsim`: 2000 is fine; 5000–20000 gives smoother tails for intervals.
* Data sanity: if `sum(issued) > sum(apps)` for a type (can happen due to definitional changes), you’ll want to reconcile or clip during preprocessing.

---

If you paste in a small sample of your table structure (just column names + a few rows, not sensitive values), I can adapt the preprocessing to your exact week definition (week starting vs ending) and make the outputs match your reporting calendar precisely (e.g., 13 weeks aligned to Mondays, ISO weeks, etc.).
