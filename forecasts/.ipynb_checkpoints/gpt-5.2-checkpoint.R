install.packages(c("dplyr","tidyr","lubridate","forecast","slider","ggplot2"))

library(dplyr)
library(tidyr)
library(lubridate)
library(forecast)
library(slider)
library(ggplot2)

#-----------------------------
# 0) Input data assumptions
#-----------------------------
# df has columns:
# week (Date), visa_type (chr/fct), applications (int), issued (int), arrivals (int)
# One row per week x visa_type (3 rows per week).
#
# Example:
# df <- read.csv("weekly_ukraine.csv") %>% mutate(week = as.Date(week))

#-----------------------------
# 1) Pivot to wide by type + totals
#-----------------------------
make_wide <- function(df){
  df %>%
    mutate(visa_type = as.character(visa_type)) %>%
    arrange(week, visa_type) %>%
    pivot_wider(
      id_cols = week,
      names_from = visa_type,
      values_from = c(applications, issued, arrivals),
      values_fill = 0
    ) %>%
    # totals across types
    mutate(
      applications_total = rowSums(across(starts_with("applications_"))),
      issued_total        = rowSums(across(starts_with("issued_"))),
      arrivals_total      = rowSums(across(starts_with("arrivals_")))
    ) %>%
    arrange(week)
}

#-----------------------------
# 2) Feature engineering: lags + rolling sums + shares
#-----------------------------
add_features <- function(wide_df, max_lag = 8, roll_windows = c(4,8,12)){
  out <- wide_df

  # Helper to add lags for a vector
  add_lags <- function(x, prefix, k){
    lagged <- sapply(1:k, function(L) dplyr::lag(x, L))
    colnames(lagged) <- paste0(prefix, "_lag", 1:k)
    as.data.frame(lagged)
  }

  # Add lag features for totals (can also do per-type similarly if you want)
  out <- bind_cols(out,
                   add_lags(out$issued_total, "issued_total", max_lag),
                   add_lags(out$applications_total, "applications_total", max_lag),
                   add_lags(out$arrivals_total, "arrivals_total", 2))  # short AR lags

  # Rolling sums of issued_total
  for(w in roll_windows){
    out[[paste0("issued_total_roll", w)]] <-
      slide_dbl(out$issued_total, sum, .before = w-1, .complete = FALSE)
  }

  # Composition shares (issued by type)
  issued_cols <- grep("^issued_", names(out), value = TRUE)
  issued_cols <- setdiff(issued_cols, "issued_total")
  for(col in issued_cols){
    share_name <- sub("^issued_", "share_issued_", col)
    out[[share_name]] <- out[[col]] / pmax(out$issued_total, 1)
  }

  # Smoothed issuance rate (total) - optional but often useful
  out$issuance_rate <- out$issued_total / pmax(out$applications_total, 1)
  out$issuance_rate_roll4 <- slide_dbl(out$issuance_rate, mean, .before = 3, .complete = FALSE)

  out
}

#-----------------------------
# 3) Train/test split + model fitting with xreg selection
#-----------------------------
fit_arrivals_model <- function(feat_df, h = 13){
  # Drop early rows with NAs due to lags
  dfm <- feat_df %>% filter(!is.na(issued_total_lag8))

  y <- ts(dfm$arrivals_total, frequency = 52)

  # Candidate regressors: keep it parsimonious to reduce overfit
  xreg <- dfm %>%
    select(
      issued_total_lag1, issued_total_lag2, issued_total_lag3, issued_total_lag4,
      issued_total_roll4, issued_total_roll8,
      arrivals_total_lag1, arrivals_total_lag2,
      issuance_rate_roll4
    ) %>% as.matrix()

  # ARIMAX with automatic order selection
  fit <- auto.arima(y, xreg = xreg, seasonal = FALSE, stepwise = TRUE, approximation = FALSE)
  list(fit = fit, model_df = dfm)
}

#-----------------------------
# 4) Forecast future issued totals (and optionally by type)
#-----------------------------
forecast_issued_total <- function(wide_df, h = 13){
  issued_ts <- ts(wide_df$issued_total, frequency = 52)
  fit_issued <- auto.arima(issued_ts, seasonal = FALSE)
  fc <- forecast(fit_issued, h = h)
  list(fit = fit_issued, forecast = fc)
}

#-----------------------------
# 5) Build future regressors from forecasted issued_total
#-----------------------------
build_future_xreg <- function(feat_df, wide_df, issued_fc, h = 13){
  # We need a continuation of issued_total to compute lags and rolling sums.
  last_feat <- feat_df %>% arrange(week)
  last_wide <- wide_df %>% arrange(week)

  issued_hist <- last_wide$issued_total
  issued_future <- as.numeric(issued_fc$forecast$mean)
  issued_all <- c(issued_hist, issued_future)

  # Also need arrivals lags (use last observed arrivals, then iterate with NA for future;
  # simplest: set arrivals_lag1/2 from last observed only for the first steps)
  arrivals_hist <- last_wide$arrivals_total

  # Prepare a small dataframe for future weeks
  future_weeks <- seq(max(last_wide$week) + weeks(1), by = "1 week", length.out = h)

  # Compute future xreg rows one-step-ahead style from the concatenated series
  n <- length(issued_hist)
  get_lag <- function(vec, idx, L) vec[idx - L]

  x_future <- matrix(NA_real_, nrow = h, ncol = 9)
  colnames(x_future) <- c(
    "issued_total_lag1","issued_total_lag2","issued_total_lag3","issued_total_lag4",
    "issued_total_roll4","issued_total_roll8",
    "arrivals_total_lag1","arrivals_total_lag2",
    "issuance_rate_roll4"
  )

  # issuance_rate features require applications_total; if you don't forecast applications,
  # carry forward last rolling mean as a constant.
  issuance_rate_roll4_last <- tail(feat_df$issuance_rate_roll4[!is.na(feat_df$issuance_rate_roll4)], 1)

  for(i in 1:h){
    idx <- n + i
    # issued lags
    x_future[i,"issued_total_lag1"] <- get_lag(issued_all, idx, 1)
    x_future[i,"issued_total_lag2"] <- get_lag(issued_all, idx, 2)
    x_future[i,"issued_total_lag3"] <- get_lag(issued_all, idx, 3)
    x_future[i,"issued_total_lag4"] <- get_lag(issued_all, idx, 4)

    # rolling sums
    x_future[i,"issued_total_roll4"] <- sum(issued_all[(idx-3):idx])
    x_future[i,"issued_total_roll8"] <- sum(issued_all[(idx-7):idx])

    # arrivals lags: use last observed for first steps; after that, use recursive forecasts is better.
    # Minimal approach: keep them fixed using last observed (often acceptable with ARIMA errors).
    x_future[i,"arrivals_total_lag1"] <- tail(arrivals_hist, 1)
    x_future[i,"arrivals_total_lag2"] <- tail(arrivals_hist, 2)[1]

    # carry forward
    x_future[i,"issuance_rate_roll4"] <- issuance_rate_roll4_last
  }

  list(x_future = x_future, future_weeks = future_weeks)
}

#-----------------------------
# 6) End-to-end function
#-----------------------------
forecast_arrivals_3mo <- function(df, h = 13){
  wide <- make_wide(df)
  feat <- add_features(wide, max_lag = 8, roll_windows = c(4,8,12))

  arrivals_fit <- fit_arrivals_model(feat, h = h)

  issued_fc <- forecast_issued_total(wide, h = h)
  future_x <- build_future_xreg(feat, wide, issued_fc, h = h)

  fc_arrivals <- forecast(arrivals_fit$fit, xreg = future_x$x_future, h = h)

  out <- tibble(
    week = future_x$future_weeks,
    arrivals_mean = as.numeric(fc_arrivals$mean),
    arrivals_lo80 = as.numeric(fc_arrivals$lower[,"80%"]),
    arrivals_hi80 = as.numeric(fc_arrivals$upper[,"80%"]),
    arrivals_lo95 = as.numeric(fc_arrivals$lower[,"95%"]),
    arrivals_hi95 = as.numeric(fc_arrivals$upper[,"95%"])
  )

  list(
    arrivals_model = arrivals_fit$fit,
    issued_model = issued_fc$fit,
    forecast_table = out,
    forecast_object = fc_arrivals
  )
}

#-----------------------------
# 7) Run
#-----------------------------
# res <- forecast_arrivals_3mo(df, h = 13)
# res$forecast_table

# Optional plot
# autoplot(res$forecast_object) + ggtitle("Forecast weekly arrivals (next 13 weeks)")