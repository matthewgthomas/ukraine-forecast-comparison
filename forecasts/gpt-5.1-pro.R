############################################################
# Forecast arrivals of people with visas using a
# 2-stage lagged pipeline model:
#   applications -> visas issued -> arrivals
############################################################

# ---- Parameters ----------------------------------------------------------

input_file <- "ukraine_weekly_flows.csv" # path to your data
forecast_horizon <- 8 # number of weeks ahead to predict

max_lag_AV <- 8 # max lag (weeks) from applications -> visas
max_lag_VR <- 12 # max lag (weeks) from visas -> arrivals

# How to project future applications:
# "last"  = hold at last observed value
# "mean4" = use mean of last 4 weeks
future_app_method <- "mean4"

# ---- Packages ------------------------------------------------------------

# Uncomment the next line once to install dependencies:
# install.packages(c("nnls", "dplyr", "readr", "lubridate", "tibble"))

library(nnls)
library(dplyr)
library(readr)
library(lubridate)
library(tibble)

# ---- Helper functions ----------------------------------------------------

build_lag_matrix <- function(x, K) {
  # Build matrix with columns x_t, x_{t-1}, ..., x_{t-K}
  n <- length(x)
  if (n <= K) {
    stop("Series length must be greater than max lag K.")
  }
  idx <- (K + 1):n
  X <- sapply(0:K, function(k) x[idx - k])
  colnames(X) <- paste0("lag_", 0:K)
  list(X = as.matrix(X), idx = idx)
}

project_future_applications <- function(A, h, method = c("last", "mean4")) {
  method <- match.arg(method)
  if (method == "last") {
    rep(tail(A, 1), h)
  } else if (method == "mean4") {
    k <- min(4, length(A))
    rep(mean(tail(A, k)), h)
  }
}

# ---- Load and prepare data ----------------------------------------------

df <- read_csv(input_file, show_col_types = FALSE)

# Basic expectations:
# - week_start: date (or something coercible to Date)
# - applications, visas_issued, arrivals: numeric
df <- df %>%
  mutate(week_start = as.Date(week_start)) %>%
  arrange(week_start)

if (any(is.na(df$week_start))) {
  stop("week_start column must be coercible to Date.")
}

A <- df$applications
V <- df$visas_issued
R <- df$arrivals

n <- nrow(df)

if (n <= max(max_lag_AV, max_lag_VR) + 1) {
  stop(
    "Not enough weeks of data for chosen max lags. Reduce max_lag_AV / max_lag_VR or use more data."
  )
}

# ---- Stage 1: Applications -> Visas (estimate lag distribution) ----------

lag_app <- build_lag_matrix(A, max_lag_AV)
X_AV <- lag_app$X
y_V <- V[lag_app$idx]

fit_AV <- nnls(X_AV, y_V)
phi <- coef(fit_AV) # length = max_lag_AV + 1
names(phi) <- paste0("lag_", 0:max_lag_AV)

conv_AV <- sum(phi) # overall "applications -> visas" intensity

cat("\n--- Applications -> Visas model ---\n")
cat("Max lag (weeks):", max_lag_AV, "\n")
cat("Non-negative least squares fit (phi):\n")
print(round(phi, 4))
cat(
  "Sum(phi) ~ overall conversion per application over all lags:",
  round(conv_AV, 3),
  "\n\n"
)

# ---- Stage 2: Visas -> Arrivals (estimate lag distribution) --------------

lag_vis <- build_lag_matrix(V, max_lag_VR)
X_VR <- lag_vis$X
y_R <- R[lag_vis$idx]

fit_VR <- nnls(X_VR, y_R)
theta <- coef(fit_VR) # length = max_lag_VR + 1
names(theta) <- paste0("lag_", 0:max_lag_VR)

conv_VR <- sum(theta) # overall "visas -> arrivals" intensity

# Normalised distribution over delays (for interpretation)
delay_weeks <- 0:max_lag_VR
theta_norm <- if (conv_VR > 0) theta / conv_VR else theta
mean_delay <- sum(delay_weeks * theta_norm)

cat("--- Visas -> Arrivals model ---\n")
cat("Max lag (weeks):", max_lag_VR, "\n")
cat("Non-negative least squares fit (theta):\n")
print(round(theta, 4))
cat(
  "Sum(theta) ~ overall arrivals per visa over all lags:",
  round(conv_VR, 3),
  "\n"
)
cat("Normalised lag distribution (theta / sum(theta)):\n")
print(round(theta_norm, 3))
cat(
  "Approximate mean delay (weeks) from visa to arrival:",
  round(mean_delay, 2),
  "\n\n"
)

# ---- Forecast future applications ---------------------------------------

h <- forecast_horizon

future_A <- project_future_applications(A, h, method = future_app_method)

A_ext <- c(A, future_A) # extended applications vector

# ---- Forecast visas using Applications -> Visas lag model ----------------

J <- length(phi) - 1 # max lag AV
nA <- length(A_ext)

# Start with actual visas for historical period, then fill predicted
V_full <- c(V, rep(NA_real_, h))

for (t in (n + 1):(n + h)) {
  # V_t = sum_{j=0}^J phi_j * A_{t-j}
  V_full[t] <- sum(phi * A_ext[t - (0:J)])
}

# ---- Forecast arrivals using Visas -> Arrivals lag model -----------------

K <- length(theta) - 1 # max lag VR

R_full <- c(R, rep(NA_real_, h))

for (t in (n + 1):(n + h)) {
  # R_t = sum_{k=0}^K theta_k * V_{t-k}
  R_full[t] <- sum(theta * V_full[t - (0:K)])
}

# ---- Build forecast data frame ------------------------------------------

last_week <- max(df$week_start)
future_weeks <- last_week + 7 * (1:h)

forecast_df <- tibble(
  week_start = future_weeks,
  applications = future_A,
  visas_pred = V_full[(n + 1):(n + h)],
  arrivals_pred = R_full[(n + 1):(n + h)]
)

cat("--- Forecast arrivals ---\n")
print(forecast_df)

# Optionally write to CSV
# write_csv(forecast_df, "ukraine_arrivals_forecast.csv")

############################################################
# End of script
############################################################
