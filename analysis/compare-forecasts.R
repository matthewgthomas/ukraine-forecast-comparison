############################################################
# Evaluate forecast accuracy at multiple horizons
# (1 week, ~1 month, ~3 months, ~6 months ahead)
#
# Assumes:
#   - Actuals CSV: one row per week with columns:
#       week_start, arrivals
#   - Forecasts CSV: long format with columns:
#       model, origin_week, target_week, forecast
#
# Outputs:
#   - Summary accuracy metrics by model & horizon
#   - (Optionally) writes metrics to CSV
############################################################

# ---------------- Parameters ------------------------------

actuals_file <- "ukraine_weekly_flows.csv" # actual arrivals
forecast_file <- "ukraine_forecasts_long.csv" # stacked forecasts

# Horizons of interest (in weeks)
# 1w  = 1 week
# 1m  ~ 4 weeks
# 3m  ~ 13 weeks
# 6m  ~ 26 weeks
horizon_tbl <- data.frame(
  horizon_weeks = c(1, 4, 13, 26),
  horizon_label = c("1w", "1m", "3m", "6m"),
  stringsAsFactors = FALSE
)

# Optional: where to save metrics
output_metrics_file <- "forecast_accuracy_summary.csv"

# ---------------- Packages -------------------------------

# Uncomment once if needed:
# install.packages(c("dplyr", "readr", "lubridate", "tibble"))

library(dplyr)
library(readr)
library(lubridate)
library(tibble)

# ---------------- Load actuals ---------------------------

actuals <- read_csv(actuals_file, show_col_types = FALSE) %>%
  mutate(week_start = as.Date(week_start)) %>%
  arrange(week_start)

if (!"arrivals" %in% names(actuals)) {
  stop("Actuals file must contain an 'arrivals' column.")
}

actuals <- actuals %>%
  select(week_start, arrivals_actual = arrivals)

# ---------------- Load forecasts -------------------------

forecasts <- read_csv(forecast_file, show_col_types = FALSE) %>%
  mutate(
    origin_week = as.Date(origin_week),
    target_week = as.Date(target_week)
  )

# If no model column, create a default one
if (!"model" %in% names(forecasts)) {
  forecasts <- forecasts %>%
    mutate(model = "model_1")
}

if (!"forecast" %in% names(forecasts)) {
  stop("Forecast file must contain a 'forecast' column.")
}

# Basic sanity checks
if (any(is.na(forecasts$origin_week)) || any(is.na(forecasts$target_week))) {
  stop("origin_week and target_week must be valid dates in forecast file.")
}

# ---------------- Merge & compute horizons ----------------

eval_df <- forecasts %>%
  # Attach actual arrivals for the target week
  inner_join(actuals, by = c("target_week" = "week_start")) %>%
  mutate(
    # Horizon in weeks: difference between target and origin
    horizon_weeks = as.numeric(difftime(
      target_week,
      origin_week,
      units = "days"
    )) /
      7
  ) %>%
  # Only future forecasts (positive horizon)
  filter(horizon_weeks > 0)

# Round horizon_weeks to nearest integer week
eval_df <- eval_df %>%
  mutate(horizon_weeks = round(horizon_weeks))

# Keep only horizons we care about (1, 4, 13, 26 weeks)
eval_df <- eval_df %>%
  inner_join(horizon_tbl, by = "horizon_weeks")

if (nrow(eval_df) == 0) {
  stop("No forecasts match the requested horizons (1,4,13,26 weeks).")
}

# ---------------- Compute error metrics -------------------

metrics <- eval_df %>%
  mutate(
    error = forecast - arrivals_actual,
    abs_error = abs(error),
    sq_error = error^2,
    ape = ifelse(
      arrivals_actual == 0,
      NA_real_,
      abs_error / abs(arrivals_actual) * 100
    )
  ) %>%
  group_by(model, horizon_label, horizon_weeks) %>%
  summarise(
    n_forecasts = n(),
    mae = mean(abs_error, na.rm = TRUE), # mean absolute error
    rmse = sqrt(mean(sq_error, na.rm = TRUE)), # root mean squared error
    bias = mean(error, na.rm = TRUE), # signed bias
    mape = mean(ape, na.rm = TRUE), # mean absolute percentage error
    .groups = "drop"
  ) %>%
  arrange(model, horizon_weeks)

cat("\n--- Forecast accuracy by model and horizon ---\n")
print(metrics)

# ---------------- Optional: save to CSV -------------------

write_csv(metrics, output_metrics_file)
cat("\nSummary metrics written to:", output_metrics_file, "\n")

############################################################
# End of script
############################################################
