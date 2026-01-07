library(tidyverse)

# ---- Load actuall arrivals data ----
actual_arrivals <- read_csv("data/visas-weekly-test.csv")

actual_arrivals <- actual_arrivals |> 
  group_by(date) |> 
  summarise(actual_arrivals = sum(arrivals)) |> 
  ungroup()

# ---- Load predictions from my simulation ----
brc_simulation <- read_csv("data/brc-simulation.csv")

brc_simulation <- brc_simulation |> 
  select(date = Date, forecast_arrivals = `Weekly arrivals`) |> 
  mutate(model = "British Red Cross simulation")

# ---- Load predictions from AI-generated simulations ----
claude_sim <- read_rds("simulations/data/claude-sonnet-4.5.rds")

claude_sim <- claude_sim |> 
  select(date, forecast_arrivals = predicted_arrivals) |> 
  mutate(model = "Claude Sonnet 4.5 (simulation)")

claude_forecast <- read_csv("forecasts2/data/claude-sonnet-4.5-forecast.csv")

claude_forecast <- claude_forecast |> 
  select(date = forecast_date, forecast_arrivals = ensemble) |> 
  mutate(model = "Claude Sonnet 4.5 (forecast)")

gemini <- read_rds("simulations/data/gemini-pro-3-forecast.rds")

gemini <- gemini |> 
  group_by(date = Week) |> 
  summarise(forecast_arrivals = sum(Expected_Arrival_Flow)) |> 
  ungroup() |> 
  mutate(model = "Gemini Pro 3")

gpt_3_5_forecast <- read_rds("forecasts/data/forecast-gpt-3.5-turbo.rds")

# Convert the forecast object to a data frame
gpt_3_5 <- tibble(
  date = actual_arrivals$date[1:length(gpt_3_5_forecast$mean)],
  forecast_arrivals = as.numeric(gpt_3_5_forecast$mean),
  model = "GPT-3.5 Turbo"
)

gpt_4o_forecast <- read_rds("forecasts/data/forecast-gpt-4o.rds")

# Convert the forecast object to a data frame
gpt_4o <- tibble(
  date = actual_arrivals$date[1:length(gpt_4o_forecast$mean)],
  forecast_arrivals = as.numeric(gpt_4o_forecast$mean),
  model = "GPT-4o"
)

gpt_4_1_forecast <- read_rds("forecasts/data/forecast-gpt-4.1.rds")

# Convert the forecast object to a data frame
gpt_4_1 <- tibble(
  date = actual_arrivals$date[1:length(gpt_4_1_forecast$mean)],
  forecast_arrivals = as.numeric(gpt_4_1_forecast$mean),
  model = "GPT-4.1"
) |> 
  slice(1:12)

# ---- Compare total number of arrivals (rather than visa type-specific) ----
eval_weekly <- 
  bind_rows(brc_simulation, claude_sim, claude_forecast, gemini, gpt_3_5, gpt_4o, gpt_4_1) |> 
  left_join(actual_arrivals, by = "date")

# Aggregate to total arrivals over the entire three month period for each model
eval_total <- eval_weekly |> 
  group_by(model) |> 
  summarise(
    forecast_arrivals = sum(forecast_arrivals),
    actual_arrivals = sum(actual_arrivals)
  ) |> 
  ungroup()

# ---- Helper functions to compute error metrics ----
calculate_errors <- function(df) {
  df |>
  mutate(
    error = forecast_arrivals - actual_arrivals,
    abs_error = abs(error),
    sq_error = error^2,
    ape = ifelse(
      actual_arrivals == 0,
      NA_real_,
      abs_error / abs(actual_arrivals) * 100
    )
  )
}

calculate_metrics <- function(df) {
  df |>
  group_by(model) |>
  summarise(
    n_forecasts = n(),
    mae = mean(abs_error, na.rm = TRUE), # mean absolute error
    rmse = sqrt(mean(sq_error, na.rm = TRUE)), # root mean squared error
    bias = mean(error, na.rm = TRUE), # signed bias
    mape = mean(ape, na.rm = TRUE), # mean absolute percentage error
    .groups = "drop"
  ) |>
  arrange(mae)
}

# ---- Evaluate forecasts ----
# How well did each model do on weekly arrivals?
errors_weekly <- calculate_errors(eval_weekly)
metrics_weekly <- calculate_metrics(errors_weekly)

metrics_weekly

write_csv(metrics_weekly, "data/forecast-evaluation.csv")

# How well did each model do on total arrivals?
errors_total <- calculate_errors(eval_total)
metrics_total <- calculate_metrics(errors_total)

metrics_total

write_csv(metrics_total, "data/forecast-evaluation-total.csv")
