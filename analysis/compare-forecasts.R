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
claude <- read_rds("simulations/data/claude-sonnet-4.5.rds")

claude <- claude |> 
  select(date, forecast_arrivals = predicted_arrivals) |> 
  mutate(model = "Claude Sonnet 4.5")

gemini <- read_rds("simulations/data/gemini-pro-3-forecast.rds")

gemini <- gemini |> 
  group_by(date = Week) |> 
  summarise(forecast_arrivals = sum(Expected_Arrival_Flow)) |> 
  ungroup() |> 
  mutate(model = "Gemini Pro 3")

# ---- Compare total number of arrivals (rather than visa type-specific) ----
eval_df <- 
  bind_rows(brc_simulation, claude, gemini) |> 
  left_join(actual_arrivals, by = "date")

# ---- Compute error metrics ----
metrics <- eval_df |>
  mutate(
    error = forecast_arrivals - actual_arrivals,
    abs_error = abs(error),
    sq_error = error^2,
    ape = ifelse(
      actual_arrivals == 0,
      NA_real_,
      abs_error / abs(actual_arrivals) * 100
    )
  ) |>
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

metrics

write_csv(metrics, "data/forecast-evaluation.csv")
