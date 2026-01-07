# Manual edits:
# - Load {slider} package
# - Load actual data and rename columns
# - Fixed duplicate column names on lines 118 and 138
# - Saved forecast data

# 1. Setup and Libraries --------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, fpp3, distributional, slider)

# 2. Synthetic Data Generation --------------------------------------------
# (Replace this block with your actual data loading)
# generate_data <- function() {
#   weeks <- 100
#   dates <- seq(as.Date("2022-03-01"), by = "week", length.out = weeks)
  
#   # Helper to create spike-and-decay pattern common in migration shocks
#   create_trend <- function(n, magnitude, decay) {
#     t <- 1:n
#     # Gamma distribution shape for "surge" + steady baseline
#     val <- dgamma(t, shape = 2, scale = 10) * magnitude * 100
#     val <- val + rnorm(n, mean = magnitude * 0.1, sd = magnitude * 0.02)
#     return(pmax(val, 0))
#   }
  
#   # Create raw tibble
#   expand_grid(
#     Week = dates,
#     Visa_Type = c("Ukraine Family Scheme", "Ukraine Sponsorship Scheme", "Government sponsored")
#   ) %>%
#     group_by(Visa_Type) %>%
#     mutate(
#       # Different scales for different schemes
#       scale = case_when(
#         Visa_Type == "Ukraine Family Scheme" ~ 100,
#         Visa_Type == "Ukraine Sponsorship Scheme" ~ 300,
#         TRUE ~ 50
#       ),
#       Applications = create_trend(n(), scale, 0.1),
      
#       # Visas issued lag behind applications (approx 2 weeks) + noise
#       Visas_Issued = lag(Applications, 2, default = 0) * runif(n(), 0.8, 0.95),
      
#       # Arrivals lag behind Visas (distributed lag) + noise
#       # Some people arrive quickly (2 weeks), some later (8 weeks)
#       Arrivals = (lag(Visas_Issued, 2, default = 0) * 0.4 + 
#                   lag(Visas_Issued, 4, default = 0) * 0.3 + 
#                   lag(Visas_Issued, 8, default = 0) * 0.1) * runif(n(), 0.9, 1.1)
#     ) %>%
#     ungroup() %>%
#     select(Week, Visa_Type, Applications, Visas_Issued, Arrivals) %>%
#     as_tsibble(index = Week, key = Visa_Type)
# }

# # Load the data
# df <- generate_data()

df <- read_csv("data/visas-weekly-training.csv")

df <- df |> 
  rename(
    Week = date,
    Visa_Type = visa_type,
    Applications = applications,
    Visas_Issued = visas_issued,
    Arrivals = arrivals
  ) |> 
  as_tsibble(index = Week, key = Visa_Type)

# 3. Feature Engineering --------------------------------------------------
# We enhance the data to capture "Pipeline" and "Stock" effects

df_engineered <- df %>%
  group_by(Visa_Type) %>%
  mutate(
    # Feature 1: Approval Ratio (Rolling 4-week average)
    # capturing changes in government processing strictness
    Approval_Rate = slide_dbl(Visas_Issued / (Applications + 1), mean, .before = 4, .complete = TRUE),
    
    # Feature 2: Visa Stock (Cumulative Unused Visas)
    # People who have visas but haven't arrived are 'at risk' of arriving
    # We estimate this as Cumulative Issued - Cumulative Arrivals
    Visa_Stock = cumsum(replace_na(Visas_Issued, 0)) - cumsum(replace_na(Arrivals, 0)),
    
    # Feature 3: Momentum (Week-on-Week change in arrivals)
    Arrival_Momentum = difference(Arrivals),
    
    # Feature 4: Weighted Lag for "Expected Arrivals"
    # A composite index of recent visa issuances
    Expected_Arrival_Flow = (lag(Visas_Issued, 1)*0.1 + 
                             lag(Visas_Issued, 4)*0.5 + 
                             lag(Visas_Issued, 8)*0.4)
  ) %>%
  # Fill NA caused by lags/rolling windows with simplistic backfill or 0
  filter(!is.na(Approval_Rate)) 

# 4. Simulation & Forecasting Pipeline ------------------------------------

# We need to forecast 3 months (12 weeks) ahead.
# PROBLEM: To predict Arrivals at T+12, we need Visas_Issued at T+8.
# SOLUTION: Hierarchical forecast. 
# Step A: Forecast Applications
# Step B: Forecast Visas (using forecasted Applications)
# Step C: Forecast Arrivals (using forecasted Visas)

HORIZON <- 12

# --- Step A: Forecast Applications ---
fit_apps <- df_engineered %>%
  model(
    # ETS handles trends and seasonality automatically
    App_Model = ETS(Applications)
  )

fc_apps <- fit_apps %>% forecast(h = HORIZON)

# --- Step B: Forecast Visas Issued ---
# We bind the forecasted applications to the future data to use as predictors
future_data_1 <- fc_apps %>%
  as_tsibble() %>%
  select(-Applications) |> 
  rename(Applications = .mean) %>%
  select(Week, Visa_Type, Applications)

fit_visas <- df_engineered %>%
  model(
    # Regression with ARIMA errors: Visas depend on Applications
    Visa_Model = ARIMA(Visas_Issued ~ Applications + PDQ(0,0,0))
  )

fc_visas <- fit_visas %>% 
  forecast(new_data = future_data_1)

# --- Step C: Forecast Arrivals ---
# Prepare the final future dataset with the forecasted drivers
future_data_2 <- fc_visas %>%
  as_tsibble() %>%
  select(-Visas_Issued) |> 
  rename(Visas_Issued = .mean) %>%
  select(Week, Visa_Type, Visas_Issued) %>%
  # Re-calculate the engineering features for the future frames
  # Note: For simplicity in this simulation, we approximate the "Stock" and "Expected" 
  # features using the new forecasted visa numbers.
  group_by(Visa_Type) %>%
  mutate(
    Expected_Arrival_Flow = (lag(Visas_Issued, 1, default = mean(Visas_Issued))*0.1 + 
                             lag(Visas_Issued, 4, default = mean(Visas_Issued))*0.5 + 
                             lag(Visas_Issued, 8, default = mean(Visas_Issued))*0.4)
  )

fit_arrivals <- df_engineered %>%
  model(
    # We use the Engineered Feature "Expected_Arrival_Flow" and "Visa_Stock"
    # The model learns how the "Expected Flow" maps to actual "Arrivals"
    Arrival_Model = TSLM(Arrivals ~ Expected_Arrival_Flow + trend())
  )

# 5. Final Prediction -----------------------------------------------------

fc_arrivals <- fit_arrivals %>%
  forecast(new_data = future_data_2)

# 6. Visualization and Results --------------------------------------------

# Plotting the forecast
fc_arrivals %>%
  autoplot(df_engineered %>% filter_index((max(Week) - 20) ~ .)) +
  labs(
    title = "3-Month Forecast: Ukrainian Arrivals by Visa Type",
    subtitle = "Simulation using Cascading Forecasts (Apps -> Visas -> Arrivals)",
    y = "Weekly Arrivals",
    x = "Week"
  ) +
  theme_minimal()

# Calculate Total Predicted Arrivals for next 3 months
total_prediction <- fc_arrivals %>%
  as_tibble() %>%
  summarise(
    Total_Predicted_Arrivals = sum(.mean),
    Lower_80_CI = sum(quantile(Arrivals, 0.1)),
    Upper_80_CI = sum(quantile(Arrivals, 0.9))
  )

print(paste("Total predicted arrivals in next 3 months:", round(total_prediction$Total_Predicted_Arrivals)))
print(total_prediction)

write_rds(fc_arrivals, "simulations/data/gemini-pro-3-forecast.rds")
