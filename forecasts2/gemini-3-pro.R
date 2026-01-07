# 1. Setup and Synthetic Data Generation ---------------------------------------
library(tidyverse)
library(fpp3)

set.seed(123)

# Create dummy dates for 22 weeks of history
dates <- seq(as.Date("2024-01-01"), by = "week", length.out = 22)

# Generate synthetic data for 3 visa types
# We simulate a "pulse" of applications that is slowing down
visa_types <- c("Ukraine Family Scheme", "Ukraine Sponsorship Scheme", "Government sponsored")

generate_series <- function(v_type) {
  t <- 1:22
  # Applications: downward trend with noise
  apps <- pmax(10, as.integer(1000 * exp(-0.1 * t) + rnorm(22, 0, 50)))
  
  # Issued: 80% of apps, lagged by approx 1 week (simplified)
  issued <- as.integer(lag(apps, 1, default = 800) * 0.8 + rnorm(22, 0, 20))
  
  # Arrivals: 60% of issued, lagged by approx 2 weeks
  arrivals <- as.integer(lag(issued, 2, default = 500) * 0.6 + rnorm(22, 0, 30))
  
  tibble(
    Week = dates,
    VisaType = v_type,
    Applications = apps,
    Issued = issued,
    Arrivals = arrivals
  )
}

# Combine into a tsibble (Time Series Tibble)
data_tbl <- map_dfr(visa_types, generate_series) %>%
  as_tsibble(index = Week, key = VisaType)

# 2. Feature Engineering -------------------------------------------------------

# We group by VisaType to calculate stocks specific to each scheme
data_features <- data_tbl %>%
  group_by(VisaType) %>%
  mutate(
    # Feature 1: Cumulative Stock (People with visas who haven't arrived)
    # This is a crucial predictor of immediate future flow
    Cum_Issued = cumsum(Issued),
    Cum_Arrivals = cumsum(Arrivals),
    Visa_Stock = Cum_Issued - Cum_Arrivals,
    
    # Feature 2: Arrival Ratio (Current Arrivals / Available Stock)
    # Useful for diagnostics, though we let the regression learn the coefficient
    Arrival_Prop = Arrivals / lag(Visa_Stock, 1, default = 1)
  ) %>%
  ungroup() 

# 3. Step 1: Forecast the Predictor ("Issued") ---------------------------------
# We cannot predict Arrivals 3 months out without knowing how many 
# visas will be issued in those 3 months.

# We fit an ETS (Error, Trend, Seasonal) model to 'Issued'
issued_model <- data_features %>%
  model(ETS(Issued))

# Forecast 'Issued' for the next 12 weeks (approx 3 months)
issued_forecast <- issued_model %>%
  forecast(h = 12)

# 4. Step 2: Forecast Arrivals using Dynamic Regression ------------------------

# We fit a model where Arrivals depends on the Visa Stock and current Issuance.
# TSLM = Time Series Linear Model. 
# We use 'trend()' to capture time decay, and 'Visa_Stock' as a regressor.
arrival_model <- data_features %>%
  model(
    # Model: Arrivals determined by how many people are waiting (Stock)
    # and a lag of recently issued visas.
    # We use TSLM for interpretability given the small sample size.
    dyn_reg = TSLM(Arrivals ~ Visa_Stock + lag(Issued, 1) + trend())
  )

# Prepare the future scenario data
# We need to calculate the FUTURE 'Visa_Stock' based on our 'Issued' forecast.
# This requires a loop or iterative calculation because:
# Future Stock(t) = Stock(t-1) + Forecasted_Issued(t) - Forecasted_Arrivals(t)
# However, for a simple approximation in TSLM, we can project the Stock 
# based on the net-change trend or use the 'Issued' forecast as a proxy.

# SIMPLIFIED APPROACH for robustness:
# We will use 'Issued' as the primary regressor (xreg) since Stock is collinear with it.
arrival_model_robust <- data_features %>%
  model(
    ARIMA(Arrivals ~ Issued + trend())
  )

# We feed the *forecasted* Issued values (from Step 1) into the Arrival model
final_forecast <- arrival_model_robust %>%
  forecast(new_data = issued_forecast)

# 5. Visualization -------------------------------------------------------------
final_forecast %>%
  autoplot(data_tbl) +
  labs(title = "Forecast of Ukrainian Arrivals (Next 3 Months)",
       y = "Weekly Arrivals",
       x = "Date") +
  theme_minimal()

# 6. Aggregate Totals ----------------------------------------------------------
# If the user wants the total number across all schemes:
total_forecast <- final_forecast %>%
  index_by(Week) %>%
  summarise(
    Total_Predicted_Arrivals = sum(.mean),
    Lower_CI_80 = sum(quantile(Arrivals, 0.1)),
    Upper_CI_80 = sum(quantile(Arrivals, 0.9))
  )

print(head(total_forecast))