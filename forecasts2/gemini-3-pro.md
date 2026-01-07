This is a challenging forecasting problem because the history is short (22 weeks) relative to the forecast horizon (12-13 weeks). In this scenario, complex machine learning models (like LSTMs) will overfit.

Instead, the most robust method is a **"Funnel" or "Pipeline" approach**. This relies on the causal logic that a person must be issued a visa before they can arrive. The "Issued Visas" data acts as a **leading indicator** for "Arrivals."

### The Methodology: Stock-Flow Pipeline

We will treat the migration flow as a physical system:

1. **Inflow:** Visas Issued.
2. **Stock:** The "Pool" of people holding valid visas who have not yet traveled.
3. **Outflow:** Arrivals.

**New Features to Engineer:**

* **`Visa_Stock` (The Pool):** The cumulative number of visas issued minus the cumulative number of arrivals. This represents the "pent-up demand" or the maximum potential number of immediate arrivals.
* *Formula:* 


* **`Arrival_Rate`:** The percentage of the "Visa Stock" that decides to travel in a given week.
* **`Lagged_Issued`:** Visas issued 1, 2, or 3 weeks ago (since travel isn't always immediate).

### The Forecasting Strategy (Two-Step)

Since we need to predict arrivals 3 months out, but arrivals depend on visas that *haven't been issued yet*, we must use a two-step chained forecast:

1. **Step 1:** Forecast the number of **Visas Issued** for the next 12 weeks (using a simple trend model like ETS).
2. **Step 2:** Forecast **Arrivals** using a Dynamic Regression model where `Arrivals` depends on the `Visa_Stock` and the (forecasted) `Issued` numbers.

---

### R Implementation

We will use the `fpp3` (Tidy Forecasting) ecosystem, which is modern, handles categorical data (Visa Types) elegantly, and integrates feature engineering seamlessly.

```r
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

```

### Why this design?

1. **Handling "Visa Stock" (Feature Engineering):**
The code calculates `Visa_Stock`. In migration theory, this is the most powerful predictor. If 50,000 visas are issued but only 10,000 have arrived, you have a "Stock" of 40,000 potential travelers. Even if visa issuance stops tomorrow, arrivals will continue as this stock depletes.
2. **Handling the Horizon Gap:**
Standard regression fails here because you don't have the input data (Issued Visas) for next month. By forecasting `Issued` first (Step 3 in code) and passing that as `new_data` to the Arrival model, we bridge the gap.
3. **Visa Types:**
The code keeps the data grouped by `VisaType`. This improves accuracy because the "Family Scheme" likely has a higher/faster arrival rate (higher urgency) than the "Sponsorship Scheme."

### Interpretation of Risks

* **Data Scarcity:** With only 22 points, statistical significance is low. The confidence intervals (generated automatically by `fpp3`) will be very wide—this is honest and correct.
* **Policy Shocks:** If the government changes the visa criteria next week, the historical correlation between Applications and Issued visas will break, invalidating the forecast.

Would you like me to refine the code to focus specifically on the "conversion rate" visualization to help you explain the model to stakeholders?