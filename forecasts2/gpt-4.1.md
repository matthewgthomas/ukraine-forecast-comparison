Here's a systematic method to forecast the **number of people from Ukraine arriving in the country in the next 3 months (≈13 weeks)**, given only 22 weeks of historical data on:

- Weekly visa applications
- Visas issued weekly
- Visa type (Family, Sponsorship, Government schemes)
- Arrivals with visas

### Methodology

#### 1. **Feature Engineering**
Given raw counts, engineer features that can provide leading indicator power and capture potential lags:

- **Lagged Features:** Arrivals in week *t* are likely due to visas issued in earlier weeks (e.g., 1-3 weeks earlier), so include lagged versions:
    - `visas_issued_lag1`, `visas_issued_lag2`, ...
    - `visa_applications_lag1`, etc.
- **Conversion Ratios:**
    - `applications_to_issued_ratio = visas_issued / visa_applications`
    - `issued_to_arrivals_ratio = arrivals / visas_issued` (potentially lagged)
- **Rolling Means:** Smooth noisy data
    - `visas_issued_rolling3 = mean(visas_issued over last 3 weeks)`
- **Disaggregate by Visa Type:**
    - For each scheme, create the above as separate features if possible.

#### 2. **Model Choice**
Twenty-two data points is **not enough for complex models** (like deep learning), so use a simple method:
- **Multiple Linear Regression** with lagged features
- Possibly try **regularization** if many correlated features (e.g., via `glmnet`)
- Alternatively, ARIMAX (ARIMA with regressors) using time series approach

#### 3. **Forecasting Future Values**
- Predict arrivals for next 13 weeks by **rolling forward** with the trained model.
- For future weeks’ regressors (e.g., future visa applications), use recent trends or averages (last 3-4 weeks); scenario planning may be needed.

---

## Example R Implementation

Assuming your data frame is something like:

```r
# Schema:
# week | visa_applications | visas_issued | type | arrivals
# type is a categorical variable with 3 values

library(dplyr)
library(tidyr)

# Sample Data Preparation (replace with your real data)
# df <- read.csv("your_data.csv")
```

### 1. Feature Engineering

```r
library(dplyr)
library(tidyr)
library(zoo)

# One-hot encoding visa type
df_wide <- df %>%
  pivot_wider(names_from = type,
              values_from = c(visa_applications, visas_issued, arrivals),
              values_fill = 0)

# Compute aggregates for all types
df_wide <- df_wide %>%
  mutate(
    visa_applications_total = rowSums(select(., starts_with("visa_applications_"))),
    visas_issued_total      = rowSums(select(., starts_with("visas_issued_"))),
    arrivals_total          = rowSums(select(., starts_with("arrivals_")))
  )

# Lagged features (assume max lag of 2 weeks)
df_wide <- df_wide %>%
  mutate(
    visas_issued_total_lag1 = lag(visas_issued_total, 1),
    visas_issued_total_lag2 = lag(visas_issued_total, 2),
    visa_applications_total_lag1 = lag(visa_applications_total, 1),
    issued_to_arrival_ratio_lag1 = lag(arrivals_total, 1) / lag(visas_issued_total, 2),
    visas_issued_rolling3 = rollmean(visas_issued_total, 3, align="right", fill=NA)
  )
```

### 2. Model Training

```r
# Create the training set, removing rows with NAs due to lags
train <- df_wide %>%
  filter(!is.na(visas_issued_total_lag2)) # adjust for max lag used

# Linear model: predict arrivals this week from past visas issued & rolling mean
model <- lm(arrivals_total ~ visas_issued_total_lag1 + visas_issued_total_lag2 + visas_issued_rolling3, data = train)
summary(model)
```

### 3. Forecast Next 13 Weeks

Because you don't know future visas issued, project using average of last few weeks:

```r
# Prepare to forecast next 13 weeks
last_week <- nrow(df_wide)
future_weeks <- 13

# Assume future visas_issued equals mean of last 3 weeks (for all variables needed)
future_visas_issued <- rep(mean(tail(df_wide$visas_issued_total, 3), na.rm=TRUE), future_weeks)
future_visas_issued_rolling3 <- rep(mean(tail(df_wide$visas_issued_rolling3, 3), na.rm=TRUE), future_weeks)

# Create forecasting data frame
forecast_df <- data.frame(
  visas_issued_total_lag1 = c(tail(df_wide$visas_issued_total, 1), future_visas_issued[-length(future_visas_issued)]),
  visas_issued_total_lag2 = c(tail(df_wide$visas_issued_total, 2), future_visas_issued[-c(length(future_visas_issued)-1, length(future_visas_issued))]),
  visas_issued_rolling3   = future_visas_issued_rolling3
)

# (Optional) Fill any NAs from head rows with most recent observed values
for(i in 1:ncol(forecast_df)) forecast_df[is.na(forecast_df[,i]),i] <- tail(df_wide[,colnames(forecast_df)[i]],1)

# Predict arrivals for next 13 weeks
predicted_arrivals <- predict(model, newdata=forecast_df)

# Output: total arrivals predicted over next 13 weeks
sum(predicted_arrivals)
```

---

## **Recap**
- **Data Used:** Lagged `visas_issued_total`, rolling mean, etc.
- **Model:** Multiple regression `arrivals_total ~ visas_issued_total_lag1 + visas_issued_total_lag2 + visas_issued_rolling3`
- **Features:** Engineered lags, rolling mean
- **Forecasting:** Project future input features with historical averages

You can further experiment with:
- Adding more lags/rolling windows
- Disaggregating by visa type if each scheme has a different lag/conversion
- Regularization (e.g. `glmnet`) to avoid overfit
- Time series models like ARIMAX (using `forecast::auto.arima` with regressors)

---

**If you provide a sample of your data, I can give a more customized, ready-to-run script!**