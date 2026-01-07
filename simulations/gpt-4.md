Without the access to the actual data, here is a general step-by-step guideline of how to approach this simulation in R.

You might load the data first of all and install the required libraries:

```r
# Load packages
library(dplyr)
library(forecast)

# Load data
df <- read.csv("your_data.csv")
```

The data can be explored and organized:

```r 
# Data exploration
glimpse(df)

# Organize data
df <- df %>%
  mutate(Week = as.Date(Week))
```

Engineer some new features like if the visa was issued and number of individuals with each type of visa:

```r
# Feature engineering
df <- df %>%
  mutate(IssueYesNo = ifelse(Issue == "Yes", 1, 0),
         FamilyScheme = ifelse(Type == "Ukraine Family Scheme", 1, 0),
         SponsorshipScheme = ifelse(Type == "Ukraine Sponsorship Scheme", 1, 0),
         GovernmentSponsored = ifelse(Type == "Government sponsored", 1, 0))
```

For prediction, one might build a time-series model using the ARIMA method:

```r
# ARIMA model for prediction, the model is built on weekly data
fit <- auto.arima(df$Application)

# Forecast for next 3 months (about 12 weeks)
forecast_values <- forecast(fit, h = 12)
print(forecast_values)
```

Due to lack of data, it's impossible to give more personalized approach but this frame may offer a decent initial spot. However, it's important to bear in mind, the time series is barely the strategy and predicting migration is a complex topic that most likely would call for far more detailed analyses and disruption of features.