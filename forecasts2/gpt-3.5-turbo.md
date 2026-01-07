To forecast the number of people from Ukraine arriving in the country in the next 3 months, we can use a time series forecasting approach. We will first preprocess the data and engineer new features if necessary to improve the forecast accuracy. Next, we will build a forecasting model using the historical data and make predictions for the next 12 weeks (3 months).

Here is a step-by-step guide to implement the forecast in R:

1. Load the necessary libraries:

```R
library(forecast)
library(tidyr)
library(dplyr)
```

2. Load and preprocess the historical data:

Assuming the historical data is stored in a CSV file named `ukraine_data.csv`, you can load and preprocess the data using the following code:

```R
data <- read.csv("ukraine_data.csv")

# Convert date column to Date format
data$Date <- as.Date(data$Date)

# Create new features by aggregating weekly data
data_aggregated <- data %>%
  group_by(week = as.numeric(format(Date, "%U"))) %>%
  summarise(
    total_visa_applications = sum(visa_applications),
    total_visas_issued = sum(visas_issued),
    total_people_arriving = sum(people_arriving)
  )
```

3. Explore the data and identify any trends or patterns that could impact the forecast.

4. Build a forecasting model:

Based on the nature of the data and the trends observed, you can choose an appropriate time series forecasting model such as ARIMA, Exponential Smoothing, or Prophet to forecast the number of people arriving from Ukraine in the next 3 months.

Here's an example using ARIMA model for the forecast:

```R
# Convert data to time series
ts_data <- ts(data_aggregated$total_people_arriving, frequency = 52)

# Fit ARIMA model
fit <- auto.arima(ts_data)

# Forecast for the next 12 weeks (3 months)
forecast_values <- forecast(fit, h = 12)

# Print the forecasted values
print(forecast_values)
```

5. Evaluate the forecast accuracy:

You can evaluate the accuracy of the forecast using metrics like Mean Absolute Percentage Error (MAPE) or Root Mean Squared Error (RMSE).

6. Visualize the forecast:

You can plot the historical data along with the forecasted values to visualize the trend and make any necessary adjustments to the model.

This is a basic outline to get you started on forecasting the number of people from Ukraine arriving in the country in the next 3 months using R. Feel free to adjust and refine the model based on the characteristics of your data and the forecast accuracy.