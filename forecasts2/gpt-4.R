data <- read.csv("your_data_file.csv")
data$Date <- as.Date(data$Date, format = "%Y-%m-%d")
data_ts <- ts(data[,2:5], start = c(2022, 1), frequency = 52)

plot(data_ts)
decomposed_ts <- decompose(data_ts)
plot(decomposed_ts)

data$Visa_Issue_Rate <- data$Visas.Issued/data$Visa.Applications
data$Arrival_Rate <- data$Arrivals/data$Visas.Issued

library(forecast)
fit <- auto.arima(data_ts$Arrivals)

future_forecast <- forecast(fit, h = 12) #h is the number of periods for forecasting
plot(future_forecast)