Here is an example code in R to simulate the number of people from Ukraine arriving in the country in the next 3 months based on the provided data. In this simulation, we will use a simple linear regression model to predict the number of people arriving.

```R
# Import necessary libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(caret)

# Create sample data
set.seed(123)
data <- tibble(
  week = 1:12,
  visa_applications = c(50, 60, 70, 80, 90, 100, 110, 120, 130, 140, 150, 160),
  visas_issued = c(40, 50, 60, 70, 80, 90, 100, 110, 120, 130, 140, 150),
  visa_type = rep(c("Ukraine Family Scheme", "Ukraine Sponsorship Scheme", "Government sponsored"), each = 4),
  people_arriving = c(30, 40, 50, 60, 70, 80, 90, 100, 110, 120, 130, 140)
)

# Engineer new features if necessary
data <- data %>%
  mutate(visa_approval_rate = visas_issued / visa_applications)

# Create linear regression model
model <- lm(people_arriving ~ visa_applications + visas_issued + visa_approval_rate, data = data)

# Predict the number of people arriving in the next 3 months
future_data <- tibble(
  week = 13:24,
  visa_applications = c(170, 180, 190, 200, 210, 220, 230, 240, 250, 260, 270, 280),
  visas_issued = c(160, 170, 180, 190, 200, 210, 220, 230, 240, 250, 260, 270),
  visa_type = rep(c("Ukraine Family Scheme", "Ukraine Sponsorship Scheme", "Government sponsored"), each = 4)
)

future_data <- future_data %>%
  mutate(visa_approval_rate = visas_issued / visa_applications)

future_data$people_arriving_predicted <- predict(model, newdata = future_data)

# Print the predicted number of people arriving in the next 3 months
print(future_data)
```

This code creates a linear regression model using the provided data and then makes predictions on the number of people arriving from Ukraine in the next 3 months based on the visa applications, visas issued, and visa approval rate. Feel free to modify the code or data as needed for more accurate predictions.