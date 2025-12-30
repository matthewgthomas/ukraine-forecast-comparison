# ──────────────────────────────────────────────────────────────
#  Libraries
# ──────────────────────────────────────────────────────────────
library(tidyverse)
library(lubridate)
library(slider)        # rolling windows
library(forecast)      # ARIMA + dynamic regression
library(tsibble)       # tidy time series

# ──────────────────────────────────────────────────────────────
#  0.  READ & RESHAPE DATA
#     columns required in the csv: Week, Scheme, Applications,
#     VisasIssued, Arrivals
# ──────────────────────────────────────────────────────────────
raw <- read_csv("ukraine_visa_weekly.csv") %>% 
        mutate(Week = yearweek(Week))         # coercion to <tsibble::yearweek>

# Pivot to one row per week, keep scheme level AND totals ----------------------
wide <- raw %>% 
  pivot_wider(names_from = Scheme,
              values_from = c(Applications, VisasIssued),
              names_sep = "_") %>% 
  group_by(Week) %>% 
  summarise(across(starts_with(c("Applications","VisasIssued")), sum, .names = "{.col}_tot"),
            Arrivals_tot = sum(Arrivals),
            .groups = "drop") %>% 
  # merge back the scheme detail:
  left_join(
       raw %>% 
         select(Week, Scheme, Applications, VisasIssued) %>% 
         pivot_wider(names_from = Scheme, values_from = c(Applications, VisasIssued),
                     names_sep = "_"),
       by = "Week")

# Keep the time order
wide <- wide %>% arrange(Week)

# ──────────────────────────────────────────────────────────────
#  1.  FEATURE ENGINEERING
# ──────────────────────────────────────────────────────────────
make_features <- function(df, prefix){
  
  issued      <- pull(df, paste0("VisasIssued_", prefix))
  arrivals    <- pull(df, "Arrivals_tot")
  applications<- pull(df, paste0("Applications_", prefix))
  
  tibble(
    !!paste0("Issued_",prefix,"_l1")      := lag(issued, 1),
    !!paste0("Issued_",prefix,"_l2")      := lag(issued, 2),
    !!paste0("Issued_",prefix,"_l3")      := lag(issued, 3),
    !!paste0("Issued_",prefix,"_l4")      := lag(issued, 4),
    !!paste0("Issued_",prefix,"_roll4")   := slide_dbl(issued, sum,
                                                       .before = 3, .complete = FALSE),
    !!paste0("Backlog_",prefix)           :=
          cumsum(replace_na(issued,0)) - cumsum(replace_na(arrivals,0)),
    !!paste0("Approval_",prefix)          := issued/(applications + 1),
    !!paste0("DeltaIssued_",prefix)       := issued - lag(issued,1)
  )
}

# Build features for each scheme and for the total ----------------------------
schemes <- c("FS","SS","GS","tot")          # abbreviations

features <- map_dfc(schemes, ~make_features(wide, .x))

data_mod <- bind_cols(wide %>% select(Week, Arrivals = Arrivals_tot), features) %>% 
            mutate(WeekNum = week(Week@yearmonth),    # simple seasonality
                   Month   = factor(month(Week@yearmonth))) %>% 
            as_tsibble(index = Week)

# ──────────────────────────────────────────────────────────────
# 2.  TRAIN / TEST SPLIT
# ──────────────────────────────────────────────────────────────
h      <- 13                         # forecast horizon = 13 weeks
train  <- head(data_mod, -h)
test   <- tail(data_mod,  h)

y_train  <- train$Arrivals
x_train  <- as.matrix(train %>% select(-Week, -Arrivals))

# ──────────────────────────────────────────────────────────────
# 3.  MODEL 1 – dynamic regression  (ARIMAX)
# ──────────────────────────────────────────────────────────────
fit <- auto.arima(y_train, xreg = x_train, seasonal = TRUE,
                  stepwise = FALSE, approximation = FALSE)

# ──────────────────────────────────────────────────────────────
# 4.  FORECAST EXOGENOUS REGRESSORS FOR THE NEXT 13 WEEKS
# ──────────────────────────────────────────────────────────────
# 4a.  Forecast visas issued totals; the most important driver
issued_tot <- ts(wide$VisasIssued_tot, frequency = 52)
fc_issued  <- forecast(auto.arima(issued_tot), h = h)$mean

# 4b.  Build future feature matrix
future_tbl <- tail(data_mod, 4) %>%           # keep last 4 rows for lag calculation
               as_tibble()

for (i in 1:h){
  
  next_row <- future_tbl %>% slice_tail(n = 1) %>% 
     mutate(Week = Week + 1,
            VisasIssued_tot = fc_issued[i])          # replace with forecast
  
  future_tbl <- bind_rows(future_tbl, next_row)
}

future_tbl <- tail(future_tbl, h)                    # keep the 13 new rows

# recompute *all* engineered features for the combined data --------------------
all_combined <- bind_rows(
                    data_mod %>% as_tibble(),
                    future_tbl) %>% 
               arrange(Week)

future_features <- tail(all_combined, h) %>% 
                   as_tsibble(index = Week)          # keep only xreg columns
x_future <- as.matrix(future_features %>% select(-Week, -Arrivals))

# ──────────────────────────────────────────────────────────────
# 5.  ARRIVALS FORECAST
# ──────────────────────────────────────────────────────────────
fc_arrivals <- forecast(fit, xreg = x_future, h = h)

autoplot(fc_arrivals) +
  ggtitle("Forecast : Arrivals of Ukrainian visa-holders (next 3 months)")

print(fc_arrivals$mean)       # point forecasts
print(fc_arrivals$lower)      # 80/95% intervals
print(fc_arrivals$upper)

# ──────────────────────────────────────────────────────────────
# 6.  SIMPLE ACCURACY CHECK ON THE HOLD-OUT SET
# ──────────────────────────────────────────────────────────────
accuracy(fc_arrivals, test$Arrivals)