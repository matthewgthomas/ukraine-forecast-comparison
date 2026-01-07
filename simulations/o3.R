############################################################
## 1.  PACKAGES  ###########################################
############################################################
library(tidyverse)      # data wrangling
library(lubridate)      # date helpers
library(tsibble)        # tidy time-series classes
library(fable)          # forecasting infrastructure
library(distributional) # to work with predictive dists
library(feasts)         # feature extraction
library(fabletools)     # modelling helpers

############################################################
## 2.  LOAD / PREPARE DATA ################################
############################################################
# df <- readr::read_csv("ukraine_visas_arrivals.csv")
# Make sure 'week' is a Date and 'visa_type' is a factor
df <- df %>%                      # <--- substitute your import
  mutate(
    week      = as_date(week),
    visa_type = factor(visa_type)
  ) %>% 
  arrange(week, visa_type)

############################################################
## 3.  FEATURE ENGINEERING ################################
############################################################
# We fit one model *per visa_type* because schemes behave differently.
# We keep it simple: arrivals_t  ~  lag(visas_issued,1:4) + lag(arrivals,1:4)
# You can add more engineered variables if helpful (e.g., backlog).

df <- df %>% 
  group_by(visa_type) %>% 
  arrange(week) %>% 
  mutate(
    backlog          = cumsum(visas_issued) - cumsum(arrivals),
    visas_issued_l1  = lag(visas_issued, 1),
    visas_issued_l2  = lag(visas_issued, 2),
    visas_issued_l3  = lag(visas_issued, 3),
    visas_issued_l4  = lag(visas_issued, 4),
    arrivals_l1      = lag(arrivals, 1),
    arrivals_l2      = lag(arrivals, 2),
    arrivals_l3      = lag(arrivals, 3),
    arrivals_l4      = lag(arrivals, 4)
  ) %>% 
  ungroup()

############################################################
## 4.  CONVERT TO TSIBBLE ##################################
############################################################
# fable wants an explicit time index.
df_ts <- df %>% 
  as_tsibble(index = week, key = visa_type)

############################################################
## 5.  SPLIT INTO TRAIN / FORECAST HORIZON #################
############################################################
h <- 12               # horizon = 12 weeks  ≈ 3 months
train_ts <- df_ts %>% filter_index(~ max(week) - weeks(h))

############################################################
## 6.  MODEL: NEGATIVE BINOMIAL GLM VIA FABLE ##############
############################################################
# We use NB to cope with count data over-dispersion.
# formula: arrivals ~ NB( . )
arrival_model <- train_ts %>% 
  model(
    nb = TSLM(
      arrivals ~ visas_issued_l1 + visas_issued_l2 + visas_issued_l3 + visas_issued_l4 +
                 arrivals_l1      + arrivals_l2      + arrivals_l3      + arrivals_l4 +
                 backlog
    )
  )

glance(arrival_model)

############################################################
## 7.  GENERATE FUTURE COVARIATES (SCENARIO) ###############
############################################################
# To simulate the next 12 weeks we need assumed paths for:
# (a) future visas issued
# (b) future backlog (which depends on visas_issued and arrivals)
#
# Below, we take a *simple* approach:
#  – carry forward the median of the last 4 weeks of visas_issued.
#  – visa applications are not used directly in this specification,
#    but could be modelled in a similar way and fed through a
#    “visa_apps -> visas_issued” model if desired.

make_future <- function(df_slice, h = 12) {
  last_week <- max(df_slice$week)
  med_issue <- median(tail(df_slice$visas_issued, 4), na.rm = TRUE)
  
  new_weeks <- tibble(
    week         = seq(from = last_week + 7, by = 7, length.out = h),
    visa_type    = unique(df_slice$visa_type),
    visas_issued = med_issue
  )
  
  bind_rows(df_slice, new_weeks) %>% 
    arrange(week) %>% 
    mutate(
      visas_issued_l1 = lag(visas_issued, 1),
      visas_issued_l2 = lag(visas_issued, 2),
      visas_issued_l3 = lag(visas_issued, 3),
      visas_issued_l4 = lag(visas_issued, 4),
      arrivals_l1     = lag(arrivals,      1, order_by = week),
      arrivals_l2     = lag(arrivals,      2, order_by = week),
      arrivals_l3     = lag(arrivals,      3, order_by = week),
      arrivals_l4     = lag(arrivals,      4, order_by = week)
    )
}

future_ts <- df_ts %>% 
  group_by_key() %>% 
  group_map(~ make_future(.x, h = h)) %>% 
  bind_rows() %>% 
  as_tsibble(index = week, key = visa_type)

############################################################
## 8.  FORECAST (WITH 1000 PATH SIMULATION) ################
############################################################
set.seed(42)
fc <- forecast(arrival_model, new_data = future_ts, bootstrap = TRUE, times = 1000)

############################################################
## 9.  SUMMARISE / VISUALISE ###############################
############################################################
# point + 80% & 95% intervals by week + aggregated total
fc_summarised <- fc %>% 
  hilo(level = c(80, 95)) %>% 
  unpack_hilo(`80%`) %>% 
  rename(q80_lower = `80%_lower`, q80_upper = `80%_upper`) %>% 
  unpack_hilo(`95%`) %>% 
  rename(q95_lower = `95%_lower`, q95_upper = `95%_upper`)

# Plot per scheme
autoplot(fc, level = 95) +
  labs(title = "Forecast of weekly arrivals from Ukraine",
       subtitle = "95% bootstrap predictive intervals",
       y = "Arrivals")

# Total arrivals across all schemes for the horizon
total_fc <- fc %>% 
  as_tibble() %>% 
  mutate(.id = NULL) %>% 
  group_by(week) %>% 
  summarise(.mean = sum(.mean),
            .lower = sum(.lower),
            .upper = sum(.upper))

print(total_fc)

############################################################
## 10.  EXPORT RESULTS #####################################
############################################################
# readr::write_csv(total_fc, "ukraine_arrivals_3_month_forecast.csv")