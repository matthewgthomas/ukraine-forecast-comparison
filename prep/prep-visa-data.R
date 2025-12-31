library(tidyverse)
library(zoo)

## Load DLUHC data ----
source(
  "https://github.com/britishredcrosssociety/ukraine-analyses/raw/main/R/load%20Ukraine%20visa%20data%20-%20scraped.R"
)
source(
  "https://github.com/britishredcrosssociety/ukraine-analyses/raw/main/R/load%20Ukraine%20visa%20data%20-%20Local%20Authorities.R"
)

# "week","visa_type","applications","visas_issued","arrivals"

## Make a dataframe containing weekly visa data ----
# Containing weekly applications, visas issued, and arrivals for:
# - Family Scheme (from scraped data)
# - Homes for Ukraine / individual sponsorship (from LA-level data)
# - Super sponsor / government sponsorship (from LA-level data)
weekly_family_scheme_visas <-
  visas_scraped |>
  arrange(Date) |>
  filter(str_detect(
    Stage,
    "arrival|visas issued|visa applications received"
  )) |>
  filter(Scheme == "Ukraine Family Scheme") |>
  select(-Date, -Visas) |>
  pivot_wider(
    names_from = Stage,
    values_from = Visas_imputed,
    values_fn = mean
  ) |>
  # Calculate week-on-week changes
  mutate(
    applications = `visa applications received` -
      lag(`visa applications received`),
    visas_issued = `visas issued` - lag(`visas issued`),
    arrivals = `arrivals of visa-holders in the UK` -
      lag(`arrivals of visa-holders in the UK`)
  ) |>
  select(
    week = Week,
    visa_type = Scheme,
    applications,
    visas_issued,
    arrivals
  ) |>
  # Turn NAs into zeros
  mutate(across(where(is.numeric), \(x) replace_na(x, 0)))

weekly_sponsorship_scheme_visas <-
  visas_ltla21_summary |>
  arrange(Date) |>
  filter(str_detect(Type, "^Sponsored")) |>
  mutate(
    visa_type = case_when(
      str_detect(Type, "Government") ~ "Government sponsored",
      Type == "Sponsored by individuals" ~ "Ukraine Sponsorship Scheme",
      .default = Type
    )
  ) |>
  # Calculate UK weekly totals
  mutate(
    week = week(Date) + (52 * (year(Date) - 2022))
  ) |>
  group_by(week, visa_type) |>
  summarise(
    `Number of visa applications` = sum(`Number of visa applications`),
    `Number of visas issued` = sum(`Number of visas issued`),
    `Number of arrivals` = sum(
      `Number of arrivals in the UK by sponsor location`
    )
  ) |>
  ungroup() |>
  # Calculate week-on-week changes
  group_by(visa_type) |>
  mutate(
    applications = `Number of visa applications` -
      lag(`Number of visa applications`),
    visas_issued = `Number of visas issued` -
      lag(`Number of visas issued`),
    arrivals = `Number of arrivals` - lag(`Number of arrivals`)
  ) |>
  ungroup() |>
  select(week, visa_type, applications, visas_issued, arrivals)

# Since March 2024, the LA-level data has been published fortnightly rather than weekly
# This creates gaps when forecasting, so create a dataset with the full set of weeks using `expand_grid()`,
# then impute the 'missing' weeks
weekly_sponsorship_scheme_visas <-
  expand_grid(
    week = min(weekly_sponsorship_scheme_visas$week):max(visas_scraped$Week),
    visa_type = unique(weekly_sponsorship_scheme_visas$visa_type)
  ) |>
  left_join(weekly_sponsorship_scheme_visas) |>

  # Impute missing weeks
  group_by(visa_type) |>
  mutate(
    applications = zoo::na.approx(
      applications,
      na.rm = FALSE
    ),
    visas_issued = zoo::na.approx(
      visas_issued,
      na.rm = FALSE
    ),
    arrivals = zoo::na.approx(arrivals, na.rm = FALSE)
  ) |>

  # Fill in any remaining NAs - which will be the latest weeks (and, so, couldn't have been imputed)
  fill(applications:arrivals, .direction = "down") |>
  ungroup() |>
  select(week, visa_type, applications, visas_issued, arrivals)

weekly_visas_by_scheme <- bind_rows(
  weekly_sponsorship_scheme_visas,
  weekly_family_scheme_visas
) |>
  arrange(week, visa_type) |>
  # Convert week number to date; source: https://stackoverflow.com/a/46183403
  mutate(date = ymd("2022-01-03") + weeks(week - 1)) |>
  relocate(date) |>
  select(-week) |>

  # Replace NAs with zeros
  mutate(
    across(
      c(applications, visas_issued, arrivals),
      ~ replace_na(.x, 0)
    )
  )

# Save
write_csv(weekly_visas_by_scheme, "data/visas-weekly.csv")

# Split visa data into train and test sets, using 24th August 2022 as the cutoff
# This is when I first developed and ran the forecasts
weekly_visas_by_scheme |>
  filter(date <= ymd("2022-08-24")) |>
  write_csv("data/visas-weekly-training.csv")

weekly_visas_by_scheme |>
  filter(date > ymd("2022-08-24")) |>
  write_csv("data/visas-weekly-test.csv")
