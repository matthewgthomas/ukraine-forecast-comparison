library(tidyverse)
library(zoo)

## Load DLUHC data ----
source(
  "https://github.com/britishredcrosssociety/ukraine-analyses/raw/main/R/load%20Ukraine%20visa%20data%20-%20scraped.R"
)
source(
  "https://github.com/britishredcrosssociety/ukraine-analyses/raw/main/R/load%20Ukraine%20visa%20data%20-%20Local%20Authorities.R"
)

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
    `Weekly applications` = `visa applications received` -
      lag(`visa applications received`),
    `Weekly visas issued` = `visas issued` - lag(`visas issued`),
    `Weekly arrivals` = `arrivals of visa-holders in the UK` -
      lag(`arrivals of visa-holders in the UK`)
  ) |>
  select(Week, Scheme, starts_with("Weekly")) |>
  # Turn NAs into zeros
  mutate(across(where(is.numeric), \(x) replace_na(x, 0)))

weekly_sponsorship_scheme_visas <-
  visas_ltla21_summary |>
  arrange(Date) |>
  filter(str_detect(Type, "^Sponsored")) |>
  mutate(
    Scheme = if_else(
      str_detect(Type, "Government"),
      "Government 'super sponsored'",
      Type
    )
  ) |>
  # Calculate UK weekly totals
  mutate(
    Week = week(Date) + (52 * (year(Date) - 2022))
  ) |>
  group_by(Week, Scheme) |>
  summarise(
    `Number of visa applications` = sum(`Number of visa applications`),
    `Number of visas issued` = sum(`Number of visas issued`),
    `Number of arrivals` = sum(
      `Number of arrivals in the UK by sponsor location`
    )
  ) |>
  ungroup() |>
  # Calculate week-on-week changes
  group_by(Scheme) |>
  mutate(
    `Weekly applications` = `Number of visa applications` -
      lag(`Number of visa applications`),
    `Weekly visas issued` = `Number of visas issued` -
      lag(`Number of visas issued`),
    `Weekly arrivals` = `Number of arrivals` - lag(`Number of arrivals`)
  ) |>
  ungroup() |>
  select(Week, Scheme, starts_with("Weekly"))

# Since March 2024, the LA-level data has been published fortnightly rather than weekly
# This creates gaps when forecasting, so create a dataset with the full set of weeks using `expand_grid()`,
# then impute the 'missing' weeks
weekly_sponsorship_scheme_visas <-
  expand_grid(
    Week = min(weekly_sponsorship_scheme_visas$Week):max(visas_scraped$Week),
    Scheme = unique(weekly_sponsorship_scheme_visas$Scheme)
  ) |>
  left_join(weekly_sponsorship_scheme_visas) |>

  # Impute missing weeks
  group_by(Scheme) |>
  mutate(
    `Weekly applications` = zoo::na.approx(
      `Weekly applications`,
      na.rm = FALSE
    ),
    `Weekly visas issued` = zoo::na.approx(
      `Weekly visas issued`,
      na.rm = FALSE
    ),
    `Weekly arrivals` = zoo::na.approx(`Weekly arrivals`, na.rm = FALSE)
  ) |>

  # Fill in any remaining NAs - which will be the latest weeks (and, so, couldn't have been imputed)
  fill(starts_with("Weekly"), .direction = "down") |>

  ungroup()

weekly_visas_by_scheme <- bind_rows(
  weekly_sponsorship_scheme_visas,
  weekly_family_scheme_visas
) |>
  arrange(Week, Scheme) |>
  # Convert week number to date; source: https://stackoverflow.com/a/46183403
  mutate(Date = ymd("2022-01-03") + weeks(Week - 1)) |>
  relocate(Date) |>

  # Replace NAs with zeros
  mutate(
    across(
      c(`Weekly applications`, `Weekly visas issued`, `Weekly arrivals`),
      ~ replace_na(.x, 0)
    )
  )

# Save
write_csv(weekly_visas_by_scheme, "data/visas-weekly.csv")
