# scripts/add_bulk_density_nearest.R

library(dplyr)
library(readr)
library(lubridate)
library(purrr)

# Load formatted flux data
flux_data <- read_csv("data_clean/flux_data_formatted.csv") %>%
  mutate(
    DOY = round(`DOY.initial_value`),
    Pile = case_when(
      PORT %in% c(1, 2, 3) ~ "C",
      PORT %in% c(4, 5, 6) ~ "E",
      TRUE ~ NA_character_
    )
  )

# Load bulk density and select only needed columns
bulk_density <- read_csv("data_raw/BD_2023.csv") %>%
  select(DOY, Pile, BulkDensity = `grams/cm^3`)

# Match each flux row to the closest DOY in bulk density
flux_data <- flux_data %>%
  rowwise() %>%
  mutate(
    CDOY = find_nearest_bd(DOY, Pile)
  ) %>%
  ungroup() %>%
  left_join(bulk_density, by = c("CDOY" = "DOY", "Pile"))



# Save updated dataset
write_csv(flux_data, "data_clean/flux_data_with_CDOY_BD.csv")
