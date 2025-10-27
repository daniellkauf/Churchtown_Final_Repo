# Load libraries
library(dplyr)
library(readr)
library(lubridate)

# Step 1: Load flux data
flux_data <- read_csv("data_clean/flux_data_with_watchdog_bulkdensity.csv")

# Step 2: Load EC data
ec_data <- read_csv("data_raw/Dairy_Merged_file.csv")

# Keep only relevant EC variables and ensure DOY is numeric
ec_data <- ec_data %>%
  select(DOY, all_of(c("air_temperature", "RH", "VPD", "Tdew", 
                       "wind_speed", "wind_dir", "u*", "TKE", 
                       "H", "LE", "air_pressure", "bowen_ratio", 
                       "ET", "specific_humidity"))) %>%
  filter(!is.na(DOY)) %>%
  mutate(DOY = as.numeric(DOY))

# Assume both data.tables are set and DOY_decimal used for precision
setDT(flux_data)
setDT(ec_data)

# Make sure DOY columns align
setnames(flux_data, "DOY_decimal", "DOY")
setnames(ec_data, "DOY_decimal", "DOY")

# Rolling join
flux_ec <- ec_data[flux_data, on = "DOY", roll = "nearest"]

# Clean column names
flux_ec <- flux_ec %>%
  rename(DATE_TIME = DATE_TIME.initial_value) %>%
  relocate(DOY, .after = DATE_TIME)

# all data is added to flux_ec lets save this as a csv form which we will be doing data cleaning.

write_csv(flux_ec, "data_clean/flux_data_with_watchdog_BD_and_ec.csv")



