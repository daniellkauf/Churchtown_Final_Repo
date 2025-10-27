library(readr)
library(dplyr)
library(lubridate)
library(data.table)

# -- Step 1: Read & format weather data
weather_header <- read_delim("data_raw/Base Weather Station Data Field Season 2023.txt",
                             delim = "\t", n_max = 3, col_names = FALSE)

weather_metadata <- tibble(
  PrettyName = as.character(weather_header[1, ]),
  Units = as.character(weather_header[2, ]),
  CodeName = as.character(weather_header[3, ])
)

weather_data <- read_delim("data_raw/Base Weather Station Data Field Season 2023.txt",
                           delim = "\t", skip = 3, col_names = FALSE)

colnames(weather_data) <- make.names(weather_metadata$CodeName, unique = TRUE)
colnames(weather_data)[1] <- "TIMESTAMP"

# -- Step 2: Create decimal DOY for weather data and filter to 2023
weather_data <- weather_data %>%
  mutate(
    TIMESTAMP = as.POSIXct(TIMESTAMP, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"),
    DOY_decimal = yday(TIMESTAMP) +
      hour(TIMESTAMP) / 24 +
      minute(TIMESTAMP) / 1440 +
      second(TIMESTAMP) / 86400
  ) %>%
  filter(year(TIMESTAMP) == 2023) %>%   # keep only 2023 values
  select(DOY_decimal, TMPA, VWCB, PAR, BAR, HMD, TMP, RNF, WND, WNG, WNS, DEW)

# -- Step 3: Load & prepare flux data
flux_data <- read_csv("data_clean/flux_data_with_CDOY_BD.csv")

flux_data <- flux_data %>%
  mutate(
    DOY_decimal = as.numeric(DOY.initial_value)  # if not already numeric
  )

# -- Step 4: Perform nearest join on DOY
setDT(flux_data)
setDT(weather_data)
setkey(weather_data, DOY_decimal)

flux_weather <- weather_data[flux_data, on = "DOY_decimal", roll = "nearest"]

# -- Step 5: Save final merged dataset
write_csv(flux_weather, "data_clean/flux_data_with_watchdog_doy.csv")
write_csv(weather_metadata, "docs/weather_watchdog_variable_guide.csv")

