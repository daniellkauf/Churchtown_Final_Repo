# adding turning events to the data set
# days since last turning by pile. 

# add_turning_variables.R

library(dplyr)
library(readr)

# === Step 1: Load Data ===
flux_data <- read_csv("data_clean/flux_data_with_watchdog_BD_and_ec.csv")

# === Step 2: Define Turning Dates ===
# Pile C and E established on DOY 152
turning_dates <- list(
  C = as.numeric(c(152, 186, 194, 208, 222, 234, 249, 262, 276, 290)),
  E = as.numeric(c(152, 186, 208, 234, 262, 290))
)



# === Step 3: Functions ===

# Days since last turn
calculate_days_since_last_turn <- function(measurement_date, pile_type) {
  turns <- turning_dates[[pile_type]]
  past_turns <- turns[turns <= measurement_date]
  if (length(past_turns) == 0) {
    return(NA)
  } else {
    return(measurement_date - max(past_turns))
  }
}

# Total number of turns so far
calculate_total_turns <- function(measurement_date, pile_type) {
  turns <- turning_dates[[pile_type]]
  return(sum(turns <= measurement_date))
}

# === Step 4: Apply rowwise ===
flux_data <- flux_data %>%
  rowwise() %>%
  mutate(
    DaysSinceLastTurn = calculate_days_since_last_turn(DOY, Pile),
    TotalTurns = calculate_total_turns(DOY, Pile)
  ) %>%
  ungroup()

# Optional: Add Time Since Start of Experiment (e.g., Day 152.42 = June 1)
flux_data <- flux_data %>%
  mutate(TimeSinceStart = DOY - 152.4175)

# === Step 5: Save Output ===
write_csv(flux_data, "data_clean/flux_data_with_turning_vars.csv")
