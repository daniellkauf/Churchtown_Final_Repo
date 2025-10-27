# Load required packages
library(data.table)

# Read in data
elev <- fread("data_raw/Chamber_Elevation.csv")
main <- fread("data_clean/flux_data_all_vars_chamber_port_corrected.csv")

# If elev has a 'Date' column in text form (e.g., "9/6/23"), convert to DOY
elev[, DOY := yday(lubridate::mdy(Date))]

# main already has a numeric DOY column, so no need to parse Date
setnames(main, "Chamber_Corrected", "Chamber")

# Set keys for rolling join
setkey(elev, Pile, Chamber, DOY)
setkey(main, Pile, Chamber, DOY)

# Join by nearest DOY
main_with_elev <- elev[main, roll = "nearest"]

# Calculate difference in DOY (time offset)
main_with_elev[, DOY_diff := abs(DOY - i.DOY)]
main_with_elev[i.DOY < 249, Chamber_Elevation := NA]

setnames(main_with_elev, "Chamber", "Chamber_Corrected")

write_csv(main_with_elev, "data_clean/flux_data_all_vars_chamber_port_corrected_elevation.csv")


