library(dplyr)

flux_data <- flux_data %>%
  mutate(Chamber_Corrected = case_when(
    DOY < 264 ~ paste0("C", PORT),
    DOY >= 264 & PORT == 1 ~ "C1",
    DOY >= 264 & PORT == 2 ~ "C2",
    DOY >= 264 & PORT == 3 ~ "C3",
    DOY >= 264 & PORT == 4 ~ "C6",  # switched with C4
    DOY >= 264 & PORT == 5 ~ "C5",
    DOY >= 264 & PORT == 6 ~ "C4",  # switched with C6
    TRUE ~ NA_character_
  ))

write_csv(flux_data, "data_clean/flux_data_all_vars_chamber_port_corrected.csv")
