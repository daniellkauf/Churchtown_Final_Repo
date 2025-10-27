
# scripts/prepare_flux_data.R
# prepares flux data and create a metadata guide

library(dplyr)

# Step 1: Read top 3 rows (instrument when applicable, var name, units)

header_path <- "data_raw/Volume_Corrected_Fluxes_40seconddeadpan_2023_allcovar.csv"
header_rows <- read.csv(header_path, header = FALSE, nrows = 3, stringsAsFactors = FALSE)

# Step 2: Extract column metadata
col_metadata <- tibble(
  Instrument = as.character(header_rows[1, ]),
  Variable = as.character(header_rows[2, ]),
  Units = as.character(header_rows[3, ])
)

# Step 3: Sanitize for safe variable names in R (but keep original names in metadata)
cleaned_names <- make.names(col_metadata$Variable, unique = TRUE)

# Step 4: Read the full data using cleaned names
flux_data <- read.csv(header_path, skip = 3, header = FALSE, stringsAsFactors = FALSE)
colnames(flux_data) <- cleaned_names

# Step 5: Save metadata
col_metadata$Clean_Name <- cleaned_names
write.csv(col_metadata, "docs/flux_variable_guide.csv", row.names = FALSE)

write.csv(flux_data, "data_clean/flux_data_formatted.csv", row.names = FALSE)
