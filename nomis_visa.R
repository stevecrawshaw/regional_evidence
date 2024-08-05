library(tidyverse)

# Load the postcode lookup data
pc_lu_df <- read_csv("../weca_cesap/data/postcode_lookup/PCD_OA21_LSOA21_MSOA21_LAD_AUG23_UK_LU.csv")

# Inspect the first few rows to check field names
head(pc_lu_df)

# Get the postcode areas for the west of england
woe_lookup_df <- pc_lu_df %>%
  filter(str_detect(ladnm, 'Bristol|Bath and North East Somerset|South Gloucestershire')) %>%
  select(pcd7, ladnm) %>%
  mutate(pcd4 = substr(pcd7, 1, 4) %>% str_trim(side = "right")) %>%
  distinct(pcd4)

# Create a vector to use as a filter in the next stage
pcd4 <- woe_lookup_df %>% pull(pcd4)

# Load the nomis visa data
visa_nomis_raw_df <- read_csv("../regional_evidence/data/POSTAL_DISTRICT_Quarterly_indexed_map_data.csv")

# Inspect the first few rows
head(visa_nomis_raw_df)

# Time the operation
system.time({
  # Filter the data to only include the west of england postcodes
  # and extract the year and quarter from the time_period_value field
  ba_bs_df <- visa_nomis_raw_df %>%
    filter(cardholder_location %in% pcd4 | merchant_location %in% pcd4) %>%
    mutate(
      year = as.integer(substr(time_period_value, 1, 4)),
      quarter = as.integer(substr(time_period_value, nchar(time_period_value), nchar(time_period_value)))
    ) %>%
    select(-time_period_value)
})

# Inspect the first few rows
head(ba_bs_df)

# Write the filtered data to a CSV file
write_csv(ba_bs_df, '../regional_evidence/data/visa_nomis_weca.csv')
