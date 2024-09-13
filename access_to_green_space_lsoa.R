pacman::p_load(readODS, tidyverse, glue, janitor, httr2)

# open the access to green space data ODS and create single table 
# for all the scenarios by LSOA to join with the LSOA table on ODS
# filter for LEP LSOA codes defined and accessible from the ODP 

# Retrieve LSOA codes from ODS ---
base_url <- "https://opendata.westofengland-ca.gov.uk/api/explore/v2.1"

endpoint <- "catalog/datasets"
dataset <- "lep_lsoa_geog"
export_endpoint <- "exports/csv"

response <- request(base_url) |> 
  req_url_path_append(endpoint) |> 
  req_url_path_append(dataset) |> 
  req_url_path_append(export_endpoint) |> 
  req_url_query(select = "lsoa21cd,ladnm",
                timezone = "UTC",
                use_labels = "false",
                epsg = 4326) |> 
  req_perform()

lsoa_codes_tbl <- response |> 
  resp_body_string() |> 
  read_csv2() 

lsoa_codes_lep_chr <- lsoa_codes_tbl |> 
  pull(lsoa21cd)

# Read data from the ODS File ----
path <- "data/Access_to_greenspace_England_2024.ods"
sheet_list <- readODS::ods_sheets(path)

# get the data about the scenarios
scenario_meta_name <- sheet_list |> 
  keep(~str_detect(.x, "contents"))

scenario_meta_tbl <- read_ods(path,
                               sheet = scenario_meta_name,
                               skip = 2) |> 
  clean_names() |> 
  mutate(scenario = glue("Scenario{worksheet_number}"),
         scenario_detail = str_extract(worksheet_title,
                                       "\\.\\s.+$") |> 
           str_remove("\\.\\s"))

# Get the scenario data itself
scenario_sheets <- sheet_list |> 
  keep(~str_detect(.x, "Scenario"))

scenario_tbl_list <- scenario_sheets |> 
  map(~read_ods(path, sheet = .x, skip = 5)) |> 
  set_names(scenario_sheets)

scenario_tbl_list |> write_rds("data/scenario_tbl_list.rds")

scenario_tbl_list <- read_rds("data/scenario_tbl_list.rds")

# function to take the list of scenario tbls and  filter \ mutate
filter_lsoas <- function(list_item, idx, lsoa_codes_lep_chr){

  list_item |> 
  clean_names() |>
  filter(lsoa21 %in% lsoa_codes_lep_chr) |>
  mutate(scenario = idx) |>
  glimpse()
}

# Make one long tbl with a scenario field to group on
filtered_scenario_long_tbl <- scenario_tbl_list |> 
  imap(~filter_lsoas(.x, .y, lsoa_codes_lep_chr)) |> 
  bind_rows() |> 
  left_join(scenario_meta_tbl |> 
              select(scenario, scenario_detail),
            by = join_by(scenario == scenario))

access_to_green_space_lep_ods_tbl <- filtered_scenario_long_tbl |> 
  group_by(msoa21, lsoa21, scenario_detail) |> 
  summarise(lsoa_household_count = sum(household_count),
            lsoa_household_with_access_count = sum(household_with_access_count)) |> 
pivot_wider(id_cols = c(msoa21, lsoa21, lsoa_household_count),
            names_from = scenario_detail,
            values_from = lsoa_household_with_access_count) |> 
  clean_names() |> 
  inner_join(lsoa_codes_tbl,
             by = join_by(lsoa21 == lsoa21cd)) |>
  mutate(prop_meets_neighbourhood_standard = round((neighbourhood_standard / lsoa_household_count) * 100, 1))

access_to_green_space_lep_ods_tbl |>
  ungroup() |> 
  filter(!ladnm %in% "North Somerset") |> 
  summarise(sum_nhood_standard = sum(neighbourhood_standard),
            sum_lsoa_household_count = sum(lsoa_household_count)) |> 
  mutate(prop_meets_neighbourhood_standard = round((sum_nhood_standard / sum_lsoa_household_count) * 100, 1)) |> 
  pull(prop_meets_neighbourhood_standard)



access_to_green_space_lep_ods_tbl |> 
  write_csv("data/access_to_green_space_lep_ods_new_tbl.csv")
  





