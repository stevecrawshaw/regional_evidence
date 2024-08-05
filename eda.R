pacman::p_load(tidyverse,
               glue,
               janitor)


la_list <- c("South Gloucestershire", "Bristol, City of", "North Somerset", "Bath and North East Somerset")

ng_data <- read_csv("data/nged_ecr_nov_2023.csv")



lep_emb_cap <- ng_data %>% 
  clean_names() %>% 
  filter(county %in% la_list) %>% 
  select(where(~n_distinct(.) != 1))  # remove zvariance
