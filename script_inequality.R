#### This script calculates 20:20 Income Deprivation inequality ratios for 174 LEA-level local 
#### authorities in England and Wales, using the method from Nyanzu & Rae

library(tidyverse)
library(janitor)
library(httr)
library(rvest)

# LSOA to LEA lookup (geography matching by Webb & Thomas, CWIP App 2019 using DfE school data)
lsoa_lea_lookup <- read_csv("lsoa_lea_lookup.csv")


# LSOA level income deprivation deciles 2015 - England
income_dep_2015 <- read_csv("https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/467774/File_7_ID_2015_All_ranks__deciles_and_scores_for_the_Indices_of_Deprivation__and_population_denominators.csv") %>%
  clean_names() %>%
  select(lsoa11cd = lsoa_code_2011, 
         income_dep_rate = income_score_rate, 
         income_dep_decile = income_decile_where_1_is_most_deprived_10_percent_of_lso_as, 
         idaci_rate = income_deprivation_affecting_children_index_idaci_score_rate, 
         idaci_decile = income_deprivation_affecting_children_index_idaci_decile_where_1_is_most_deprived_10_percent_of_lso_as)



# LSOA level income deprivation deciles 2019 - England
income_dep_2019 <- read_csv("https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/845345/File_7_-_All_IoD2019_Scores__Ranks__Deciles_and_Population_Denominators_3.csv") %>%
  clean_names() %>%
  select(lsoa11cd = lsoa_code_2011, 
         income_dep_rate = income_score_rate, 
         income_dep_decile = income_decile_where_1_is_most_deprived_10_percent_of_lso_as, 
         idaci_rate = income_deprivation_affecting_children_index_idaci_score_rate, 
         idaci_decile = income_deprivation_affecting_children_index_idaci_decile_where_1_is_most_deprived_10_percent_of_lso_as)

# Join English data

english_id <- left_join(income_dep_2015, income_dep_2019, suffix = c("_2015", "_2019"), by = "lsoa11cd")


# LSOA level income deprivation deciles 2014 - Wales

# LSOA code to StatsWales LSOA name lookup
stats_wales_name_lookup <- tempfile()

download.file(stats_wales_name_lookup, url = "https://statswales.gov.wales/Download/File?fileId=570")

statswales_lookup <- readODS::read_ods(stats_wales_name_lookup, sheet = 2) %>%
  as_tibble() %>%
  clean_names() %>%
  select(lsoa11code = lower_layer_super_output_area_lsoa_code, lsoa_name = lsoa_name)


#Income deprivarion 2014

wales_id_14 <- read_html("https://statswales.gov.wales/v/IDpY")
  
wales_id_14_data <- wales_id_14 %>%
  html_nodes(xpath = '//*[@id="pivotGrid_MT"]') %>%
  html_table(fill = TRUE, header = TRUE, trim = TRUE) %>%
  as.data.frame(.) %>%
  as_tibble() %>%
  clean_names() %>%
  slice(14:nrow(.)) %>%
  select(2:4) 

names(wales_id_14_data) <- c("lsoa_sw_name", "wimd_dec", "inc_dec")

wales_id_14_data


# LSOA level income deprivation deciles 2019 - Wales

wales_id_19 <- read_html("https://statswales.gov.wales/v/IDsO")

wales_id_19_data <- wales_id_19 %>%
  html_nodes(xpath = '//*[@id="pivotGrid_MT"]') %>%
  html_table(fill = TRUE, header = TRUE, trim = TRUE) %>%
  as.data.frame(.) %>%
  as_tibble() %>%
  clean_names() %>%
  slice(15:nrow(.)) %>%
  select(3:5) 

names(wales_id_19_data) <- c("lsoa_sw_name", "wimd_dec", "inc_dec")

wales_id_19_data

# Join welsh data
welsh_id <- left_join(wales_id_14_data, wales_id_19_data, suffix = c("_2014", "_2019"), by = "lsoa_sw_name") %>%
  left_join(statswales_lookup, by = c("lsoa_sw_name" = "lsoa_name"))

# Welsh income deprivation data (need to calculate deciles from scratch to make relative to the whole country)

wales_id_15 <- read_html("https://statswales.gov.wales/v/IDyZ")

wales_id_15_data <- wales_id_15 %>%
  html_nodes(xpath = '//*[@id="pivotGrid_MT"]') %>%
  html_table(fill = TRUE, header = TRUE, trim = TRUE) %>%
  as.data.frame(.) %>%
  as_tibble() %>%
  clean_names() %>%
  slice(20:nrow(.)) %>%
  select(3:4) 

names(wales_id_15_data) <- c("lsoa11nm", "id_rate_2015")

wales_id_15_data

welsh_id <- left_join(welsh_id, wales_id_15_data, by = c("lsoa_sw_name" = "lsoa11nm"))


### select only income deprivation columns and rates (for 2015)
english_idd <- english_id %>%
  select(lsoa11cd, income_dep_decile_2015, income_dep_decile_2019, income_dep_rate_2015)

welsh_idd <- welsh_id %>%
  select(lsoa11cd = lsoa11code, income_dep_decile_2015 = inc_dec_2014, income_dep_decile_2019 = inc_dec_2019, income_dep_rate_2015 = id_rate_2015) %>%
  mutate(income_dep_decile_2015 = as.double(income_dep_decile_2015), income_dep_decile_2019 = as.double(income_dep_decile_2019), income_dep_rate_2015 = as.double(income_dep_rate_2015))

welsh_idd

idd_combined <- bind_rows(english_idd, welsh_idd)

idd_combined

# calculate England and Wales comparable rates for 2015
idd_combined <- idd_combined %>%
  mutate(
    income_dep_decile_2015_ew = ntile(desc(income_dep_rate_2015), 10)
  ) %>%
  select(-income_dep_rate_2015)


# Add LEA level LA grouping data
idd_combined <- left_join(idd_combined, lsoa_lea_lookup %>% select(LSOA11CD, LA_name), by = c("lsoa11cd" = "LSOA11CD")) 
  
idd_combined

# Calculate ratio according to http://ajrae.staff.shef.ac.uk/atlasofinequality/reports/tech_report_aoi_21_nov_2019.pdf p.9
# • Calculated the absolute difference in the number of LSOA within the top 20% and the bottom 20% of the Income domain of the English Indices of Deprivation 2019.
# • These results were then expressed as a percentage of the total number of LSOA within each TTWA (See Appendix IV for an illustration).

nyanzu_rae_indices <- idd_combined %>%
  mutate(
    low_flag_2015 = ifelse(income_dep_decile_2015 == 9 | income_dep_decile_2015 == 10, 1, 0),
    high_flag_2015 = ifelse(income_dep_decile_2015 == 1 | income_dep_decile_2015 == 2, 1, 0),
    low_flag_2015_ew = ifelse(income_dep_decile_2015_ew == 9 | income_dep_decile_2015_ew == 10, 1, 0),
    high_flag_2015_ew = ifelse(income_dep_decile_2015_ew == 1 | income_dep_decile_2015_ew == 2, 1, 0),
    low_flag_2019 = ifelse(income_dep_decile_2019 == 9 | income_dep_decile_2019 == 10, 1, 0),
    high_flag_2019 = ifelse(income_dep_decile_2019 == 1 | income_dep_decile_2019 == 2, 1, 0)
  ) %>%
  group_by(LA_name) %>%
  mutate(
    n_lsoas = n()
  ) %>%
  summarise(
    LA_name = first(LA_name),
    n_lsoas = first(n_lsoas),
    sum_low20_2015 = sum(low_flag_2015),
    sum_low20_2015_ew = sum(low_flag_2015_ew),
    sum_low20_2019 = sum(low_flag_2019),
    sum_high20_2015 = sum(high_flag_2015),
    sum_high20_2015_ew = sum(high_flag_2015_ew),
    sum_high20_2019 = sum(high_flag_2019)
  ) %>%
  mutate(
   abs_diff_2015 = abs(sum_low20_2015 - sum_high20_2015),
   abs_diff_2015_ew = abs(sum_low20_2015_ew - sum_high20_2015_ew),
   abs_diff_2019 = abs(sum_low20_2019 - sum_high20_2019),
   nyanzu_rae_2020index_2015 = abs_diff_2015 / n_lsoas,
   nyanzu_rae_2020index_2015_ew = abs_diff_2015_ew / n_lsoas,
   nyanzu_rae_2020index_2019 = abs_diff_2019 / n_lsoas
  ) %>%
  select(LA_name, nyanzu_rae_2020index_2015, nyanzu_rae_2020index_2015_ew, nyanzu_rae_2020index_2019)

nyanzu_rae_indices

# Save csv
write_csv(nyanzu_rae_indices, "lea_ineq_index/nyanzu_rae_indices.csv")








