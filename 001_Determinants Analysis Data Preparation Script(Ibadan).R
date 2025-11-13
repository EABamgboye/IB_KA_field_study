#file.path(gsub("[\\]", "/", gsub("Documents", "", Sys.getenv("HOME"))))
LuDataDir <- file.path("C:/Users/ebamgboye/Urban Malaria Proj Dropbox/urban_malaria/data/nigeria/kano_ibadan_epi")
DellDataDir <- file.path("C:/Users/DELL/Urban Malaria Proj Dropbox/urban_malaria/data/nigeria/kano_ibadan_epi")

# Load necessary libraries
library(haven)
library(dplyr)
library(forcats)
library(haven)
library(tidyr)
library(FactoMineR)  # For PCA
library(factoextra)  # Optional: for PCA visualization
library(missMDA) #For imputing missing values


##My console
ibhhdry_data <- read_dta(file.path(LuDataDir, "Combined Working Data", "Ibadan", "Dry Season Data", "Long Data", "Ibadan_long_dry_season_household_members_records.dta"))

ibhhwet_data <- read_dta(file.path(LuDataDir, "Combined Working Data", "Ibadan", "Wet Season Data", "Long Data", "ibadan_long_wetseason_household_members_with_ind_nets.dta"))


##My Dell Laptop
ibhhdry_data <- read_dta(file.path(DellDataDir, "Combined Working Data", "Ibadan", "Dry Season Data", "Long Data" , "Ibadan_long_dry_season_household_members_records.dta"))

ibhhwet_data <- read_dta(file.path(DellDataDir, "Combined Working Data", "Ibadan", "Wet Season Data", "Long Data", "ibadan_long_wetseason_household_members_with_ind_nets.dta"))

##Checking name mismatches
# names_dry <- c(
#   "sn", "redcap_repeat_instrument_x", "redcap_repeat_instance_x", "bi1", "bi2",
#   "bi3", "bi4", "bi5", "bi6", "bi7_long", "bi7_lat", "bi8", "bi9", "bi10", "bi11", 
#   "bi12", "bi12i", "bi13", "bi13i", "bi14", "bi14i", "bi15", "bi16", 
#   "background_informati_v_0", "q100", "q100i", "q101", "q101i", "q102", "q102i", 
#   "q103", "q103i", "q104_1", "q104_2", "q104_3", "q104_4", "q104_5", "q104_6", 
#   "q104_7", "q104_8", "q104_9", "q104_10", "q104_11", "q104_12", "q104_13", 
#   "q104_14", "q104_15", "q105", "q106", "q107", "q107i", "q108", "q109", "q110", 
#   "q111", "q112", "q112i", "q113", "q114", "q115", "q116", 
#   "section_1_household_v_1", "c1", "cli", "tfh_complete", "q123", "q124", "q124i", 
#   "q125", "q126", "q126i", "q127", "citizen_science_complete", "q201", "q202", 
#   "q203", "q204", "q205", "q206", "q207", "q208", "q209", "q210", "q211", 
#   "household_and_neighb_v_2", "nh101a", "nh101b", "nh102_1", "nh102_2", "nh102_3", 
#   "nh102_4", "nh102_5", "nh102_6", "nh102_7", "nh102_8", "nh102_9", "nh102_10", 
#   "nh102_11", "nh102_12", "nh102i", "nh103", "nh104", "nh105", "nh105i", "nh106", 
#   "nh107", "nh107a", "net_ownership_and_ch_v_3", "ea", "ea_1", "ea_2", 
#   "redcap_repeat_instrument_y", "redcap_repeat_instance_y", "hl1", "hl2", "hl3", 
#   "hl4", "hl5", "hl6", "hl7", "hl7i", "hl8", "hl9", "hl10", "household_list_complete", 
#   "newdbs_code", "q300", "q301", "q302", "q303", "q304", "ward", "ea_cluster", 
#   "line_number00", "age_calc", "unique_id", "agebin", "ward_total", "ea_total", 
#   "hh_total", "age_total", "ward_2", "enumeration_area_x", "cluster_number_x", 
#   "hh_serial_no_in_ea_y", "hh_serial_no_in_structure_y", "ward_weight", 
#   "ea_settlement_weight", "hhs_weights", "ind_total", "prob_ind_hh", 
#   "ind_weights_hh", "overall_hh_weight"
# )
# 
# 
# names_wet <- c(
#     "sn", "redcap_repeat_instrument", "redcap_repeat_instance", "hl1", "hl2", "hl3",
#     "hl4", "hl5", "hl6", "hl7", "hl8", "hl9", "household_list_complete", 
#     "newdbs_code", "q300", "q300i", "q301", "q302", "q303", "ln", "rdt", "dob_w", 
#     "hl6_c", "rdt_m", "dob_m", "_merge", "ward", "enumaration_area", 
#     "settlement_type_new", "prob_selected_ward", "prob_selected_eas_settlement", 
#     "prob_selected_hh_structure", "ward_weight", "ea_settlement_weight", 
#     "hhs_weights", "hh_number", "tot", "overall_total", "ages", "ward_total", 
#     "ea_total", "hh_total", "agebin", "age_total", "ind_total", "prob_ind_hh", 
#     "ind_weights_hh", "overall_hh_weight", "bi1", "name_lga", "bi2", "bi3_1", 
#     "bi3_2", "bi3_3", "settlement", "bi4", "bi5", "bi6", "bi7_long", "bi7_lat", 
#     "bi8", "bi9", "bi10", "bi11", "bi12", "bi12i", "bi13", "bi13i", "bi14", "bi14i", 
#     "bi15", "bi16", "background_informati_v_0", "q100", "q100i", "q101", "q101i", 
#     "q102", "q102i", "q103", "q103i", "q104_1", "q104_2", "q104_3", "q104_4", 
#     "q104_5", "q104_6", "q104_7", "q104_8", "q104_9", "q104_10", "q104_11", 
#     "q104_12", "q104_13", "q104_14", "q104_15", "q105", "q106", "q107", "q107i", 
#     "q108", "q109", "q110", "q111", "q112", "q112i", "q113", "q114", "q115", "q116", 
#     "section_1_household_v_1", "c1", "cli", "tfh_complete", "q123", "q124", "q124i", 
#     "q125", "q126", "q126i", "q127", "citizen_science_complete", "q201", "q202", 
#     "q203", "q204", "q205", "q206", "q207", "q208", "q209", "q210", "q211", 
#     "household_and_neighb_v_2", "nh101a", "nh101b", "nh102_1", "nh102_2", "nh102_3", 
#     "nh102_4", "nh102_5", "nh102_6", "nh102_7", "nh102_8", "nh102_9", "nh102_10", 
#     "nh102_11", "nh102_12", "nh102i", "nh103", "nh104", "nh105", "nh105i", "nh106", 
#     "nh107", "nh107a", "net_ownership_and_ch_v_3", "lga", "LGA", "ward_r", 
#     "ea", "ea_1", "ea_2"
#   )
# 


# ##Remove extra __ in dataset
# ibhhwet_data <- ibhhwet_data %>%
#   rename_with(~ gsub("___", "_", .x)) %>%   # Replace triple underscores
#   rename_with(~ gsub("__", "_", .x))        # Replace double underscores
# 
# 
# # Compare the two datasets' variable names
# common_vars <- intersect(names_dry, names_wet)   # Variables present in both
# only_in_names_dry <- setdiff(names_dry, names_wet)   # Variables only in vars1
# only_in_names_wet <- setdiff(names_wet, names_dry)   # Variables only in vars2
# 

##Remove extra __ in dataset to facilitate merging
ibhhwet_data <- ibhhwet_data %>%
  rename_with(~ gsub("___", "_", .x)) %>%   # Replace triple underscores
  rename_with(~ gsub("__", "_", .x))        # Replace double underscores

##Rename variables to facilitate merge
ibhhdry_data <- ibhhdry_data %>%
  rename(settlement = bi3)


#Extract data for household analysis
ibhh_drydf <- ibhhdry_data %>% 
  dplyr::select ("sn", "bi1", "bi2", "settlement", "bi4", "bi5", "bi6", "bi7_long", "bi7_lat", "bi8", "bi9",
    "bi10", "bi11", "bi12", "bi12i", "bi13", "bi13i", "bi14", "bi14i", "bi15", "bi16",
    "background_informati_v_0", "q100", "q100i", "q101", "q101i", "q102", "q102i",
    "q103", "q103i", "q104_1", "q104_2", "q104_3", "q104_4", "q104_5", "q104_6",
    "q104_7", "q104_8", "q104_9", "q104_10", "q104_11", "q104_12", "q104_13",
    "q104_14", "q104_15", "q105", "q106", "q107", "q107i", "q108", "q109", "q110",
    "q111", "q112", "q112i", "q113", "q114", "q115", "q116", "section_1_household_v_1",
    "c1", "cli", "tfh_complete", "q123", "q124", "q124i", "q125", "q126", "q126i",
    "q127", "citizen_science_complete", "q201", "q202", "q203", "q204", "q205",
    "q206", "q207", "q208", "q209", "q210", "q211", "household_and_neighb_v_2",
    "nh101a", "nh101b", "nh102_1", "nh102_2", "nh102_3", "nh102_4", "nh102_5",
    "nh102_6", "nh102_7", "nh102_8", "nh102_9", "nh102_10", "nh102_11", "nh102_12",
    "nh102i", "nh103", "nh104", "nh105", "nh105i", "nh106", "nh107", "nh107a",
    "net_ownership_and_ch_v_3", "ea", "ea_1", "ea_2", "hl1", "hl2", "hl3", "hl4",
    "hl5", "hl6", "hl7", "hl8", "hl9", "household_list_complete", "newdbs_code",
    "q300", "q301", "q302", "q303", "ward", "agebin", "ward_total", "ea_total",
    "hh_total", "age_total", "ward_weight", "ea_settlement_weight", "hhs_weights",
    "ind_total", "prob_ind_hh", "ind_weights_hh", "overall_hh_weight"
  )

ibhh_wetdf <- ibhhwet_data %>% 
  dplyr::select ("sn", "bi1", "bi2", "settlement", "bi4", "bi5", "bi6", "bi7_long", "bi7_lat", "bi8", "bi9",
                 "bi10", "bi11", "bi12", "bi12i", "bi13", "bi13i", "bi14", "bi14i", "bi15", "bi16",
                 "background_informati_v_0", "q100", "q100i", "q101", "q101i", "q102", "q102i",
                 "q103", "q103i", "q104_1", "q104_2", "q104_3", "q104_4", "q104_5", "q104_6",
                 "q104_7", "q104_8", "q104_9", "q104_10", "q104_11", "q104_12", "q104_13",
                 "q104_14", "q104_15", "q105", "q106", "q107", "q107i", "q108", "q109", "q110",
                 "q111", "q112", "q112i", "q113", "q114", "q115", "q116", "section_1_household_v_1",
                 "c1", "cli", "tfh_complete", "q123", "q124", "q124i", "q125", "q126", "q126i",
                 "q127", "citizen_science_complete", "q201", "q202", "q203", "q204", "q205",
                 "q206", "q207", "q208", "q209", "q210", "q211", "household_and_neighb_v_2",
                 "nh101a", "nh101b", "nh102_1", "nh102_2", "nh102_3", "nh102_4", "nh102_5",
                 "nh102_6", "nh102_7", "nh102_8", "nh102_9", "nh102_10", "nh102_11", "nh102_12",
                 "nh102i", "nh103", "nh104", "nh105", "nh105i", "nh106", "nh107", "nh107a",
                 "net_ownership_and_ch_v_3", "ea", "ea_1", "ea_2", "hl1", "hl2", "hl3", "hl4",
                 "hl5", "hl6", "hl7", "hl8", "hl9", "household_list_complete", "newdbs_code",
                 "q300", "q301", "q302", "q303", "ward", "agebin", "ward_total", "ea_total",
                 "hh_total", "age_total", "ward_weight", "ea_settlement_weight", "hhs_weights",
                 "ind_total", "prob_ind_hh", "ind_weights_hh", "overall_hh_weight"
  )

##Add season to the dataframes
ibhh_drydf$season <- "Dry"
ibhh_wetdf$season <- "Wet"

##Ensure column matches before merging 

# Check which columns differ in type
common_cols <- intersect(names(ibhh_drydf), names(ibhh_wetdf))

type_check <- data.frame(
  column = common_cols,
  dry_type = sapply(ibhh_drydf[common_cols], class),
  wet_type = sapply(ibhh_wetdf[common_cols], class),
  stringsAsFactors = FALSE
)


# # View columns with mismatched types
# type_check[type_check$dry_type != type_check$wet_type, ]

for (col in common_cols) {
  if (class(ibhh_drydf[[col]])[1] != class(ibhh_wetdf[[col]])[1]) {
    ibhh_drydf[[col]] <- as.character(ibhh_drydf[[col]])
    ibhh_wetdf[[col]] <- as.character(ibhh_wetdf[[col]])
  }
}
#Combine files
ibhhdata_combined <- rbind(ibhh_drydf, ibhh_wetdf)

# Write data frame to a .dta file(Dell Laptop)
write_dta(ibhhdata_combined, file.path(DellDataDir, "ibhhdata_combined.dta"))

# Write data frame to a .dta file(My console)
write_dta(ibhhdata_combined, file.path(LuDataDir, "ibhhdata_combined.dta"))

##Read in back data
ibhhdata_combined <- read_dta(file.path(LuDataDir, "ibhhdata_combined.dta"))

##Read in Net Ownership data
ibnet_dry <- read_dta("C:/Users/ebamgboye/Urban Malaria Proj Dropbox/urban_malaria/data/nigeria/kano_ibadan_epi/new_field_data/Ibadan Dry Season data_latest_Nov24/Ibadan Dry season survey data/IB dry season net inspectn.dta")
ibnet_wet <- read_dta("C:/Users/ebamgboye/Urban Malaria Proj Dropbox/urban_malaria/data/nigeria/kano_ibadan_epi/new_field_data/last_upload_Akinyemi/IB wet household qnaire/IB Wet season hhold net inspection.dta")

#Merge net ownership data
ibnet_all <- rbind(ibnet_dry, ibnet_wet)

##Add net use to main data frame
##Ensure only one household per net use
ibnet_all_uniquehh <- ibnet_all  %>%
  group_by(sn) %>%
  slice(1) %>%
  ungroup()

ibhhdata_combined <- ibhhdata_combined %>%
  left_join(
    ibnet_all_uniquehh,
    by = c("sn")
  )


##Convert all labels for ease of analysis
ibhhdata_combined <- ibhhdata_combined %>%
  mutate(across(where(is.labelled), as_factor))


##Some data wrangling
##Recode age group based on field study sample collection categories
ibhhdata_combined <- ibhhdata_combined %>%
  mutate(age_group = case_when(
    hl5 >= 0  & hl5 <= 5  ~ "0–5",
    hl5 >= 6  & hl5 <= 10 ~ "6–10",
    hl5 >= 11 & hl5 <= 17 ~ "11–17",
    hl5 >= 18 & hl5 <= 30 ~ "18–30",
    hl5 > 30              ~ "30+",
    TRUE                  ~ NA_character_
  ))

##Collapse Age group Into 3 categories
ibhhdata_combined <- ibhhdata_combined %>%
  mutate(age_group3 = case_when(
    age_group == "0–5" ~ "0–5",
    age_group %in% c("6–10", "11–17") ~ "6–17",
    age_group %in% c("18–30", "30+") ~ "18+",
    TRUE ~ NA_character_
  ))

#Reorder age_group
ibhhdata_combined <- ibhhdata_combined %>%
  mutate(age_group3 = factor(
    age_group3,
    levels = c("0–5", "6–17", "18+"),
    ordered = TRUE
  ))


###Set up data for wealth quintile computation
##Recode variables as numeric
# List all wealth-related variables to convert
wealth_vars_to_convert <- c("q100", "q101", "q102", "q103", "q105", "q106",
                            "q114", "q115")  

# Convert them all to numeric
ibhhdata_combined <- ibhhdata_combined %>%
  mutate(across(all_of(wealth_vars_to_convert), ~ as.integer(.), .names = "{.col}_num"))

# Check result
head(ibhhdata_combined[, paste0(wealth_vars_to_convert, "_num")])

##Recode variables into two categories

# Recode q100 (water source)
ibhhdata_combined$water_drink <- dplyr::case_when(
  ibhhdata_combined$q100_num %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9) ~ "Improved",
  ibhhdata_combined$q100_num %in% c(10, 11, 12, 13, 14) ~ "Unimproved",
  TRUE ~ NA_character_
)

# Recode q101 (water use)
ibhhdata_combined$water_use <- dplyr::case_when(
  ibhhdata_combined$q101_num %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9) ~ "Improved",
  ibhhdata_combined$q101_num %in% c(10, 11, 12, 13, 14) ~ "Unimproved",
  TRUE ~ NA_character_
)

# Recode q102 (power source)
ibhhdata_combined$power <- dplyr::case_when(
  ibhhdata_combined$q102_num %in% c(1, 2, 3) ~ "Improved",
  ibhhdata_combined$q102_num %in% c(4, 5, 6, 7, 8) ~ "Unimproved",
  TRUE ~ NA_character_
)

# Recode q103 (sanitation facility)
ibhhdata_combined$toilet <- dplyr::case_when(
  ibhhdata_combined$q103_num %in% c(1, 2, 3, 4) ~ "Improved",
  ibhhdata_combined$q103_num %in% c(5, 6, 7, 8) ~ "Unimproved",
  TRUE ~ NA_character_
)

# Recode q105 (livestock)
ibhhdata_combined$livestock <- dplyr::case_when(
  ibhhdata_combined$q105_num %in% c(1) ~ "Improved",
  ibhhdata_combined$q105_num %in% c(2) ~ "Unimproved",
  TRUE ~ NA_character_
)

# Recode q106 (house ownership)
ibhhdata_combined$house <- dplyr::case_when(
  ibhhdata_combined$q106_num %in% c(1) ~ "Improved",
  ibhhdata_combined$q106_num %in% c(2, 3, 4) ~ "Unimproved",
  TRUE ~ NA_character_
)

# Recode q114 (toilet use)
ibhhdata_combined$toilet_use <- dplyr::case_when(
  ibhhdata_combined$q114_num %in% c(1) ~ "Improved",
  ibhhdata_combined$q114_num %in% c(2) ~ "Unimproved",
  TRUE ~ NA_character_
)

# Recode q115 (bathroom location)
ibhhdata_combined$bathroom <- dplyr::case_when(
  ibhhdata_combined$q115_num %in% c(1) ~ "Improved",
  ibhhdata_combined$q115_num %in% c(2,3) ~ "Unimproved",
  TRUE ~ NA_character_
)


##Recode labels from Improved and Unimproved to 1 and 0 aid PCA
ibhhdata_combined <- ibhhdata_combined %>%
  mutate(
    water_drink = ifelse(water_drink == "Improved", 1, 0),
    water_use = ifelse(water_use == "Improved", 1, 0),
    power = ifelse(power == "Improved", 1, 0),
    toilet = ifelse(toilet == "Improved", 1, 0),
    livestock = ifelse(livestock == "Improved", 1, 0),
    house = ifelse(house == "Improved", 1, 0),
    toilet_use = ifelse(toilet_use == "Improved", 1, 0),
    bathroom = ifelse(bathroom == "Improved", 1, 0)
  )

##Compute wealth quintile
# List of q104 variables(household items owned)
q104_vars <- paste0("q104_", 1:15)

# Convert factors of q104 ("1. Checked" / "0. Unchecked" to numeric 1/0)
ibhhdata_combined <- ibhhdata_combined %>%
  mutate(across(all_of(q104_vars), 
                ~ case_when(
                  . %in% c("1. Checked", "1. Ch…") ~ 1,
                  . %in% c("0. Unchecked", "0. Un…") ~ 0,
                  TRUE ~ NA_real_
                )
  ))

# Check result
head(ibhhdata_combined[, q104_vars])


# Select only the variables for wealth index
wealth_vars <- ibhhdata_combined %>%
  select(water_drink, water_use, power, toilet, q104_1, q104_2, q104_3, q104_4, 
         q104_5, q104_6, q104_7, q104_8, q104_9, q104_10, q104_11, q104_12, q104_13, 
         q104_14, q104_15, livestock, house, toilet_use, bathroom)

# Convert categorical variables to numeric(confirm)
wealth_vars_all <- wealth_vars %>%
  mutate(across(where(haven::is.labelled), as.numeric))

##Replace NAs with column means
wealth_vars_all <- wealth_vars_all %>%
  mutate(across(everything(), ~ifelse(is.na(.), mean(., na.rm = TRUE), .)))

##Convert from tibble to matrix 
wealth_matrix <- as.matrix(wealth_vars_all)


# Perform PCA analysis
pca_res <- PCA(wealth_vars_all, scale.unit = TRUE, ncp = 5, graph = FALSE)

# Extract the first principal component as the wealth index
ibhhdata_combined$wealth_index <- pca_res$ind$coord[,1]

# Check distribution
range(ibhhdata_combined$wealth_index)

# Create wealth quintiles(5 categories)
# ibhhdata_combined <- ibhhdata_combined %>%
#   mutate(wealth_quintile = ntile(wealth_index, 5))  # 1 = poorest, 5 = richest


##Retry grouping using 3 groups
# Compute quartiles
q1 <- quantile(ibhhdata_combined$wealth_index, 0.25, na.rm = TRUE)
q3 <- quantile(ibhhdata_combined$wealth_index, 0.75, na.rm = TRUE)
iqr <- q3 - q1

# Classify based on IQR
ibhhdata_combined <- ibhhdata_combined %>%
  mutate(
    wealth_iqr_group = case_when(
      wealth_index <= q1 ~ "Low",
      wealth_index > q1 & wealth_index < q3 ~ "Middle",
      wealth_index >= q3 ~ "High"
    )
  )

# Order factor levels
ibhhdata_combined$wealth_iqr_group <- factor(
  ibhhdata_combined$wealth_iqr_group,
  levels = c("Low", "Middle", "High")
)

##Save new data with wealth quintile
##Convert variable names for ease of saving
ibhhdata_combined <- ibhhdata_combined %>%
  rename_with(~ gsub("\\.", "_", .x))

# Write data frame to .dta file(My console)
write_dta(ibhhdata_combined, file.path(LuDataDir, "ibhhdata_combined.dta"))





