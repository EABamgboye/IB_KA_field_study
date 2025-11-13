
#file.path(gsub("[\\]", "/", gsub("Documents", "", Sys.getenv("HOME"))))
LuDataDir <- file.path("C:/Users/ebamgboye/Urban Malaria Proj Dropbox/urban_malaria/data/nigeria/kano_ibadan_epi")
DellDataDir <- file.path("C:/Users/DELL/Urban Malaria Proj Dropbox/urban_malaria/data/nigeria/kano_ibadan_epi")

# Load necessary libraries
library(haven)
library(dplyr)
library(forcats)
library(haven)
library(ggcorrplot)
library(FactoMineR)  # For PCA
library(factoextra)  # Optional: for PCA visualization
library(missMDA) #For imputing missing values

##My console
knhhdry_data <- read_dta(file.path(LuDataDir, "Combined Working Data", "Kano", "Dry Season Data", "Long Data", "kano_dryseason_long_data.dta"))

knhhwet_data <- read_dta(file.path(LuDataDir, "Combined Working Data", "Kano", "Wet Season Data", "Long Data", "kano_wetseason_long_data.dta"))

##My Dell Laptop
knhhdry_data <- read_dta(file.path(DellDataDir, "Combined Working Data", "Kano", "Dry Season Data", "Long Data", "kano_dryseason_long_data.dta"))

knhhwet_data <- read_dta(file.path(DellDataDir, "Combined Working Data", "Kano", "Wet Season Data", "Long Data", "kano_wetseason_long_data.dta"))

# names_knwet <- c("sn", "dry_sn", "redcap_repeat_instrument_x", "redcap_repeat_instance_x",
# "bi1", "lga", "bi2", "bi2ii", "bi3", "bi4", "bi5", "bi6", "bi7_long", "bi7_lat",
# "bi8", "bi9", "bi10", "bi11", "bi12", "bi12i", "bi13", "bi13i", "bi14", "bi14i",
# "bi15", "bi16", "background_informati_v_0", "q100", "q100i", "q101", "q101i",
# "q102", "q102i", "q103", "q103i", "q104___1", "q104___2", "q104___3", "q104___4",
# "q104___5", "q104___6", "q104___7", "q104___8", "q104___9", "q104___10",
# "q104___11", "q104___12", "q104___13", "q104___14", "q104___15", "q105", "q106",
# "q107", "q107i", "q108", "q109", "q110", "q111", "q112", "q112i", "q113", "q114",
# "q115", "q116", "section_1_household__v_1", "c1", "cli", "tfh_complete", "q123",
# "q124", "q124i", "q125", "q126", "q126i", "q127", "citizen_science_complete",
# "q201", "q202", "q203", "q204", "q205", "q206", "q207", "q208", "q209", "q210",
# "q211", "household_and_neighb_v_2", "nh101a", "nh101b", "nh102___1", "nh102___2",
# "nh102___3", "nh102___4", "nh102___5", "nh102___6", "nh102___7", "nh102___8",
# "nh102___9", "nh102___10", "nh102___11", "nh102___12", "nh102i", "nh103", "nh104",
# "nh105", "nh105i", "nh106", "nh107", "nh107a", "net_ownership_and_ch_v_3", "ward_x",
# "ea", "ea_1", "ea_2", "settle_type", "wardn", "old_ea", "ea_name_new", "ea_cluster",
# "ward_y", "settlement1", "redcap_repeat_instrument_y", "redcap_repeat_instance_y",
# "hl1", "hl2", "hl3", "hl4", "hl5", "hl6", "hl7", "hl8", "hl9", "household_list_complete",
# "q300", "q301", "q302", "q303", "q304", "age_calc", "line_number00", "unique_id",
# "agebin", "ward_total", "ea_total", "hh_total", "age_total", "ward", "enumeration_area",
# "ea_serial_number", "hh_serial_number", "structure_serial_number", "ward_weight",
# "ea_settlement_weight", "hhs_weights", "ind_total", "prob_ind_hh", "ind_weights_hh",
# "overall_hh_weight", "longitude", "latitude"
# )
# 
# 
# # 
# 
 # names_kndry <- c("sn", "bi1", "bi2", "bi2ii", "bi3", "bi4", "bi5", "bi6", "bi7_long", 
 #                "bi7_lat", "bi8", "bi9", "bi10", "bi11", "bi12", "bi12i", "bi13", "bi13i",
 #                "bi14", "bi14i", "bi15", "bi16", "background_informati_v_0", "q100", "q100i", 
 #                "q101", "q101i", "q102", "q102i", "q103", "q103i", "q104_1", "q104_2", 
 #                "q104_3", "q104_4", "q104_5", "q104_6", "q104_7", "q104_8", "q104_9", 
 #                "q104_10", "q104_11", "q104_12", "q104_13", "q104_14", "q104_15", "q105", 
 #                "q106", "q107", "q107i", "q108", "q109", "q110", "q111", "q112", "q112i", 
 #                "q113", "q114", "q115", "q116", "section_1_household_v_1", "c1", "cli", 
 #                "tfh_complete", "q123", "q124", "q124i", "q125", "q126", "q126i", "q127", 
 #                "citizen_science_complete", "q201", "q202", "q203", "q204", "q205", "q206", 
 #                "q207", "q208", "q209", "q210", "q211", "household_and_neighb_v_2", "nh101a", 
 #                "nh101b", "nh102_1", "nh102_2", "nh102_3", "nh102_4", "nh102_5", "nh102_6", 
 #                "nh102_7", "nh102_8", "nh102_9", "nh102_10", "nh102_11", "nh102_12", "nh102i", 
 #                "nh103", "nh104", "nh105", "nh105i", "nh106", "nh107", "nh107a", 
 #                "net_ownership_and_ch_v_3", "ea", "ea_1", "ea_2", "ea_3", "ea_4", "ward", 
 #                "old_ea", "ea_name_new", "ea_cluster", "redcap_repeat_instrument", 
 #                "redcap_repeat_instance", "hl1", "hl2", "hl3", "hl4", "hl5", "hl6", "hl7", 
 #                "hl8", "hl9", "household_list_complete", "q300", "q301", "q302", "q303", "q304", 
 #                "q305", "new_dbscode", "line_number00", "age_calc", "unique_id", "agebin", 
 #                "ward_total", "ea_total", "hh_total", "age_total", "latitude_n", "ward_2", 
 #                "enumeration_area", "ea_serial_number", "hh_serial_number", "structure_serial_number", 
 #                "ward_weight", "ea_settlement_weight", "hhs_weights", "ind_total", 
 #                "prob_ind_hh", "ind_weights_hh", "overall_hh_weight", "longitude")
 # 
 # 


# ##Remove extra __ in dataset
# knhhwet_data <- knhhwet_data %>%
#   rename_with(~ gsub("___", "_", .x)) %>%   # Replace triple underscores
#   rename_with(~ gsub("__", "_", .x))        # Replace double underscores
# 
# 
# # Compare the two datasets' variable names
#  common_varskn <- intersect(names_kndry, names_knwet)   # Variables present in both
#  only_in_names_kndry <- setdiff(names_kndry, names_knwet)   # Variables only in vars1
#  only_in_names_knwet <- setdiff(names_knwet, names_kndry)   # Variables only in vars2
# # 
#  # Print summary
#  cat("✅ Common variables:", length(common_varskn), "\n")
#  cat("❌ Only in vars1:", length(only_in_names_kndry), "\n")
#  cat("❌ Only in vars2:", length(only_in_names_knwet), "\n")
# # 
# 
#  # Clean and make names unique in one go
#  names(knhhwet_data) <- names(knhhwet_data) %>%
#    gsub("___", "_", .) %>%
#    gsub("__", "_", .) %>%
#    make.unique()
#  


#Extract data for household analysis
knhh_drydf <- knhhdry_data %>% 
  dplyr::select ("sn", "bi1", "bi2", "bi3", "bi4", "bi5", "bi6", "bi7_long", "bi7_lat", "bi8", "bi9",
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
                 "hl5", "hl6", "hl7", "hl8", "hl9", "household_list_complete", "q300", "q301", "q302", "q303", "ward", "agebin", "ward_total", "ea_total",
                 "hh_total", "age_total", "ward_weight", "ea_settlement_weight", "hhs_weights",
                 "ind_total", "prob_ind_hh", "ind_weights_hh", "overall_hh_weight"
  )

knhh_wetdf <- knhhwet_data %>% 
  dplyr::select ("sn", "bi1", "bi2", "bi3", "bi4", "bi5", "bi6", "bi7_long", "bi7_lat", "bi8", "bi9",
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
                 "hl5", "hl6", "hl7", "hl8", "hl9", "household_list_complete", "q300", "q301", "q302", "q303", "ward", "agebin", "ward_total", "ea_total",
                 "hh_total", "age_total", "ward_weight", "ea_settlement_weight", "hhs_weights",
                 "ind_total", "prob_ind_hh", "ind_weights_hh", "overall_hh_weight"
  )

##Add season to dataframes
knhh_drydf$season <- "Dry"
knhh_wetdf$season <- "Wet"

##Ensure column matches before merging 

# Convert all haven_labelled columns to character
knhh_drydf <- knhh_drydf %>% mutate(across(where(is.labelled), as.character))
knhh_wetdf <- knhh_wetdf %>% mutate(across(where(is.labelled), as.character))

# Also, ensure all factor columns are converted to character
knhh_drydf <- knhh_drydf %>% mutate(across(where(is.factor), as.character))
knhh_wetdf <- knhh_wetdf %>% mutate(across(where(is.factor), as.character))

# Align classes again
for (col in intersect(names(knhh_drydf), names(knhh_wetdf))) {
  if (class(knhh_drydf[[col]])[1] != class(knhh_wetdf[[col]])[1]) {
    knhh_drydf[[col]] <- as.character(knhh_drydf[[col]])
    knhh_wetdf[[col]] <- as.character(knhh_wetdf[[col]])
  }
}

#Combine files
knhhdata_combined <- rbind(knhh_drydf, knhh_wetdf)

# Write data frame to a .dta file(Dell Laptop)
write_dta(knhhdata_combined, file.path(DellDataDir, "knhhdata_combined.dta"))

# Write data frame to a .dta file(My console)
write_dta(knhhdata_combined, file.path(DellDataDir, "knhhdata_combined.dta"))

##Read in back data
knhhdata_combined <- read_dta(file.path(LuDataDir, "knhhdata_combined.dta"))

##Read in net use data
##Net Ownership
knnet_dry <- read_dta("C:/Users/ebamgboye/Urban Malaria Proj Dropbox/urban_malaria/data/nigeria/kano_ibadan_epi/new_field_data/Kano latest Dry Season Corrected Data_Jan 2025/KN dry hhold net inspectn.dta")
knnet_wet <- read_dta("C:/Users/ebamgboye/Urban Malaria Proj Dropbox/urban_malaria/data/nigeria/kano_ibadan_epi/new_field_data/Kano Wet Season Data Sept. 2024/5. KN Wet season household net inspection.dta")

knnet_all <- rbind(knnet_dry, knnet_wet)

##Add net use to main data frame
##Ensure only one household per net use
knnet_all_uniquehh <- knnet_all  %>%
  group_by(sn) %>%
  slice(1) %>%
  ungroup()

knhhdata_combined <- knhhdata_combined %>%
  mutate(
    sn = as.character(sn)          # convert sn to character
  ) %>% 
  left_join(
    knnet_all_uniquehh,
    by = c("sn")
  )


##Some data wrangling
##Recode age group based on field study sample collection categories
knhhdata_combined <- knhhdata_combined %>%
  mutate(age_group = case_when(
    hl5 >= 0  & hl5 <= 5  ~ "0–5",
    hl5 >= 6  & hl5 <= 10 ~ "6–10",
    hl5 >= 11 & hl5 <= 17 ~ "11–17",
    hl5 >= 18 & hl5 <= 30 ~ "18–30",
    hl5 > 30              ~ "30+",
    TRUE                  ~ NA_character_
  ))

##Collapse Age group Into 3 categories
knhhdata_combined <- knhhdata_combined %>%
  mutate(age_group3 = case_when(
    age_group == "0–5" ~ "0–5",
    age_group %in% c("6–10", "11–17") ~ "6–17",
    age_group %in% c("18–30", "30+") ~ "18+",
    TRUE ~ NA_character_
  ))

#Reorder age_group
knhhdata_combined <- knhhdata_combined %>%
  mutate(age_group3 = factor(
    age_group3,
    levels = c("0–5", "6–17", "18+"),
    ordered = TRUE
  ))


##Generate new settlement in two categories for ease of analysis
knhhdata_combined <- knhhdata_combined %>%
  # Recode bi3 to meaningful settlement labels
  mutate(settlement = fct_recode(as_factor(bi3),
                                 "Formal" = "1",
                                 "Informal" = "2",
                                 "Slum" = "3")) %>%
  
  # Collapse Informal + Slum into single "Informal" category
  mutate(settlement = case_when(
    settlement == "Formal" ~ "Formal",
    settlement %in% c("Informal", "Slum") ~ "Informal",
    TRUE ~ NA_character_
  )) %>%
  
  
  # Convert back to factor with two levels
  mutate(settlement = factor(settlement, levels = c("Formal", "Informal")))



###Set up wealth qunitile Variable
##Recode variables as need be
# Recode q100 (water source)
knhhdata_combined$water_drink <- dplyr::case_when(
  knhhdata_combined$q100 %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9) ~ "Improved",
  knhhdata_combined$q100 %in% c(10, 11, 12, 13, 14) ~ "Unimproved",
  TRUE ~ NA_character_
)

# Recode q101 (water use)
knhhdata_combined$water_use <- dplyr::case_when(
  knhhdata_combined$q101 %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9) ~ "Improved",
  knhhdata_combined$q101 %in% c(10, 11, 12, 13, 14) ~ "Unimproved",
  TRUE ~ NA_character_
)

# Recode q102 (power source)
knhhdata_combined$power <- dplyr::case_when(
  knhhdata_combined$q102 %in% c(1, 2, 3) ~ "Improved",
  knhhdata_combined$q102 %in% c(4, 5, 6, 7, 8) ~ "Unimproved",
  TRUE ~ NA_character_
)

# Recode q103 (sanitation facility)
knhhdata_combined$toilet <- dplyr::case_when(
  knhhdata_combined$q103 %in% c(1, 2, 3, 4) ~ "Improved",
  knhhdata_combined$q103 %in% c(5, 6, 7, 8) ~ "Unimproved",
  TRUE ~ NA_character_
)

# Recode q105 (livestock)
knhhdata_combined$livestock <- dplyr::case_when(
  knhhdata_combined$q105 %in% c(1) ~ "Improved",
  knhhdata_combined$q105 %in% c(2) ~ "Unimproved",
  TRUE ~ NA_character_
)

# Recode q106 (house ownership)
knhhdata_combined$house <- dplyr::case_when(
  knhhdata_combined$q106 %in% c(1) ~ "Improved",
  knhhdata_combined$q106 %in% c(2, 3, 4) ~ "Unimproved",
  TRUE ~ NA_character_
)

# Recode q114 (toilet use)
knhhdata_combined$toilet_use <- dplyr::case_when(
  knhhdata_combined$q114 %in% c(1) ~ "Improved",
  knhhdata_combined$q114 %in% c(2) ~ "Unimproved",
  TRUE ~ NA_character_
)

# Recode q115 (bathroom location)
knhhdata_combined$bathroom <- dplyr::case_when(
  knhhdata_combined$q115 %in% c(1) ~ "Improved",
  knhhdata_combined$q115 %in% c(2,3) ~ "Unimproved",
  TRUE ~ NA_character_
)


##Recode labels for Improved and Unimproved to 1 and 0 to aid PCA
knhhdata_combined <- knhhdata_combined %>%
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

# Select only the variables for wealth index
kn_wealth_vars <- knhhdata_combined %>%
  select(water_drink, water_use, power, toilet, q104_1, q104_2, q104_3, q104_4, 
         q104_5, q104_6, q104_7, q104_8, q104_9, q104_10, q104_11, q104_12, q104_13, 
         q104_14, q104_15, livestock, house, toilet_use, bathroom)

# Convert categorical variables to numeric 
kn_wealth_vars_all <- kn_wealth_vars %>%
  mutate(
    across(where(haven::is.labelled), as.numeric),      # labelled → numeric
    across(where(is.character), ~ as.numeric(.))       # character → numeric
  )


##Replace NAs with column means
kn_wealth_vars_all <- kn_wealth_vars_all %>%
  mutate(across(everything(), ~ifelse(is.na(.), mean(., na.rm = TRUE), .)))
##Convert from tibble to matrix 
kn_wealth_matrix <- as.matrix(kn_wealth_vars_all)


# Perform PCA analysis
kn_pca_res <- PCA(kn_wealth_vars_all, scale.unit = TRUE, ncp = 5, graph = FALSE)

# Extract the first principal component as the wealth index
knhhdata_combined$kn_wealth_index <- kn_pca_res$ind$coord[,1]

# Check distribution
range(knhhdata_combined$kn_wealth_index)


##Group into 3
# Compute quartiles
knq1 <- quantile(knhhdata_combined$kn_wealth_index, 0.25, na.rm = TRUE)
knq3 <- quantile(knhhdata_combined$kn_wealth_index, 0.75, na.rm = TRUE)
kniqr <- knq3 - knq1

# Classify based on IQR
knhhdata_combined <- knhhdata_combined %>%
  mutate(
    wealth_iqr_group = case_when(
      kn_wealth_index <= knq1 ~ "Low",
      kn_wealth_index > knq1 & kn_wealth_index < knq3 ~ "Middle",
      kn_wealth_index >= knq3 ~ "High"
    )
  )

# Order factor levels
knhhdata_combined$wealth_iqr_group <- factor(
  knhhdata_combined$wealth_iqr_group,
  levels = c("Low", "Middle", "High")
)

##Clean data for ease of saving
knhhdata_combined <- knhhdata_combined %>%
  rename_with(~ gsub("[^A-Za-z0-9_]", "_", .x))


# Write data frame to a .dta file(My console)
write_dta(knhhdata_combined, file.path(LuDataDir, "knhhdata_combined.dta"))

##Read in back data
knhhdata_combined <- read_dta(file.path(LuDataDir, "knhhdata_combined.dta"))













