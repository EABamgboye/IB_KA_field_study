user <- Sys.getenv("USERNAME")
Datadir <- file.path("C:/Users/ebamgboye/Urban Malaria Proj Dropbox/urban_malaria/data/nigeria")
LSdatadir <- file.path(Datadir, "kano_ibadan_epi/Latest Longitudinal Survey Data_May 2025/Ibadan Longitudinal data")
IBHSdatadir <- file.path(Datadir, "kano_Ibadan_epi/Combined Working Data/Ibadan")
IBHSrawdir <- file.path(Datadir, "kano_Ibadan_epi/new_field_data")

my_backupdir <- "C:/Users/ebamgboye/Urban Malaria Proj Dropbox/urban_malaria/data/nigeria/UMP field data_backup"

library(haven)
library(dplyr)
library(tidyr)
library(janitor)
library(psych)

IBLSHH_data <- read_dta(file.path(LSdatadir, "IBLS_hhold list.dta"))

IBLSbase_data <- read_dta(file.path(LSdatadir, "IBLS_Baseline.dta"))

IBLSffwp_data <- read_dta(file.path(LSdatadir, "IBLS_Followup.dta"))

##Read in data set for malaria cases
IBLS_Basefmcases <- read_dta(file.path(LSdatadir,"IBLS_Baseline fever mal cases.dta"))

IBLS_Ffwpfmcases <- read_dta(file.path(LSdatadir,"IBLS_Followup fever mal cases.dta"))

##Some data wrangling
##Create new column for line number and rename
IBLSbase_data <- IBLSbase_data %>%
  mutate(line_no = q300ii) %>%
  relocate(line_no, .after = q300i) 

IBLSHH_data <- IBLSHH_data %>%
  mutate(line_no = redcap_repeat_instance) %>%
  relocate(line_no, .after = hl1) 

# Adding demographic info for selected child into baseline data 
IBLSbase_data <- IBLSbase_data %>%
  left_join(IBLSHH_data, by = c("sn", "line_no"))

##Generate line no in Followup data
IBLSffwp_data <- IBLSffwp_data %>%
  mutate(DBS_code = q707) %>%
  separate(DBS_code, into = c("serailn", "line_no"), sep = "/")

##Clean line numbers with leading zeros
IBLSffwp_data$line_no <- sub("^0+", "", IBLSffwp_data$line_no )

##Data wrangling for malaria cases database(Baseline)
##Remove duplicate entry
IBLS_Basefmcases <- IBLS_Basefmcases[-10, ]

##Create new column for line number and rename
IBLS_Basefmcases <- IBLS_Basefmcases %>%
  mutate(
    line_no = q504b,
    fever_or_suspected_malaria = redcap_repeat_instrument
  ) %>%
  relocate(line_no, fever_or_suspected_malaria, .after = sn)

##Add suffix to the variable names except sn and line no
IBLS_Basefmcases_rec <- IBLS_Basefmcases %>%
  rename_with(~ paste0(., "_baseline"), -c(sn, line_no))


##Add baseline data and malaria cases at baseline
IBLS_baselineall <- IBLSbase_data %>%
  left_join(IBLS_Basefmcases_rec, by = c("sn", "line_no"))


##Data wrangling for malaria cases database(Followup)
##Create new column for line number and rename
IBLS_Ffwpfmcases <- IBLS_Ffwpfmcases %>%
  mutate(line_no = q504b) %>%
  relocate(line_no, .after = sn)

##Remove wrong entry
IBLS_Ffwpfmcases <- IBLS_Ffwpfmcases[-216, ]

##Clean line numbers with leading zeros
IBLS_Ffwpfmcases$line_no <- sub("^0+", "", IBLS_Ffwpfmcases$line_no )

##Replace erroneous entry in line number
IBLS_Ffwpfmcases[335, 2] <- "4"

##Add a column to reflect they are fever or suspected cases at followup
IBLS_Ffwpfmcases$fever_or_suspected_malaria <- "fever_or_suspected_malaria"

#Covert follow up malaria case data to wide format before merging
IBLS_Ffwpfmcases_wide <- IBLS_Ffwpfmcases %>%
  dplyr::select(-c(redcap_repeat_instrument, redcap_repeat_instance, q504c, fever_or_suspected_m_v_0, bif13))%>%
  pivot_wider(
    id_cols = c(sn, line_no),
    names_from = redcap_event_name,
    values_from = -c(sn,line_no, redcap_event_name),
    names_sep = "_"
  )


# ##Combine baseline and follow_up (Long format)
# combined_LSdf <- IBLSffwp_data %>%
#   left_join(IBLSbase_data, by = "sn")


##Combine baseline and follow_up (Wide format)

##Remove redundant background information before combining to wide
IBLSffwp_data <- IBLSffwp_data %>% 
  dplyr::select(-c(redcap_repeat_instrument, redcap_repeat_instance, bif1, bif_long, bif_lat, bif3, bif4,
                 bif5, bif6, bif7, bif8, bif9, bif10, bif11, bif12, bif13, background_informati_v_0))


#Covert follow up to wide format before merging
IBLSffwp_data_wide <- IBLSffwp_data %>%
  pivot_wider(
    id_cols = sn,
    names_from = redcap_event_name,
    values_from = -c(sn, redcap_event_name),
    names_sep = "_"
  )

##Create a column for line number to be used for merging
IBLSffwp_data_wide <- IBLSffwp_data_wide %>%
  mutate(line_no = line_no_follow_up_1_arm_1)


##Add follow up data and malaria cases at follow ups
IBLS_ffwupall <- IBLSffwp_data_wide %>%
  left_join(IBLS_Ffwpfmcases_wide, by = c("sn", "line_no"))

labels_dfwup <- data.frame(
  variable = names(IBLS_ffwupall),
  label = sapply(IBLS_ffwupall, function(x) {
    lbl <- attr(x, "label")
    if (is.null(lbl)) NA_character_ else lbl
  }),
  stringsAsFactors = FALSE
)

labels_dfbase <- data.frame(
  variable = names(IBLS_baselineall),
  label = sapply(IBLS_baselineall, function(x) {
    lbl <- attr(x, "label")
    if (is.null(lbl)) NA_character_ else lbl
  }),
  stringsAsFactors = FALSE
)

##Write data sets to drop box
write.csv(labels_dfbase, file.path(LSdatadir, "codebook for_IBLS_baselineall.csv"))
write.csv(labels_dfwup, file.path(LSdatadir, "codebook for_IBLS_ffwupall.csv"))

##Write labels into codebook in drop box
write.csv(IBLS_baselineall, file.path(LSdatadir, "IBLS_baselineall.csv"))
write.csv(IBLS_ffwupall, file.path(LSdatadir, "IBLS_ffwupall.csv"))
# 
# ##Combine baseline and follow_up (Long format)
# combined_LSdf <- IBLSffwp_data %>%
#   left_join(IBLSbase_data, by = "sn")


##Combine baseline and follow_up (Wide format)


# # Where all values in q707_follow_up_1_arm_1 : q707_follow_up_12_arm_1 are equal
# IBLSffwp_data_match <- IBLSffwp_data_wide %>%
#   filter(if_all(q707_follow_up_1_arm_1:q707_follow_up_12_arm_1, ~ . == q707_follow_up_1_arm_1))
# 
# # Where any value in that range differs
# IBLSffwp_data_nomatch <- IBLSffwp_data_wide %>%
#   filter(!if_all(q707_follow_up_1_arm_1:q707_follow_up_12_arm_1, ~ . == q707_follow_up_1_arm_1))


##Household data
# #Read in Wide format of Household data (wet Season)
# IBHH_widedata_wet <- read.csv(file.path(IBHSdatadir, "wet Season Data/Wide Data/ibadan_wetseason_wide_data_final.csv"))
# 
# ##Generate labels for ease of understanding the dataset
# labels_IBHH_widedata_wet <- data.frame(
#   variable = names(IBHH_widedata_wet),
#   label = map_chr(names(IBHH_widedata_wet), function(v) {
#     lbl <- attr(IBHH_widedata_wet[[v]], "label")
#     if (is.null(lbl)) {
#       NA_character_
#     } else if (length(lbl) > 1) {
#       paste(lbl, collapse = "; ")  # or lbl[1] if you prefer just the first
#     } else {
#       lbl
#     }
#   }),
#   stringsAsFactors = FALSE
# )

##Read in Ibadan Household data in wide format(Wet Season)
IBHH_widedata_wet <- read_dta(file.path(IBHSdatadir, "Wet Season Data/Wide Data/all_ibadan_wetseason_dater_wide.dta"))

IBHH_widedata_wet <- read_dta(file.path(my_backupdir, "Old/Wet/Combined and working format/all_ibadan_wetseason_dater_wide.dta"))

my_backupdir
library(stringr)

# Remove identifiers in  columns 1058 to 2611
colnames(IBHH_widedata_wet)[1058:2611] <- str_replace(colnames(IBHH_widedata_wet)[1058:2611],
                                              "(^([^_]+_[^_]+)_).*", "\\1")

# Add '_men' suffix to column names in positions 650 to 916
colnames(IBHH_widedata_wet)[650:916] <- paste0(colnames(IBHH_widedata_wet)[650:916], "_men")

# Replace any character that is NOT a-z, A-Z, 0-9, or _ with _
names(IBHH_widedata_wet) <- gsub("[^A-Za-z0-9_]", "_", names(IBHH_widedata_wet))


##Generate labels for ease of understanding the dataset
labels_IBHH_widedata_wet <- data.frame(
  variable = names(IBHH_widedata_wet),
  label = map_chr(names(IBHH_widedata_wet), function(v) {
    lbl <- attr(IBHH_widedata_wet[[v]], "label")
    if (is.null(lbl)) {
      NA_character_
    } else if (length(lbl) > 1) {
      paste(lbl, collapse = "; ")  # or lbl[1] if you prefer just the first
    } else {
      lbl
    }
  }),
  stringsAsFactors = FALSE
)

##Write labels as code book into drop box
write.csv(labels_IBHH_widedata_wet, file.path(my_backupdir, "IB_codebookwide_wet.csv"))
# Save to Stata
write_dta(IBHH_widedata_wet, file.path(my_backupdir, "all_ibadan_wetseason_dater_wide1.dta"))



##Read in men dry season data
IB_mendata_dry <- read_dta(file.path(IBHSrawdir, "Ibadan Dry Season data_latest_Nov24/Ibadan Dry season survey data/IB Dry season Men_revised 131124.dta"))

IB_mendata_dry <- read_dta(file.path(my_backupdir, "Ibadan Dry Season Corrected Household Data_Jan 2025/IB Dry season Men_revised 131124 (1).dta"))

##Read in household information for Ibadan(Dry Season)
IBHHold_data_dry <- read_dta(file.path(my_backupdir, "Ibadan Dry Season Corrected Household Data_Jan 2025/IB dry season hhold data_edited_150125.dta"))

IBHH_mendata_dry <- IBHHold_data_dry %>% 
  inner_join(IB_mendata_dry, by = c("sn"))

# Replace any character that is NOT a-z, A-Z, 0-9, or _ with _
names(IBHH_mendata_dry) <- gsub("[^A-Za-z0-9_]", "_", names(IBHH_mendata_dry))


##Generate labels for ease of understanding the dataset
labels_IBHH_mendata_dry <- data.frame(
  variable = names(IBHH_mendata_dry),
  label = map_chr(names(IBHH_mendata_dry), function(v) {
    lbl <- attr(IBHH_mendata_dry[[v]], "label")
    if (is.null(lbl)) {
      NA_character_
    } else if (length(lbl) > 1) {
      paste(lbl, collapse = "; ")  # or lbl[1] if you prefer just the first
    } else {
      lbl
    }
  }),
  stringsAsFactors = FALSE
)

##Write labels as code book into drop box
write.csv(labels_IBHH_mendata_dry, file.path(my_backupdir, "IB_codebookhhmen_dry.csv"))
# Save to Stata
write_dta(IBHH_mendata_dry, file.path(my_backupdir, "Ibadan_dryseason_hh_men_dry.dta"))


#------------------------------------------------------------------------------------------------------------------------------

##Data wrangling for children with fever and tested in household survey
##Read in Ibadan Household data with rdt results(Dry Season)
IBHH_data_rdt_dry <- read_dta(file.path(IBHSrawdir, "Ibadan Dry Season Corrected Household Data_Jan 2025/IB dry season hhold list wt RDT_150125.dta"))


IBHH_data_rdt_dry <- read_dta(file.path(my_backupdir, "Ibadan Dry Season Corrected Household Data_Jan 2025/IB dry season hhold list wt RDT_150125.dta"))

##Read in Ibadan household data without RDT to obtain background information
IBHH_data_dry <- read_dta(file.path(IBHSrawdir, "Ibadan Dry Season Corrected Household Data_Jan 2025/IB dry season hhold data_edited_150125.dta"))

##Read in dataset for kids who had fever or suspected malaria
IBHH_kids_malhx_dry <- read_dta(file.path(IBHSrawdir, "Ibadan Dry Season data_latest_Nov24/Ibadan Dry season survey data/IB Dry  Women malaria hx.dta"))

IBHH_kids_malhx_dry <- read_dta(file.path(my_backupdir, "Old/Dry/Women/IB Dry  Women malaria hx.dta"))

#rename column name to facilitate merge
IBHH_kids_malhx_dry <- IBHH_kids_malhx_dry %>% 
  rename(line_no = q404b)

##Split dbs code to generate line no on rdt dataset 
IBHH_data_rdt_dry <- IBHH_data_rdt_dry %>%
  mutate(newdbs_code1 = newdbs_code) %>%
  separate(newdbs_code1, into = c("serailn", "line_no"), sep = "/")

##Ensure line number is numeric to facilitate merging
IBHH_kids_malhx_dry <- IBHH_kids_malhx_dry %>%
  mutate(line_no = as.numeric(line_no))

IBHH_data_rdt_dry <- IBHH_data_rdt_dry %>%
  mutate(line_no = as.numeric(line_no))

##Merge data set to filter out those that had a test carried out
IBHH_kidsmal_rdt_data_dry <- IBHH_kids_malhx_dry %>%
  inner_join(IBHH_data_rdt_dry, by = c("sn", "line_no"))

#Remove column with identifiers and household heads(line_no 1), and then Clean column names
IBHH_kidsmal_rdt_data_dry <- IBHH_kidsmal_rdt_data_dry %>%
  dplyr::filter(line_no != 1) %>%
  dplyr::select(-c(q404a, redcap_repeat_instrument.x, redcap_repeat_instance.x))%>%
  janitor::clean_names()



##Read in Ibadan Household data with rdt results(Wet Season)
IBHH_data_rdt_wet <- read_dta(file.path(IBHSrawdir, "Ibadan Wet Season data Sept 2024/IB Wet season household members RDT_160924.dta"))

IBHH_data_rdt_wet <- read_dta(file.path(my_backupdir, "Ibadan Wet Season data Sept 2024/IB Wet season household members RDT_160924.dta"))


##Read in Ibadan household data without RDT to obtain background information
IBHH_data_wet <- read_dta(file.path(IBHSrawdir, "Ibadan Wet Season data Sept 2024/IB Wet season household data_edited_220924 rev.dta"))


#rename column name to facilitate merge
IBHH_data_rdt_wet <- IBHH_data_rdt_wet %>% 
  rename(line_no = ln)

##Read in data set for kids who had fever or suspected malaria
IBHH_kids_malhx_wet <- read_dta(file.path(IBHSrawdir, "Ibadan Wet Season data Sept 2024/IB Wet season Women malaria hx_rev 220924.dta"))

IBHH_kids_malhx_wet <- read_dta(file.path(my_backupdir, "Ibadan Wet Season data Sept 2024/IB Wet season Women malaria hx_rev 220924.dta"))


#rename column name to facilitate merge
IBHH_kids_malhx_wet <- IBHH_kids_malhx_wet %>% 
  rename(line_no = q404b)

##Clean line numbers with leading zeros
IBHH_kids_malhx_wet$line_no <- sub("^0+", "", IBHH_kids_malhx_wet$line_no )


# ##Split dbs code to generate line no on rdt dataset 
# IBHH_data_rdt_wet <- IBHH_data_rdt_wet %>%
#   mutate(newdbs_code1 = newdbs_code) %>%
#   separate(newdbs_code1, into = c("serailn", "line_no"), sep = "/")



##Ensure line number is numeric to facilitate merging
IBHH_kids_malhx_wet <- IBHH_kids_malhx_wet %>%
  mutate(line_no = as.numeric(line_no))

IBHH_data_rdt_wet <- IBHH_data_rdt_wet %>%
  mutate(line_no = as.numeric(line_no))

##Merge data set to filter out those that had a test carried out
IBHH_kidsmal_rdt_data_wet <- IBHH_kids_malhx_wet %>%
  inner_join(IBHH_data_rdt_wet, by = c("sn", "line_no"))

#Remove column with identifiers and household heads(line_no 1), and then Clean column names
IBHH_kidsmal_rdt_data_wet <- IBHH_kidsmal_rdt_data_wet %>%
  dplyr::filter(line_no != 1) %>%
  dplyr::select(-c(q404a, redcap_repeat_instrument.x, redcap_repeat_instance.x))%>%
  janitor::clean_names()

# ##Write data to folder in drop box
# # Clean up names
# names(IBHH_kidsmal_rdt_data_dry) <- gsub("\\.", "_", names(IBHH_kidsmal_rdt_data_dry))
# 
# names(IBHH_kidsmal_rdt_data_wet) <- gsub("\\.", "_", names(IBHH_kidsmal_rdt_data_wet))


##Generate labels for ease of understanding the datasets
##Dry
labels_IBHH_kidsmal_rdt_data_dry <- data.frame(
  variable = names(IBHH_kidsmal_rdt_data_dry),
  label = map_chr(names(IBHH_kidsmal_rdt_data_dry), function(v) {
    lbl <- attr(IBHH_kidsmal_rdt_data_dry[[v]], "label")
    if (is.null(lbl)) {
      NA_character_
    } else if (length(lbl) > 1) {
      paste(lbl, collapse = "; ")  # or lbl[1] if you prefer just the first
    } else {
      lbl
    }
  }),
  stringsAsFactors = FALSE
)

##Wet
labels_IBHH_kidsmal_rdt_data_wet <- data.frame(
  variable = names(IBHH_kidsmal_rdt_data_wet),
  label = map_chr(names(IBHH_kidsmal_rdt_data_wet), function(v) {
    lbl <- attr(IBHH_kidsmal_rdt_data_wet[[v]], "label")
    if (is.null(lbl)) {
      NA_character_
    } else if (length(lbl) > 1) {
      paste(lbl, collapse = "; ")  # or lbl[1] if you prefer just the first
    } else {
      lbl
    }
  }),
  stringsAsFactors = FALSE
)


##Write data to drop box
write_dta(IBHH_kidsmal_rdt_data_wet, file.path(IBHSrawdir, "mrpt_analysis data/IBHH_kidsmal_data_wet.dta"))
write_dta(IBHH_kidsmal_rdt_data_dry, file.path(IBHSrawdir, "mrpt_analysis data/IBHH_kidsmal_data_dry.dta"))



##Write labels as code book into drop box
write.csv(labels_IBHH_kidsmal_rdt_data_dry , file.path(IBHSrawdir, "mrpt_analysis data/IB_kidscodebook_dry.csv"))
write.csv(labels_IBHH_kidsmal_rdt_data_wet, file.path(IBHSrawdir, "mrpt_analysis data/IB_kidscodebook_wet.csv"))

# 
# write_dta(IBHH_kidsmal_rdt_data_wet, file.path(IBHSrawdir, "Ibadan Wet Season data Sept 2024/IB_kidsmal_data_Wet.dta"))
# write_dta(IBHH_kidsmal_rdt_data_dry, file.path(my_backupdir, "Ibadan Dry Season Corrected Household Data_Jan 2025/IB_kidsmal_data_dry.dta"))
# 







###Create household level data for incidence-prevalence analysis
##Men's data + those with known fever status
##Dry
##Read in mens data
IB_mendata_dry <- read_dta(file.path(IBHSrawdir, "Ibadan Dry Season data_latest_Nov24/Ibadan Dry season survey data/IB Dry season Men_revised 131124.dta"))

##Select necessary variables
IB_IP_mendata_dry <- IB_mendata_dry %>% 
  dplyr::select(sn, bi1, bi2, bi3, q201a, q201b, q401, q402, q701, q702, q703, q704)

#Add gender(hl4) and rename columns to facilitate appending
IB_IP_mendata_dry <- IB_IP_mendata_dry %>%
  mutate(hl4 = 1) %>% 
  rename(hl5 = q201a,
         hl6 = q201b,
         q301 = q701, 
         q302 = q702, 
         q303 = q703,
         newdbs_code = q704)


##Add HHold background information to kidsmal data 
IBHH_data_dry <- IBHH_data_rdt_dry

IBHH_kidsmal_rdt_data_dry <- IBHH_kidsmal_rdt_data_dry %>%
  inner_join(IBHH_data_dry, by = c("sn"))

IB_HHothersmal_data_dry <- IBHH_kidsmal_rdt_data_dry %>% 
  dplyr::select(sn, bi1, bi2, bi3, hl4, hl5, hl6, q301, q302, q303, newdbs_code)

##Add extra variables to facilitate appending
IB_HHothersmal_data_dry <- IB_HHothersmal_data_dry %>% 
  mutate(q401 = 1,
         q402 = 1)

##Append men and other household members with fever information
IB_IP_alldata_dry <- rbind(IB_IP_mendata_dry,  IB_HHothersmal_data_dry)

#Separate dbscode to generate line no
IB_IP_alldata_dry  <- IB_IP_alldata_dry %>% 
  separate(newdbs_code, into = c("serailn", "line_no"), sep = "/")

##Check for duplicates and remove
duplicates_df <- IB_IP_alldata_dry %>%
  group_by(sn) %>%
  filter(n() > 1) %>%
  ungroup()

IB_IP_alldata_dry <- IB_IP_alldata_dry[-1437,] 


##Wet
##Read in men's data
IB_mendata_wet <- read_dta(file.path(IBHSrawdir, "/Ibadan Wet Season data Sept 2024/IB Wet season Men survey_rev 220924.dta"))

##Select necessary variables
IB_IP_mendata_wet <- IB_mendata_wet %>% 
  dplyr::select(sn, bi1, bi2, bi3, q201a, q201b, q401, q402, q701, q702, q703, q704)

#Add gender(hl4) and rename columns to facilitate appending
IB_IP_mendata_wet <- IB_IP_mendata_wet %>%
  mutate(hl4.x = 1) %>% 
  rename(hl5.x = q201a,
         hl6 = q201b,
         q301 = q701, 
         q302 = q702, 
         q303 = q703,
         newdbs_code = q704,
         settlement = bi3)


##Add HHold background information to kidsmal data 
IBHH_kidsmal_rdt_data_wet <- IBHH_kidsmal_rdt_data_wet %>%
  inner_join(IBHH_data_wet, by = c("sn"))

IB_HHothersmal_data_wet <- IBHH_kidsmal_rdt_data_wet %>% 
  dplyr::select(sn, bi1, bi2, settlement, hl4.x, hl5.x, hl6, q301, q302, q303, newdbs_code)

##Add extra variables to facilitate appending
IB_HHothersmal_data_wet <- IB_HHothersmal_data_wet %>% 
  mutate(q401 = 1,
         q402 = 1)

##Append men and other household members with fever information
IB_IP_alldata_wet <- rbind(IB_IP_mendata_wet,  IB_HHothersmal_data_wet)

#Separate dbscode to generate line no
IB_IP_alldata_wet  <- IB_IP_alldata_wet %>% 
  separate(newdbs_code, into = c("serailn", "line_no"), sep = "/")

##Check for duplicates and remove
duplicates_df <- IB_IP_alldata_wet %>%
  group_by(sn) %>%
  filter(n() > 1) %>%
  ungroup()

IB_IP_alldata_wet <- IB_IP_alldata_wet[-c(119, 69),] 


###Generate the label
labels_IB_IP_alldata_dry <- data.frame(
  variable = names(IB_IP_alldata_dry),
  label = map_chr(names(IB_IP_alldata_dry), function(v) {
    lbl <- attr(IB_IP_alldata_dry[[v]], "label")
    if (is.null(lbl)) {
      NA_character_
    } else if (length(lbl) > 1) {
      paste(lbl, collapse = "; ")  # or lbl[1] if you prefer just the first
    } else {
      lbl
    }
  }),
  stringsAsFactors = FALSE
)

names(IB_IP_alldata_wet) <- gsub("\\.", "_", names(IB_IP_alldata_wet))

##Write data to drop box
write_dta(IB_IP_alldata_wet, file.path(IBHSrawdir, "mrpt_analysis data/IB_IP_alldata_wet.dta"))
write_dta(IB_IP_alldata_dry, file.path(IBHSrawdir, "mrpt_analysis data/IB_IP_alldata_dry.dta"))



##Write labels as code book into drop box
write.csv(labels_IBHH_kidsmal_rdt_data_dry , file.path(IBHSrawdir, "mrpt_analysis data/IB_kidscodebook_dry.csv"))
write.csv(labels_IB_IP_alldata_wet, file.path(IBHSrawdir, "mrpt_analysis data/IB_alldatacodebook_wet.csv"))


##Prevalence analysis(2-10)
IB_LongBase <- read.csv(file.path("C:/Users/ebamgboye/Urban Malaria Proj Dropbox/urban_malaria/data/nigeria/kano_ibadan_epi/Latest Longitudinal Survey Data_May 2025/Ibadan longitudinal data/IBLS_baselineall.csv"))

IB_LongFFwup <- read.csv(file.path("C:/Users/ebamgboye/Urban Malaria Proj Dropbox/urban_malaria/data/nigeria/kano_ibadan_epi/Latest Longitudinal Survey Data_May 2025/Ibadan longitudinal data/IBLS_ffwupall.csv"))


##Update followup data with some background information columns(age, ward and settlement)
IB_LongFFwup <- IB_LongFFwup %>%
  left_join(select(IB_LongBase, sn, bi2, bi3, q300iii), by = "sn")



#Overall monthly prevalence computation

##Recode test result to 1(Positive) and 0(Negative)
IB_LongFFwup_prev <- IB_LongFFwup %>%
  mutate(across(starts_with("q705_follow_up_"), ~case_when(
    .x == 1 ~ 1,
    .x == 2 ~ 0,
    TRUE ~ NA_real_
  )))

#Estimate monthly prevalence
IB_monthly_prev <- IB_LongFFwup_prev %>%
  summarise(across(starts_with("q705_"), ~mean(.x, na.rm = TRUE)))

##Compute geometric mean of prevalence

ib_gm <- geometric.mean(IB_monthly_prev[IB_monthly_prev > 0])  

##Visualize Prevalence
IB_monthly_prev_long <- IB_monthly_prev %>%
  pivot_longer(cols = everything(), names_to = "Month", values_to = "Prevalence") %>%
  mutate(MonthNum = as.numeric(gsub("q705_follow_up_|_arm_1", "", Month))) %>%
  arrange(MonthNum)

ggplot(IB_monthly_prev_long, aes(x = MonthNum, y = Prevalence)) +
  geom_line(color = "darkred", size = 1.2) +
  geom_point(color = "black", size = 2) +
  geom_hline(yintercept = ib_gm, linetype = "dashed", color = "blue", size = 1) +# Add geom_hline here
  #facet_wrap(~ bi2)+
  annotate("text", x = 10, y = ib_gm, label = paste0("Geometric Mean = ", round(ib_gm, 4)),
           vjust = -1, color = "blue", size = 4) +                               # Add annotation label
  scale_x_continuous(breaks = 1:12, labels = paste0("Month ", 1:12)) +
  labs(title = "Monthly Malaria Prevalence Trend for kids 0-10yrs(Ibadan)",
       x = "Month of Follow-up",
       y = "Prevalence (Proportion Positive)") +
  theme_minimal() #+
  #theme(axis.text.x = element_text(angle = 45, hjust = 1))


##Children aged 2-10 years only
IBLSbasekids_2_10 <- IB_LongFFwup_prev %>%
  dplyr::filter(q300iii >= 2 & q300iii <= 10)

#Estimate monthly prevalence
IB_monthly_prev_2_10 <- IBLSbasekids_2_10 %>%
  #group_by(bi3) %>%  
  summarise(across(starts_with("q705_"), ~mean(.x, na.rm = TRUE)))

##Compute geometric mean of prevalence
IB_gm_2_10 <- geometric.mean(IB_monthly_prev_2_10[IB_monthly_prev_2_10 > 0])  

##Visualize Prevalence
IB_monthly_prev_long_2_10 <- IB_monthly_prev_2_10 %>%
  pivot_longer(cols = starts_with("q705_follow_up"), 
               names_to = "Month", values_to = "Prevalence") %>%
  mutate(MonthNum = as.numeric(gsub("q705_follow_up_|_arm_1", "", Month))) %>%
  arrange(MonthNum)


##Compute geometric mean by settlement type
gm_settlement <- IB_monthly_prev_long_2_10 %>%
  group_by(bi3) %>%
  summarise(gm = geometric.mean(Prevalence[Prevalence > 0])) # exclude zeros for geometric mean

##Add geometric mean to data by settlement type
IB_monthly_prev_long_2_10gm <- IB_monthly_prev_long_2_10 %>%
  left_join(gm_settlement, by = "bi3")

##Recode settlement type before plotting
IB_monthly_prev_long_2_10gm <- IB_monthly_prev_long_2_10gm %>%
  mutate(SettlementType = recode(bi3,
                                 `1` = "Formal",
                                 `2` = "Informal",
                                 `3` = "Slum"))
gm_settlement <- gm_settlement %>%
  mutate(SettlementType = recode(bi3,
                                 `1` = "Formal",
                                 `2` = "Informal",
                                 `3` = "Slum"))


ggplot(IB_monthly_prev_long_2_10gm, aes(x = MonthNum, y = Prevalence)) +
  geom_line(color = "darkred", size = 1.2) +
  geom_point(color = "black", size = 2) +
  geom_hline(aes(yintercept = gm), linetype = "dashed", color = "blue", size = 1) +
  facet_wrap(~ SettlementType) +
  geom_text(data = gm_settlement, aes(x = 10, y = gm, label = paste0("Geometric Mean = ", round(gm, 4))),
            color = "red", size = 4, hjust = 0.4, vjust = -4, inherit.aes = FALSE)+
    scale_x_continuous(breaks = 1:12, labels = paste0("Month ", 1:12)) +
  labs(title = "Monthly Malaria Prevalence Trend kids 2-10yrs(Ibadan)",
       x = "Month of Follow-up",
       y = "Prevalence (Proportion Positive)") +
  theme_minimal() #+
  #theme(axis.text.x = element_text(angle = 45, hjust = 1))


