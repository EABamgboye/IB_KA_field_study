user <- Sys.getenv("USERNAME")
Datadir <- file.path("C:/Users/ebamgboye/Urban Malaria Proj Dropbox/urban_malaria/data/nigeria")
KNLSdatadir <- file.path(Datadir, "kano_KNadan_epi/Latest Longitudinal Survey Data_May 2025/Re_ Kano longitudinal data")

library(haven)
library(dplyr)
library(tidyr)
library(lubridate)

KNLSHH_data <- read_dta(file.path(KNLSdatadir, "KNLS- Household list.dta"))

KNLSbase_data <- read_dta(file.path(KNLSdatadir, "KNLS_Baseline.dta"))

KNLSffwp_data <- read_dta(file.path(KNLSdatadir, "KNLS_Followup.dta"))

##My Laptop
KNLSHH_data <- read_dta(file.path(LuDir, "KNLS- Household list.dta"))

KNLSbase_data <- read_dta(file.path(LuDir, "KNLS- Baseline.dta"))

KNLSffwp_data <- read_dta(file.path(LuDir, "KNLS- Followup.dta"))



##Read in data set for malaria cases
KNLS_Basefmcases <- read_dta(file.path(LuDir,"KNLS- fever malaria cases-baseline.dta"))

KNLS_Ffwpfmcases <- read_dta(file.path(LuDir,"KNLS- fever malaria cases-FUP.dta"))


##Some data wrangling
##Baseline and Household
##Create new column for line number and rename
KNLSbase_data <- KNLSbase_data %>%
  mutate(line_no = q300ii) %>%
  relocate(line_no, .after = q300i) 

# # Age issues due to wrong entry in dates
# age_issue_df <- KNLSbase_data %>%
#   dplyr::filter(age_years <= 0) %>%
#   dplyr::select(sn, age_years, q301, bi9, bi12i, everything())

##Correct the entries with date issues (q301)
KNLSbase_data <- KNLSbase_data %>%
  mutate(q301 = case_when(
    sn == 31929 ~ as.Date("2017-01-07"),
    sn == 231 ~ as.Date("2023-01-18"),
    sn == 27850 ~ as.Date("2020-03-29"),
    sn == 20576 ~ as.Date("2018-04-24"),
    TRUE ~ q301
  ))

##Correct the entries with date issues (bi12i)
KNLSbase_data <- KNLSbase_data %>%
  mutate(bi12i = case_when(
    sn == 20846 ~ as.Date("2024-01-11"),
    sn == 21112 ~ as.Date("2024-01-29"),
    sn == 20034 ~ as.Date("2024-02-10"),
    TRUE ~ bi12i
  ))


##Compute new age variable
KNLSbase_data <- KNLSbase_data %>%
  mutate(
    age_years = floor(interval(q301, bi12i) / years(1))
  )

##Household data wrangling
KNLSHH_data <- KNLSHH_data %>%
  mutate(line_no = redcap_repeat_instance) %>%
  relocate(line_no, .after = hl1) 

##Ensure variables are compatible before joining
KNLSbase_data <- KNLSbase_data %>%
  mutate(line_no = as.character(line_no))

KNLSHH_data <- KNLSHH_data %>%
  mutate(line_no = as.character(line_no))



# Adding demographic info for selected child into baseline data 
KNLSbase_data <- KNLSbase_data %>%
  left_join(KNLSHH_data, by = c("sn", "line_no"))




##Generate line no in Followup data
KNLSffwp_data <- KNLSffwp_data %>%
  mutate(DBS_code = q707) %>%
  separate(DBS_code, into = c("serailn", "line_no"), sep = "/")

##Clean line numbers with leading zeros
KNLSffwp_data$line_no <- sub("^0+", "", KNLSffwp_data$line_no )



##Data wrangling for malaria cases database(Baseline)
# ##Remove duplicate entry
# KNLS_Basefmcases <- KNLS_Basefmcases[-10, ]

##Create new column for line number and rename
KNLS_Basefmcases <- KNLS_Basefmcases %>%
  mutate(
    line_no = q504b,
    fever_or_suspected_malaria = redcap_repeat_instrument
  ) %>%
  relocate(line_no, fever_or_suspected_malaria, .after = sn)

##Add suffix to the variable names except sn and line no
KNLS_Basefmcases_rec <- KNLS_Basefmcases %>%
  rename_with(~ paste0(., "_baseline"), -c(sn, line_no))

##Ensure variables are compatible before joining
KNLSbase_data <- KNLSbase_data %>%
  mutate(line_no = as.character(line_no))

KNLS_Basefmcases_rec <- KNLS_Basefmcases_rec %>%
  mutate(line_no = as.character(line_no))


##Add baseline data and malaria cases at baseline
KNLS_baselineall <- KNLSbase_data %>%
  left_join(KNLS_Basefmcases_rec, by = c("sn", "line_no"))


##Data wrangling for malaria cases database(Followup)
##Create new column for line number and rename
KNLS_Ffwpfmcases <- KNLS_Ffwpfmcases %>%
  mutate(line_no = q504b) %>%
  relocate(line_no, .after = sn)

# ##Remove wrong entry
# KNLS_Ffwpfmcases <- KNLS_Ffwpfmcases[-216, ]

##Clean line numbers with leading zeros
KNLS_Ffwpfmcases$line_no <- sub("^0+", "", KNLS_Ffwpfmcases$line_no )

##Replace erroneous row in entry
KNLS_Ffwpfmcases <- KNLS_Ffwpfmcases[-40, ] 

##Add a column to reflect they are fever or suspected cases at followup
KNLS_Ffwpfmcases$fever_or_suspected_malaria <- "fever_or_suspected_malaria"

#Covert follow up malaria case data to wide format before merging
KNLS_Ffwpfmcases_wide <- KNLS_Ffwpfmcases %>%
  dplyr::select(-c(redcap_repeat_instrument, redcap_repeat_instance, q504c, fever_or_suspected_m_v_0, bif13))%>%
  pivot_wider(
    id_cols = c(sn, line_no),
    names_from = redcap_event_name,
    values_from = -c(sn,line_no, redcap_event_name),
    names_sep = "_"
  )


# ##Combine baseline and follow_up (Long format)
# combined_LSdf <- KNLSffwp_data %>%
#   left_join(KNLSbase_data, by = "sn")


##Combine baseline and follow_up (Wide format)

##Remove redundant background information before combining to wide
KNLSffwp_data <- KNLSffwp_data %>% 
  dplyr::select(-c(redcap_repeat_instrument, redcap_repeat_instance, bif1, bif_long, bif_lat, bif3, bif4,
                   bif5, bif7, bif8, bif9, bif10, bif11, bif12, bif13, background_informati_v_0))


#Covert follow up to wide format before merging
KNLSffwp_data_wide <- KNLSffwp_data %>%
  pivot_wider(
    id_cols = sn,
    names_from = redcap_event_name,
    values_from = -c(sn, redcap_event_name),
    names_sep = "_"
  )

##Create a column for line number to be used for merging
KNLSffwp_data_wide <- KNLSffwp_data_wide %>%
  mutate(line_no = line_no_follow_up_1_arm_1)


##Add follow up data and malaria cases at follow ups
KNLS_ffwupall <- KNLSffwp_data_wide %>%
  left_join(KNLS_Ffwpfmcases_wide, by = c("sn", "line_no"))

labels_dfwup <- data.frame(
  variable = names(KNLS_ffwupall),
  label = sapply(KNLS_ffwupall, function(x) {
    lbl <- attr(x, "label")
    if (is.null(lbl)) NA_character_ else lbl
  }),
  stringsAsFactors = FALSE
)

labels_dfbase <- data.frame(
  variable = names(KNLS_baselineall),
  label = sapply(KNLS_baselineall, function(x) {
    lbl <- attr(x, "label")
    if (is.null(lbl)) NA_character_ else lbl
  }),
  stringsAsFactors = FALSE
)

##Write data sets to drop box
write.csv(labels_dfbase, file.path(LuDir, "codebook for_KNLS_baselineall.csv"))
write.csv(labels_dfwup, file.path(LuDir, "codebook for_KNLS_ffwupall.csv"))

##Write labels into codebook in drop box
write.csv(KNLS_baselineall, file.path(LuDir, "KNLS_baselineall.csv"))
write.csv(KNLS_ffwupall, file.path(LuDir, "KNLS_ffwupall.csv"))
# 
# ##Combine baseline and follow_up (Long format)
# combined_LSdf <- KNLSffwp_data %>%
#   left_join(KNLSbase_data, by = "sn")


##Combine baseline and follow_up (Wide format)


# # Where all values in q707_follow_up_1_arm_1 : q707_follow_up_12_arm_1 are equal
# KNLSffwp_data_match <- KNLSffwp_data_wide %>%
#   filter(if_all(q707_follow_up_1_arm_1:q707_follow_up_12_arm_1, ~ . == q707_follow_up_1_arm_1))
# 
# # Where any value in that range differs
# KNLSffwp_data_nomatch <- KNLSffwp_data_wide %>%
#   filter(!if_all(q707_follow_up_1_arm_1:q707_follow_up_12_arm_1, ~ . == q707_follow_up_1_arm_1))







##Specific to Incidence and Prevalence
KNLS_baselineall



KNLSffwp_tpr_wide <- KNLSffwp_data %>%
  pivot_wider(
    id_cols = sn,
    names_from = redcap_event_name,
    values_from = c(bif4, q501, q502, q503, q702i, q703, q704, q705, q706),
    names_sep = "_"
  )




##Add baseline information
KNLS_tprdata_wide <- KNLSbase_data %>%
  left_join(KNLSffwp_tpr_wide, by = "sn")

##Add baseline malaria case data
KNLS_Basefmcases_rec
