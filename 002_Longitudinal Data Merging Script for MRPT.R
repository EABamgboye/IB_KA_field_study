user <- Sys.getenv("USERNAME")
Datadir <- file.path("C:/Users/ebamgboye/Urban Malaria Proj Dropbox/urban_malaria/data/nigeria")
KNLSdatadir <- file.path(Datadir, "kano_Ibadan_epi/Latest Longitudinal Survey Data_May 2025/Re_ Kano longitudinal data")
KNHSdatadir <- file.path(Datadir, "kano_Ibadan_epi/Combined Working Data/Kano")
KNHHSrawdir <- file.path(Datadir, "kano_Ibadan_epi/new_field_data")



library(haven)
library(dplyr)
library(tidyr)
library(lubridate)
library(janitor)
library(psych)

KNLSHH_data <- read_dta(file.path(KNLSdatadir, "KNLS- Household list.dta"))

KNLSbase_data <- read_dta(file.path(KNLSdatadir, "KNLS- Baseline.dta"))

KNLSffwp_data <- read_dta(file.path(KNLSdatadir, "KNLS- Followup.dta"))

# ##My Laptop
LuDir <- file.path(Drive, "Documents")
# KNLSHH_data <- read_dta(file.path(LuDir, "KNLS- Household list.dta"))
# 
# KNLSbase_data <- read_dta(file.path(LuDir, "KNLS- Baseline.dta"))
# 
# KNLSffwp_data <- read_dta(file.path(LuDir, "KNLS- Followup.dta"))



##Read in data set for malaria cases
KNLS_Basefmcases <- read_dta(file.path(KNLSdatadir,"KNLS- fever malaria cases-baseline.dta"))

KNLS_Ffwpfmcases <- read_dta(file.path(KNLSdatadir,"KNLS- fever malaria cases-FUP.dta"))


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

-------------------------------------------------------------------------------
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


##Household data
#Read in Wide format of Household data (Dry Season)

KNHH_widedata_dry <- read_dta(file.path(KNHSdatadir, "Dry Season Data/Wide Data/kano_dryseason_wide_data.dta"))

##Generate labels for ease of understanding the dataset
labels_KNHH_widedata_dry <- data.frame(
  variable = names(KNHH_widedata_dry),
  label = map_chr(names(KNHH_widedata_dry), function(v) {
    lbl <- attr(KNHH_widedata_dry[[v]], "label")
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
##Read in Kano Household data in wide format(Wet Season)
KNHH_widedata_wet <- read_dta(file.path(KNHSdatadir, "Wet Season Data/Wide Data/kano_wetseason_wide_data_final.dta"))

##Generate labels for ease of understanding the dataset
labels_KNHH_widedata_wet <- data.frame(
  variable = names(KNHH_widedata_wet),
  label = map_chr(names(KNHH_widedata_wet), function(v) {
    lbl <- attr(KNHH_widedata_wet[[v]], "label")
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
write.csv(labels_KNHH_widedata_dry, file.path(KNHSdatadir, "Dry Season Data/Wide Data/KN_codebookwide_dry.csv"))
write.csv(labels_KNHH_widedata_wet, file.path(KNHSdatadir, "Wet Season Data/Wide Data/KN_codebookwide_wet.csv"))












##Data wrangling for kids with fever and tested in household survey
##Read in Kano Household data with rdt results(Dry Season)
KNHH_data_rdt_dry <- read_dta(file.path(KNHHSrawdir, "Kano Dry Season Data_latest_Nov2024/Kano dry season survey data/KN dry hhold with RDT_131124.dta"))


KNHH_data_rdt_dry <- read_dta(file.path(my_backupdir, "Kano latest Dry Season Corrected Data_Jan 2025/KN dry hhold with RDT_131124.dta"))


##Read in dataset for kids who had fever or suspected malaria
KNHH_kids_malhx_dry <- read_dta(file.path(KNHHSrawdir, "Kano latest Dry Season Corrected Data_Jan 2025/KN dry- women malaria hx.dta"))

KNHH_kids_malhx_dry <- read_dta(file.path(my_backupdir, "Kano latest Dry Season Corrected Data_Jan 2025/KN dry- women malaria hx.dta"))

#rename column name to facilitate merge
KNHH_kids_malhx_dry <- KNHH_kids_malhx_dry %>% 
  rename(line_no = q404b)

##Split dbs code to generate line no on rdt dataset 
KNHH_data_rdt_dry <- KNHH_data_rdt_dry %>%
  mutate(newdbs_code1 = new_dbscode) %>%
  separate(newdbs_code1, into = c("serailn", "line_no"), sep = "/")

##Ensure line number is numeric to facilitate merging
KNHH_kids_malhx_dry <- KNHH_kids_malhx_dry %>%
  mutate(line_no = as.numeric(line_no))

KNHH_data_rdt_dry <- KNHH_data_rdt_dry %>%
  mutate(line_no = as.numeric(line_no))

##Merge dataset to filter out those that had a test carried out
# KNHH_kidsmal_rdt_data_dry <- KNHH_kids_malhx_dry %>%
#   inner_join(KNHH_data_rdt_dry, by = c("sn", "line_no"))

KNHH_kidsmal_rdt_data_dry <- KNHH_kids_malhx_dry %>%
  filter(!is.na(line_no)) %>%
  inner_join(KNHH_data_rdt_dry %>% filter(!is.na(line_no)), by = c("sn", "line_no"))

#Remove column with identifiers and household heads(line_no 1), then  Clean column names
KNHH_kidsmal_rdt_data_dry <- KNHH_kidsmal_rdt_data_dry %>%
  dplyr::filter(line_no != 1) %>%
  dplyr::select(-c(q404a, redcap_repeat_instrument.x, redcap_repeat_instance.x)) %>%
  janitor::clean_names()





##Read in Kano Household data with rdt results(Wet Season)
KNHH_data_rdt_wet <- read_dta(file.path(KNHHSrawdir, "Kano Wet Season Data Sept. 2024/KN wet season hhold  RDT results_290924.dta"))

KNHH_data_rdt_wet <- read_dta(file.path(my_backupdir, "Kano Wet Season Data Sept. 2024/KN wet season hhold  RDT results_290924.dta"))

# #rename column name to facilitate merge
# KNHH_data_rdt_wet <- KNHH_data_rdt_wet %>% 
#   rename(line_no = ln)

##Read in dataset for kids who had fever or suspected malaria
KNHH_kids_malhx_wet <- read_dta(file.path(KNHHSrawdir, "Kano Wet Season Data Sept. 2024/KN Wet season women malaria hx_280924.dta"))

KNHH_kids_malhx_wet <- read_dta(file.path(my_backupdir, "Kano Wet Season Data Sept. 2024/KN Wet season women malaria hx_280924.dta"))


#rename column name to facilitate merge
KNHH_kids_malhx_wet <- KNHH_kids_malhx_wet %>% 
  rename(line_no = q404b)

##Clean line numbers with leading zeros
KNHH_kids_malhx_wet$line_no <- sub("^0+", "", KNHH_kids_malhx_wet$line_no )


##Split dbs code to generate line no on rdt dataset
KNHH_data_rdt_wet <- KNHH_data_rdt_wet %>%
  mutate(newdbs_code1 = q304) %>%
  separate(newdbs_code1, into = c("serailn", "line_no"), sep = "/")



##Ensure line number is numeric to facilitate merging
KNHH_kids_malhx_wet <- KNHH_kids_malhx_wet %>%
  mutate(line_no = as.numeric(line_no))

KNHH_data_rdt_wet <- KNHH_data_rdt_wet %>%
  mutate(line_no = as.numeric(line_no))

##Merge dataset to filter out those that had a test carried out
KNHH_kidsmal_rdt_data_wet <- KNHH_kids_malhx_wet %>%
  filter(!is.na(line_no)) %>%
  inner_join(KNHH_data_rdt_wet %>% filter(!is.na(line_no)), by = c("sn", "line_no"))

#Remove column with identifiers and household heads(line_no 1), and then Clean column names
KNHH_kidsmal_rdt_data_wet <- KNHH_kidsmal_rdt_data_wet %>%
  dplyr::filter(line_no != 1) %>%
  dplyr::select(-c(q404a, redcap_repeat_instrument.x, redcap_repeat_instance.x))%>%
  janitor::clean_names()



##Generate labels for ease of understanding the datasets
##Dry
labels_KNHH_kidsmal_rdt_data_dry <- data.frame(
  variable = names(KNHH_kidsmal_rdt_data_dry),
  label = map_chr(names(KNHH_kidsmal_rdt_data_dry), function(v) {
    lbl <- attr(KNHH_kidsmal_rdt_data_dry[[v]], "label")
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
labels_KNHH_kidsmal_rdt_data_wet <- data.frame(
  variable = names(KNHH_kidsmal_rdt_data_wet),
  label = map_chr(names(KNHH_kidsmal_rdt_data_wet), function(v) {
    lbl <- attr(KNHH_kidsmal_rdt_data_wet[[v]], "label")
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

write_dta(KNHH_kidsmal_rdt_data_wet, file.path(KNHHSrawdir, "mrpt_analysis data/KNHH_kidsmal_data_wet.dta"))

write_dta(KNHH_kidsmal_rdt_data_dry, file.path(KNHHSrawdir, "mrpt_analysis data/KNHH_kidsmal_data_dry.dta"))


##Write labels as code book into drop box
write.csv(labels_KNHH_kidsmal_rdt_data_dry , file.path(KNHHSrawdir, "mrpt_analysis data//KN_kidscodebook_dry.csv"))
write.csv(labels_KNHH_kidsmal_rdt_data_wet, file.path(KNHHSrawdir, "mrpt_analysis data//KN_kidscodebook_wet.csv"))





##Prevalence analysis(2-10)
KN_LongBase <- read.csv(file.path("C:/Users/ebamgboye/Urban Malaria Proj Dropbox/urban_malaria/data/nigeria/kano_ibadan_epi/Latest Longitudinal Survey Data_May 2025/Re_ Kano longitudinal data/KNLS_baselineall.csv"))

KN_LongFFwup <- read.csv(file.path("C:/Users/ebamgboye/Urban Malaria Proj Dropbox/urban_malaria/data/nigeria/kano_ibadan_epi/Latest Longitudinal Survey Data_May 2025/Re_ Kano longitudinal data/KNLS_ffwupall.csv"))

                                   
##Update followup data with some background information columns(age, ward and settlement)
KN_LongFFwup <- KN_LongFFwup %>%
  left_join(select(KN_LongBase, sn, bi2, bi3, age_years), by = "sn")



#Overall monthly prevalence computation

##Recode test result to 1(Positive) and 0(Negative)
KN_LongFFwup_prev <- KN_LongFFwup %>%
  mutate(across(starts_with("q705_follow_up_"), ~case_when(
    .x == 1 ~ 1,
    .x == 2 ~ 0,
    TRUE ~ NA_real_
  )))

#Estimate monthly prevalence
monthly_prev <- KN_LongFFwup_prev %>%
  summarise(across(starts_with("q705_"), ~mean(.x, na.rm = TRUE)))

##Compute geometric mean of prevalence
library(psych)
gm <- geometric.mean(monthly_prev[monthly_prev > 0])  

##Visualize Prevalence
monthly_prev_long <- monthly_prev %>%
  pivot_longer(cols = everything(), names_to = "Month", values_to = "Prevalence") %>%
  mutate(MonthNum = as.numeric(gsub("q705_follow_up_|_arm_1", "", Month))) %>%
  arrange(MonthNum)

ggplot(monthly_prev_long, aes(x = MonthNum, y = Prevalence)) +
  geom_line(color = "darkred", size = 1.2) +
  geom_point(color = "black", size = 2) +
  geom_hline(yintercept = gm, linetype = "dashed", color = "blue", size = 1) +# Add geom_hline here
  #facet_wrap(~ bi2)+
  annotate("text", x = 10, y = gm, label = paste0("Geometric Mean = ", round(gm, 4)),
           vjust = -1, color = "blue", size = 4) +                               # Add annotation label
  scale_x_continuous(breaks = 1:12, labels = paste0("Month ", 1:12)) +
  labs(title = "Monthly Malaria Prevalence Trend for kids 0-10yrs(Kano)",
       x = "Month of Follow-up",
       y = "Prevalence (Proportion Positive)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))




##Children aged 2-10 years only
KNLSbasekids_2_10 <- KN_LongFFwup_prev %>%
  dplyr::filter(age_years >= 2 & age_years <= 10)

#Estimate monthly prevalence
KN_monthly_prev_2_10 <- KNLSbasekids_2_10 %>%
  #group_by(bi3) %>%  
  summarise(across(starts_with("q705_"), ~mean(.x, na.rm = TRUE)))

##Compute geometric mean of prevalence
KN_gm_2_10 <- geometric.mean(KN_monthly_prev_2_10[KN_monthly_prev_2_10 > 0])  

##Convert to long for visualization
KN_monthly_prev_long_2_10 <- KN_monthly_prev_2_10 %>%
  pivot_longer(cols = starts_with("q705_follow_up"), 
               names_to = "Month", values_to = "Prevalence") %>%
  mutate(MonthNum = as.numeric(gsub("q705_follow_up_|_arm_1", "", Month))) %>%
  arrange(MonthNum)


##Compute geometric mean by settlement type
gm_settlement <- KN_monthly_prev_long_2_10 %>%
  group_by(bi3) %>%
  summarise(gm = geometric.mean(Prevalence[Prevalence > 0])) # exclude zeros for geometric mean

##Add geometric mean to data by settlement type
KN_monthly_prev_long_2_10gm <- KN_monthly_prev_long_2_10 %>%
  left_join(gm_settlement, by = "bi3")

##Recode settlement type before plotting
KN_monthly_prev_long_2_10gm <- KN_monthly_prev_long_2_10gm %>%
  mutate(SettlementType = recode(bi3,
                                 `1` = "Formal",
                                 `2` = "Informal"
                                 ))

gm_settlement <- gm_settlement %>%
  mutate(SettlementType = recode(bi3,
                                 `1` = "Formal",
                                 `2` = "Informal"
                                 ))


ggplot(KN_monthly_prev_long_2_10gm, aes(x = MonthNum, y = Prevalence)) +
  geom_line(color = "darkred", size = 1.2) +
  geom_point(color = "black", size = 2) +
  geom_hline(aes(yintercept = gm), linetype = "dashed", color = "blue", size = 1) +
  facet_wrap(~ SettlementType) +
  geom_text(data = gm_settlement, aes(x = 10, y = gm, label = paste0("Geometric Mean = ", round(gm, 4))),
            color = "red", size = 4, hjust = 0.4, vjust = -4, inherit.aes = FALSE)+
  scale_x_continuous(breaks = 1:12, labels = paste0("Month ", 1:12)) +
  labs(title = "Monthly Malaria Prevalence Trend kids 2-10yrs(Kano)",
       x = "Month of Follow-up",
       y = "Prevalence (Proportion Positive)") +
  theme_minimal() #+
#theme(axis.text.x = element_text(angle = 45, hjust = 1))


##Combine Ibadan and Kano plots
##Select necessary variables for plotting
KN_monthly_prev_long_2_10c <- KN_monthly_prev_long_2_10 %>% 
  dplyr::select(Month, Prevalence, MonthNum)

IB_monthly_prev_long_2_10c <- IB_monthly_prev_long_2_10 %>% 
  dplyr::select(Month, Prevalence, MonthNum)

##Add states
KN_monthly_prev_long_2_10c$state <- "Kano"

IB_monthly_prev_long_2_10c$state <- "Ibadan"

##Combine data sets and geometric means
KNIB_monthlyprev_2_10 <- bind_rows(IB_monthly_prev_long_2_10c, KN_monthly_prev_long_2_10c)

gm_combined <- bind_rows(
  tibble(gm = IB_gm_2_10, state = "Ibadan"),
  tibble(gm = KN_gm_2_10, state = "Kano")
)



##Make plots
ggplot(KNIB_monthlyprev_2_10, aes(x = MonthNum, y = Prevalence, color = state, linetype = state, group = state)) +
  geom_line(size = 1.5) +
  geom_point(size = 2.5) +
  geom_hline(data = gm_combined, aes(yintercept = gm, color = state, linetype = state), size = 0.8, inherit.aes = FALSE) +
  geom_text(data = gm_combined, 
            aes(x = 10, y = gm, label = paste0("Geometric Mean = ", round(gm, 4)), color = state),
            size = 3.5, vjust = -1, inherit.aes = FALSE) +
  scale_x_continuous(breaks = 1:12, labels = paste0("Month ", 1:12)) +
  scale_color_manual(values = c(Ibadan = "navyblue", Kano = "seagreen"))+
  labs(title = "Monthly Malaria Prevalence Trend in kids 2-10yrs i Ibadan and Kano",
       x = "Month of Follow-up",
       y = "Prevalence (Proportion Positive)",) +
  theme_manuscript()










































##More data wrangling to combine the fever cases with entire household..
##Household data

#Read in Long format of Household data (Dry Season)
KNHH_longdata_dry <- read_dta(file.path(KNHSdatadir, "Dry Season Data/Long Data/kano_dryseason_long_data.dta"))

KNHHIPdata_dry <- KNHH_longdata_dry %>% 
  dplyr:: select(sn,bi1, bi2, bi2ii, bi3, hl1, hl2, hl3, hl4, hl5, hl6, hl7, hl8, hl9, q300, q301, q302, q303, q304, q305, new_dbscode, line_number00, age_calc, unique_id)     


KNHHIPdata_dry$Fever <- "Not Known"

KNHHIPdata_dry <- KNHHIPdata_dry %>% 
  mutate(line_no = line_number00)

##Extract the individuals with Fever History
KNHH_fever_rdt_data_dry <- KNHH_kidsmal_rdt_data_dry %>% 
  dplyr:: select(sn,line_no)

KNHH_fever_rdt_data_dry$Fever <- "Yes"


##Combine datasets
KNHH_combined_dry <- KNHHIPdata_dry %>%
  full_join(KNHH_fever_rdt_data_dry, by = c("sn", "line_no"))


KNHH_combined_dry <- KNHH_combined_dry %>%
  mutate(Fever.y = ifelse(is.na(Fever.y), "Not Known", Fever.y))

##Exclude Household heads since fever status was asked about children in the household
KNHH_combined_others_dry <- KNHH_combined_dry %>%
  dplyr::filter(line_no != 1) 

##Exclude women since they answered on behalf of the children in the household
KNHH_combined_1and2_dry <- KNHH_combined_dry %>%
  dplyr::filter(line_no != 2)




