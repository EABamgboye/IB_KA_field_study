LuDataDir <- file.path("C:/Users/ebamgboye/Urban Malaria Proj Dropbox/urban_malaria/data/nigeria/kano_ibadan_epi")
DellDataDir <- file.path("C:/Users/DELL/Urban Malaria Proj Dropbox/urban_malaria/data/nigeria/kano_ibadan_epi")

# Load necessary libraries
library(haven)
library(dplyr)
library(ggplot2)
library(Hmisc) 
library(survey)
library(forcats)


##TPR Analysis(Ibadan)
##A) TPR by Age, Settlement,Gender, Season
##B) TPR by Neighbourhood features
##C) TPR by Wealth Quintile 
##D) TPR by ITN ownership and use


##Initial Data Exploration
##TPR by age-group
tpr_age <- table(ibhhdata_combined$age_group, ibhhdata_combined$q302)

# Drop category 3 in q302
tpr_age_no3 <- tpr_age[, colnames(tpr_age) != "3"]

# Row-wise percentages (each row sums to 100)
percent_tpr_age <- prop.table(tpr_age_no3, margin = 1) * 100

# Round for readability
round(percent_tpr_age, 2)

##Weighted Exploration
# Define survey design
idesign <- svydesign(ids = ~1, data = ibhhdata_combined, weights = ~overall_hh_weight)

# Get weighted counts
svytable(~q302, idesign)

# Get weighted percentages
prop.table(svytable(~q302, idesign)) * 100

# Create the cross-tab for tpr by season
tpr_season <- table(ibhhdata_combined$q302, ibhhdata_combined$season)

# Exclude category 3
tpr_season_no3 <- tpr_season[rownames(tpr_season) != "3", ]

# Convert to proportions **by column (season)**
prop_no3 <- prop.table(tpr_season_no3, margin = 2) * 100  # margin = 2 => column-wise

# Round for readability
round(prop_no3, 3)


#Data wrangling
##Make preliminary visualizations for TPR distributions
tpr_df <- ibhhdata_combined %>%
  # Exclude category 3
  filter(q302 != 3) %>%
  # Count occurrences per age_group, season, settlement_type, and q302
  group_by(sn, overall_hh_weight, season, settlement, age_group, hl4, q302) %>%
  summarise(n = n(), .groups = "drop") %>%
  # Calculate row-wise percentages within each combination of season, settlement_type, age_group
  group_by(season, settlement, age_group) %>%
  mutate(percent = n / sum(n) * 100) %>%
  ungroup() %>%
  mutate(percent = round(percent, 2))

# Inspect the dataframe
tpr_df

#Reorder age_group
tpr_df <- tpr_df %>%
  mutate(age_group = factor(
    age_group,
    levels = c("0–5", "6–10", "11–17", "18–30", "30+"),
    ordered = TRUE
  ))

##Label variables
tpr_df <- tpr_df %>%
  mutate(
    settlement = as_factor(settlement),  # 1 → "Formal", 2 → "Informal", 3 → "Slum"
    hl4        = as_factor(hl4),         # 1 → "Male", 2 → "Female"
    q302       = as_factor(q302)         # 1 → "POSITIVE", 2 → "NEGATIVE"
  )


#Make plot
ggplot(tpr_df, aes(x = age_group, y = percent, fill = factor(q302))) +
  geom_col(position = "stack") +
  facet_grid(hl4 ~ season ~ settlement ) +
  labs(x = "Age Group", y = "Percent", fill = "q302 Category") +
  theme_minimal()

##Manipuate plot to focus on only positives
# Calculate percent POSITIVE and 95% CI
positives_df <- tpr_df %>%
  group_by(season, settlement, age_group, hl4) %>%
  summarise(
    n_positive = sum(n[q302 == "POSITIVE"]),
    n_total = sum(n),
    .groups = "drop"
  ) %>%
  mutate(
    percent = n_positive / n_total * 100,
    lower = Hmisc::binconf(n_positive, n_total, method = "wilson")[, 2] * 100,
    upper = Hmisc::binconf(n_positive, n_total, method = "wilson")[, 3] * 100
  )


#Make plot
ggplot(positives_df, aes(x = age_group, y = percent, color = settlement, group = settlement)) +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2,
                position = position_dodge(width = 0.5)) +
  facet_grid(hl4 ~ season) +
  labs(x = "Age Group", y = "Percent POSITIVE", color = "Settlement Type") +
  theme_minimal()

#Make another plot
ggplot(positives_df, aes(x = age_group, y = percent, color = settlement, group = settlement)) +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2,
                position = position_dodge(width = 0.5)) +
  facet_grid(~ season) +
  labs(x = "Age Group", y = "Percent POSITIVE", color = "Settlement Type") +
  theme_minimal()


##Fine tune plot
ggplot(positives_df, aes(x = age_group, y = percent, color = settlement, shape = hl4)) +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  geom_errorbar(aes(ymin = lower, ymax = upper),
                width = 0.2,
                position = position_dodge(width = 0.5)) +
  facet_grid(~ season) +
  labs(
    x = "Age Group",
    y = "Percent POSITIVE",
    color = "Settlement Type",  # for color legend
    shape = "Gender",               # for shape legend
    title = "TPR Positives by Settlement, Gender and Season(Ibadan)"
  ) +
  scale_color_manual(
    name = "Settlement Type",          # legend title
    values = c("1. Formal" = "#1f77b4", "2. Informal" = "plum", "3. Slum" = "brown"
    )
  ) +
  scale_shape_manual(values = c(Male = 16, Female = 17)) + # optional: customize shapes
  theme_manuscript()


##Let's Rerun using household weights

# Recode outcome variable ---
tpr_df <- tpr_df %>%
  mutate(
    pos = ifelse(q302 == "POSITIVE", 1, 
                 ifelse(q302 == "NEGATIVE", 0, NA))  # Ensure NA for any unexpected value
  )


# Define survey design ---
ib_design <- svydesign(
  ids = ~1,                 # no clusters 
  weights = ~overall_hh_weight,     
  data = tpr_df
)

# Compute weighted TPR ---
ib_svy_tpr <- svyby(
  ~pos,
  by = ~season + settlement + age_group + hl4,
  design = ib_design,
  FUN = svymean,
  vartype = c("ci")
)

# Clean and format output ---
positives_df <- ib_svy_tpr %>%
  rename(
    percent = pos,
    lower = ci_l,
    upper = ci_u
  ) %>%
  mutate(
    percent = percent * 100,
    lower = lower * 100,
    upper = upper * 100
  ) %>%
  select(season, settlement, age_group, hl4, percent, lower, upper)

# View results 
print(positives_df)


##New Plot for Weighted Analysis

# --- Prepare data for plotting ---
positives_df <- positives_df %>%
  mutate(
    settlement = factor(settlement,
                        levels = c("1. Formal", "2. Informal", "3. Slum"),
                        labels = c("Formal", "Informal", "Slum")
    )
  )


# --- Plot ---
ggplot(positives_df, aes(x = age_group, y = percent, color = settlement, shape = hl4)) +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  geom_errorbar(aes(ymin = lower, ymax = upper),
                width = 0.2,
                position = position_dodge(width = 0.5)) +
  facet_grid(~ season) +
  labs(
    x = "Age Group",
    y = "Percent POSITIVE (Weighted)",
    color = "Settlement Type",
    shape = "Sex",
    title = "Weighted Test Positivity Rate (TPR) by Settlement, Gender, and Season (Ibadan)"
  ) +
  scale_color_manual(
    name = "Settlement Type",
    values = c("Formal" = "#1f77b4",
               "Informal" = "plum",
               "Slum" = "brown")
  ) +
  scale_shape_manual(
    name = "Sex",
    values = c("Male" = 16, "Female" = 17)
  ) +
  theme_manuscript()


##TPR by Neighbouurhood characteristics
###Recode neigbhourhood variables to numeric
household_vars_to_convert <- c("q116", "q204", "q205", "q206", "q207",
                               "q208", "q209", "q211")  

# Convert them all to numeric
ibhhdata_combined <- ibhhdata_combined %>%
  mutate(across(all_of(household_vars_to_convert), ~ as.integer(.), .names = "{.col}_num"))

# Check result
head(ibhhdata_combined[, paste0(household_vars_to_convert, "_num")])


# Create dataframe for 
#presence of dirty environmnet 
tpr_dirty_df <- ibhhdata_combined %>%
  filter(q302 != 3, q116 !=3) %>%
  group_by(q116, settlement, q302) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(q116) %>%
  mutate(percent = n / sum(n) * 100)

tpr_dirty_df <- tpr_dirty_df %>%
  rename(FeaturePresent = q116) %>%
  mutate(Source = "Dirty Environment")

#presence of bush 
tpr_bush_df <- ibhhdata_combined %>%
  filter(q302 != 3) %>%
  group_by(q204, settlement, q302) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(q204) %>%
  mutate(percent = n / sum(n) * 100)

tpr_bush_df <- tpr_bush_df %>%
  rename(FeaturePresent = q204) %>%
  mutate(Source = "Bush")

##presence of dumpsites
tpr_dump_df <- ibhhdata_combined %>%
  filter(q302 != 3) %>%
  group_by(q205, settlement, q302) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(q205) %>%
  mutate(percent = n / sum(n) * 100)

tpr_dump_df <- tpr_dump_df %>%
  rename(FeaturePresent = q205) %>%
  mutate(Source = "Dumpsites")

#presence of stagnant H20
tpr_stagnant_df <- ibhhdata_combined %>%
  filter(q302 != 3) %>%
  group_by(q206, settlement, q302) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(q206) %>%
  mutate(percent = n / sum(n) * 100)

tpr_stagnant_df <- tpr_stagnant_df %>%
  rename(FeaturePresent = q206) %>%
  mutate(Source = "Stagnant Water")

#presence of vessels that can store water
tpr_vessel_df <- ibhhdata_combined %>%
  filter(q302 != 3) %>%
  group_by(q207, settlement, q302) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(q207) %>%
  mutate(percent = n / sum(n) * 100)

tpr_vessel_df <- tpr_vessel_df %>%
  rename(FeaturePresent = q207) %>%
  mutate(Source = "Open Vessels")

##Presence of Overgrown Vegetation
tpr_overg_df <- ibhhdata_combined %>%
  filter(q302 != 3) %>%
  group_by(q208, settlement, q302) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(q208) %>%
  mutate(percent = n / sum(n) * 100)

tpr_overg_df <- tpr_overg_df %>%
  rename(FeaturePresent = q208) %>%
  mutate(Source = "Overgrown Vegetation")

##Presence of open drainage
tpr_open_df <- ibhhdata_combined %>%
  filter(q302 != 3) %>%
  group_by(q209, settlement, q302) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(q209) %>%
  mutate(percent = n / sum(n) * 100)

tpr_open_df <- tpr_open_df %>%
  rename(FeaturePresent = q209) %>%
  mutate(Source = "Open Drainages")


##Have a garden or Farm
tpr_garden_df <- ibhhdata_combined %>%
  filter(q302 != 3) %>%
  group_by(q211, settlement, q302) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(q211) %>%
  mutate(percent = n / sum(n) * 100)

tpr_garden_df <- tpr_garden_df %>%
  rename(FeaturePresent = q211) %>%
  mutate(Source = "Garden for farming")

neighbour_tpr_df <- bind_rows(tpr_bush_df, tpr_dump_df, tpr_stagnant_df, tpr_dirty_df,
                              tpr_vessel_df, tpr_overg_df, tpr_open_df, tpr_garden_df)

##Remove NAs
neighbour_tpr_df <- neighbour_tpr_df %>%
  filter(!is.na(FeaturePresent))


# Filter only positives
neighbour_tpr_pos <- neighbour_tpr_df %>%
  filter(q302 == "POSITIVE", 
         FeaturePresent == "1. Yes")

##Make plot
ggplot(neighbour_tpr_pos, aes(x = Source, y = percent, 
                              color = settlement, 
                              group = FeaturePresent)) +
  geom_point(aes(size = n), alpha = 0.7, position = position_dodge(width = 0.5)) +
  labs(
    x = "Neighbourhood Feature",
    y = "Percent Positive (TPR)",
    color = "Settlement Type",
    size = "No of Households",
    title = "TPR among individuals in households with these Neighbourhood Features (Ibadan)"
  ) +
  scale_color_manual(
    name = "Settlement Type",
    values = c(
      "1. Formal" = "#1f77b4", 
      "2. Informal" = "plum", 
      "3. Slum" = "brown"
    )
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "top"
  ) +
  theme_manuscript()



##Net Ownership and Household Use
# Prepare the data
net_df <- ibhhdata_combined %>%
  filter(q302 != 3) %>%  # exclude q302 == 3
  group_by(nh101a, settlement, q302) %>%
  summarise(count = n(), .groups = "drop") %>%  # compute counts
  group_by(nh101a, settlement) %>%
  mutate(percent = count / sum(count) * 100)    # compute percentages


##Recode variables 
net_df <- net_df %>%
  mutate(settlement = factor(recode(as.character(settlement),
                                    `1` = "Formal",
                                    `2` = "Informal",
                                    `3` = "Slum"),
                             levels = c("Formal", "Informal", "Slum")))
net_df <- net_df %>%
  mutate(
    nh101a = as_factor(nh101a),  # 1 → "Yes", 2 → "No"
    q302  = as_factor(q302)         # 1 → "POSITIVE", 2 → "NEGATIVE"
  )

# Plot with facet_wrap by settlement
ggplot(net_df, aes(x = factor(nh101a), y = count, fill = factor(q302))) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(round(percent, 1), "%")),
            position = position_stack(vjust = 0.5), size = 3) +
  facet_wrap(~ settlement) +
  labs(
    x = "Net Ownership",
    y = "Count",
    fill = "Malaria Test Result",
    title = "Malaria Positivity by Net Ownership and Settlement Type (Ibadan)"
  ) +
  scale_fill_manual(
    values = c("POSITIVE" = "lightyellow", "NEGATIVE" = "plum"),
    labels = c("1" = "Positive", "2" = "Negative")
  ) +
  theme_manuscript()


# Net Use
#Recode net use to 2 categories
# Recode nh105 (Frequency of use of net)
ibhhdata_combined$net_use_freq <- dplyr::case_when(
  ibhhdata_combined$nh105 %in% c(1) ~ "Ideal",
  ibhhdata_combined$nh105 %in% c(2,3,4) ~ "Not Ideal",
  TRUE ~ NA_character_
)

#Summarize over "someone slept under net last night"
netuse_df <- ibhhdata_combined %>%
  filter(q302 != 3) %>%  # exclude q302 == 3
  group_by(nh113, settlement, q302) %>%
  summarise(count = n(), .groups = "drop") %>%  # compute counts
  group_by(nh113, settlement) %>%
  mutate(percent = count / sum(count) * 100)  %>% # compute percentages
  filter(!is.na(nh113))   


netuse_df <- netuse_df %>% 
  rename(RDT_Result = q302)

# Plot with facet_wrap by settlement
ggplot(netuse_df, aes(x = factor(nh113), y = count, fill = factor(RDT_Result))) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(round(percent, 1), "%")),
            position = position_stack(vjust = 0.5), size = 3.0) +
  facet_wrap(~ settlement) +
  labs(
    x = "Pattern of Household Net Use",
    y = "Count",
    fill = "RDT_Result",
    title = "Distribution of TPR by household net use and settlement type"
  ) +
  scale_fill_manual(
    values = c("POSITIVE" = "lightyellow", "NEGATIVE" = "plum"),
    labels = c("1" = "Positive", "2" = "Negative")
  ) +
  theme_manuscript()

#Remake plot for household net use

# Filter for only positive RDT results
netuse_positive <- netuse_df %>%
  filter(RDT_Result == "POSITIVE")

# Plot
ggplot(netuse_positive, aes(x = factor(nh113), y = count, fill = factor(RDT_Result))) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(round(percent, 1), "%")),
            position = position_stack(vjust = 0.5), size = 3.0) +
  facet_wrap(~ settlement) +
  labs(
    x = "Pattern of Household Net Use",
    y = "Count",
    fill = "RDT_Result",
    title = "Distribution of Positive TPR by household net use and settlement type"
  ) +
  scale_fill_manual(
    values = c("POSITIVE" = "cyan3"),
    labels = c("POSITIVE" = "Positive")
  ) +
  theme_manuscript()




##Analyse Malaria positivity by wealth quintile
# Wealth quintile (Unweighted all)
wealth_df1 <- ibhhdata_combined %>%
  filter(q302 != 3) %>%  # exclude q302 == 3
  group_by(wealth_iqr_group, settlement, q302) %>%
  summarise(count = n(), .groups = "drop") %>%  # compute counts
  group_by(wealth_iqr_group, settlement) %>%
  mutate(percent = count / sum(count) * 100)  %>% # compute percentages
  filter(!is.na(wealth_iqr_group))   


wealth_df1 <- wealth_df1 %>%
  mutate(
    q302  = as_factor(q302)         # 1 → "POSITIVE", 2 → "NEGATIVE"
  )

# Plot with facet_wrap by settlement
ggplot(wealth_df1, aes(x = factor(wealth_iqr_group), y = count, fill = factor(q302))) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(round(percent, 1), "%")),
            position = position_stack(vjust = 0.5), size = 2.5) +
  facet_wrap(~ settlement) +
  labs(
    x = "wealth_iqr_group",
    y = "Count",
    fill = "q302",
    title = "Distribution of Malaria Positivity by Wealth Quintile and Settlement"
  ) +
  scale_fill_manual(
    values = c("POSITIVE" = "lightyellow", "NEGATIVE" = "plum"),
    labels = c("1" = "Positive", "2" = "Negative")
  ) +
  theme_manuscript()


##Compute Weighted TPR by Wealth quintile
# Prepare data ---
wealth_df <- ibhhdata_combined %>%
  # Keep only POSITIVE or NEGATIVE
  filter(q302 %in% c("POSITIVE", "NEGATIVE")) %>%
  mutate(
    pos = ifelse(q302 == "POSITIVE", 1, 0)  # binary outcome
  )


# Define survey design object ---
design_wealth <- svydesign(
  ids = ~1,                     # 
  weights = ~overall_hh_weight,        
  data = wealth_df
)

# Compute weighted TPR (mean of 'pos') ---
wealth_tpr <- svyby(
  ~pos,                         #
  ~wealth_iqr_group + settlement, # groups
  design_wealth,
  svymean,                      # computes weighted mean
  vartype = c("ci"),            # include confidence intervals
  na.rm = TRUE
)

# Clean up and rename columns for clarity ---
wealth_tpr <- wealth_tpr %>%
  rename(
    tpr = pos,
    lower = ci_l,
    upper = ci_u
  ) %>%
  mutate(
    tpr_percent = tpr * 100,
    lower_percent = lower * 100,
    upper_percent = upper * 100
  )

##Make new plot
tprwealth_p <- ggplot(wealth_tpr, aes(x = factor(wealth_iqr_group, 
                                                 levels = c("Low", "Middle", "High")), 
                                      y = tpr_percent, fill = factor(settlement))) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  geom_errorbar(aes(ymin = lower_percent, ymax = upper_percent),
                position = position_dodge(width = 0.8), width = 0.2) +
  geom_text(aes(label = paste0(round(tpr_percent, 1), "%")),
            position = position_dodge(width = 0.8), 
            vjust = -0.5, hjust = 1.2, size = 3) +
  facet_wrap(~ settlement, labeller = labeller(settlement = c(
    `1` = "1. Formal", `2` = "2. Informal", `3` = "3. Slum"
  ))) +
  scale_fill_manual(
    values = c("1. Formal" = "#66C2A5", "2. Informal" = "#FC8D62", "3. Slum" = "#8DA0CB"),
    labels = c("1" = "Formal", "2" = "Informal", "3" = "Slum")
  ) +
  labs(
    title = "Weighted Malaria Test Positivity Rate by Wealth Quintile and Settlement(Ibadan)",
    x = "Wealth Quintile",
    y = "Weighted TPR (%)",
    fill = "Settlement Type"
  ) +
  theme_manuscript()

ggsave(paste0(LuDir, '/plots/', Sys.Date(), 'TPR by Settlement Type and Wealthquintile.pdf'), tprwealth_p, width = 11, height = 6)



##------------------------------------------------------------------------------
##Kano
##------------------------------------------------------------------------------
##Data exploration
ktpr <- table(knhhdata_combined$q302)

# Exclude category 3
ktpr_no3 <- ktpr[names(ktpr) != "3"]

# Calculate percentages
kpercent_no3 <- prop.table(ktpr_no3) * 100

# Print as rounded percentages
round(kpercent_no3, 2)

##Exploration with survey weights
# Define survey design
kdesign <- svydesign(ids = ~1, data = knhhdata_combined, weights = ~overall_hh_weight)

# Get weighted counts
svytable(~q302, kdesign)

# Get weighted percentages
prop.table(svytable(~q302, kdesign)) * 100


# Create the cross-tab for tpr by season
ktpr_season <- table(knhhdata_combined$q302, knhhdata_combined$season)

# Exclude category 3
ktpr_season_no3 <- tpr_season[rownames(tpr_season) != "3", ]

# Convert to proportions **by column (season)**
kprop_no3 <- prop.table(ktpr_season_no3, margin = 2) * 100  # margin = 2 => column-wise

# Round for readability
round(kprop_no3, 3)

#More exploration 

##TPR by age-group
kntpr_age <- table(knhhdata_combined$age_group3, knhhdata_combined$q302, knhhdata_combined$season)

kntpr_age_df <- as.data.frame(kntpr_age)

# Drop category 3 in q302
kntpr_age_no3 <- kntpr_age[, colnames(kntpr_age) != "3"]

kntpr_age_no3 <- kntpr_age_no3[, c("1", "2")]

# Row-wise percentages (each row sums to 100)
knpercent_tpr_age <- prop.table(kntpr_age_no3, margin = 1) * 100

# Round for readability
round(knpercent_tpr_age, 2)



##Make preliminary visualizations for TPR distributions
kntpr_df <- knhhdata_combined %>%
  # Exclude category 3
  filter(q302 != 3) %>%
  # Count occurrences per age_group, season, settlement_type, and q302
  group_by(sn, season, bi3, age_group, hl4, q302, overall_hh_weight) %>%
  summarise(n = n(), .groups = "drop") %>%
  # Calculate row-wise percentages within each combination of season, settlement_type, age_group
  group_by(season, bi3, age_group) %>%
  mutate(percent = n / sum(n) * 100) %>%
  ungroup() %>%
  mutate(percent = round(percent, 2))

# Inspect the data frame
kntpr_df

##Combine Informal and Slum Settlements
kntpr_df <- kntpr_df %>%
  mutate(bi3 = if_else(as.numeric(as.character(bi3)) == 3, 2, as.numeric(as.character(bi3))))


#Reorder age_group
kntpr_df <- kntpr_df %>%
  mutate(age_group3 = factor(
    age_group,
    levels = c("0–5", "6–17", "18+"),
    ordered = TRUE
  ))

##Label vriables
kntpr_df <- kntpr_df %>%
  mutate(
    bi3 = as_factor(bi3),  # 1 → "Formal", 2 → "Informal"
    hl4        = as_factor(hl4),         # 1 → "Male", 2 → "Female"
    q302       = as_factor(q302)         # 1 → "POSITIVE", 2 → "NEGATIVE"
  )

##Label again
kntpr_df <- kntpr_df %>%
  mutate(
    bi3 = factor(bi3,
                 levels = c(1, 2),
                 labels = c("Formal", "Informal")),
    q302 = factor(q302,
                  levels = c(1, 2),
                  labels = c("Positive", "Negative"))
  )

#Make plot
ggplot(kntpr_df, aes(x = age_group, y = percent, fill = factor(q302))) +
  geom_col(position = "stack") +
  facet_grid(hl4 ~ season ~ bi3 ) +
  labs(x = "Age Group", y = "Percent", fill = "q302 Category") +
  theme_minimal()


#Run Test Positive Rates (Age group in 5 categories)
knpositives_df <- kntpr_df %>%
  filter(q302 %in% c("POSITIVE", "NEGATIVE")) %>%
  group_by(season, bi3, age_group, hl4, q302) %>%
  summarise(n = sum(n), .groups = "drop") %>%
  pivot_wider(names_from = q302, values_from = n, values_fill = 0) %>%
  mutate(
    n_positive = POSITIVE,
    n_total = POSITIVE + NEGATIVE
  ) %>%
  filter(!is.na(n_total), n_total > 0) %>%
  rowwise() %>%
  mutate(
    conf = list(
      if (!is.na(n_positive) && !is.na(n_total) && n_total > 0) {
        Hmisc::binconf(n_positive, n_total, alpha = 0.05, method = "wilson")
      } else {
        c(NA, NA, NA)
      }
    ),
    percent = (n_positive / n_total) * 100,
    lower = conf[[1]][2] * 100,
    upper = conf[[1]][3] * 100
  ) %>%
  ungroup() %>%
  select(-conf)

knpositives_df <- knpositives_df %>% 
  filter(!is.na(hl4))

##Let's Rerun using household weights

# Recode outcome variable ---
kntpr_df <- kntpr_df %>%
  mutate(
    pos = ifelse(q302 == 1, 1, 0)  # Binary indicator
  )

# Define survey design ---
kn_design <- svydesign(
  ids = ~1,                 # no clusters (update if you have PSU variable)
  weights = ~overall_hh_weight,     # replace with your actual weight column
  data = kntpr_df
)

# Compute weighted TPR ---
kn_svy_tpr <- svyby(
  ~pos,
  by = ~season + bi3 + age_group + hl4,
  design = kn_design,
  FUN = svymean,
  vartype = c("ci")
)

# Clean and format output ---
knpositives_df <- kn_svy_tpr %>%
  rename(
    percent = pos,
    lower = ci_l,
    upper = ci_u
  ) %>%
  mutate(
    percent = percent * 100,
    lower = lower * 100,
    upper = upper * 100
  ) %>%
  select(season, bi3, age_group, hl4, percent, lower, upper)

# --- 5. View results ---
print(knpositives_df)


#Make plot
ggplot(knpositives_df, aes(x = age_group, y = percent, color = bi3, group = bi3)) +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2,
                position = position_dodge(width = 0.5)) +
  facet_grid(hl4 ~ bi3) +
  labs(x = "Age Group", y = "Percent POSITIVE", color = "Settlement Type") +
  theme_minimal()

#Make another plot
ggplot(knpositives_df, aes(x = age_group, y = percent, color = bi3, group = bi3)) +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2,
                position = position_dodge(width = 0.5)) +
  facet_grid(~ season) +
  labs(x = "Age Group", y = "Percent POSITIVE", color = "Settlement Type") +
  theme_minimal()


##Fine tune plot
ggplot(knpositives_df, aes(x = age_group, y = percent, color = bi3, shape = hl4)) +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  geom_errorbar(aes(ymin = lower, ymax = upper),
                width = 0.2,
                position = position_dodge(width = 0.5)) +
  facet_grid(~ season) +
  labs(
    x = "Age Group",
    y = "Percent POSITIVE",
    color = "Settlement Type",  # for color legend
    shape = "Sex",               # for shape legend
    title = "TPR Positives by Settlement, Gender and Season(Kano)"
  ) +
  scale_color_manual(
    name = "Settlement Type",          # legend title
    values = c("Formal" = "#1f77b4", "Informal" = "plum", "Slum" = "brown"
    )
  ) +
  scale_shape_manual(values = c(Male = 16, Female = 17)) + # optional: customize shapes
  theme_manuscript()


##New Plot for Weighted Analysis
#Reorder age_group
knpositives_df <- knpositives_df %>%
  mutate(age_group = factor(
    age_group,
    levels = c("0–5", "6–10", "11–17", "18–30", "30+"),
    ordered = TRUE
  ))

# --- Plot ---
ggplot(knpositives_df, aes(x = age_group, y = percent, color = bi3, shape = hl4)) +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  geom_errorbar(aes(ymin = lower, ymax = upper),
                width = 0.2,
                position = position_dodge(width = 0.5)) +
  facet_grid(~ season) +
  labs(
    x = "Age Group",
    y = "Percent POSITIVE (Weighted)",
    color = "Settlement Type",
    shape = "Sex",
    title = "Weighted Test Positivity Rate (TPR) by Settlement, Gender, and Season (Kano)"
  ) +
  scale_color_manual(
    name = "Settlement Type",
    values = c("Formal" = "#1f77b4",
               "Informal" = "plum",
               "Slum" = "brown")
  ) +
  scale_shape_manual(
    name = "Sex",
    values = c("Male" = 16, "Female" = 17)
  ) +
  theme_manuscript()





##TPR by Neighbouurhood characteristics(Kano)
#Recode Settlement into 2 groups
knhhdata_combined <- knhhdata_combined %>%
  mutate(bi3 = if_else(as.numeric(as.character(bi3)) == 3, 2, as.numeric(as.character(bi3))))

# Create dataframe for 
kntpr_dirty_df <- knhhdata_combined %>%
  filter(q302 != 3, q116 !=3) %>%
  group_by(q116, bi3, q302) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(q116) %>%
  mutate(percent = n / sum(n) * 100)

kntpr_dirty_df <- kntpr_dirty_df %>%
  rename(FeaturePresent = q116) %>%
  mutate(Source = "Dirty Environment")

#presence of bush 
kntpr_bush_df <- knhhdata_combined %>%
  filter(q302 != 3) %>%
  group_by(q204, bi3, q302) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(q204) %>%
  mutate(percent = n / sum(n) * 100)

kntpr_bush_df <- kntpr_bush_df %>%
  rename(FeaturePresent = q204) %>%
  mutate(Source = "Bush")

##presence of dumpsites
kntpr_dump_df <- knhhdata_combined %>%
  filter(q302 != 3) %>%
  group_by(q205, bi3, q302) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(q205) %>%
  mutate(percent = n / sum(n) * 100)

kntpr_dump_df <- kntpr_dump_df %>%
  rename(FeaturePresent = q205) %>%
  mutate(Source = "Dumpsites")

#presence of stagnant H20
kntpr_stagnant_df <- knhhdata_combined %>%
  filter(q302 != 3) %>%
  group_by(q206, bi3, q302) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(q206) %>%
  mutate(percent = n / sum(n) * 100)

kntpr_stagnant_df <- kntpr_stagnant_df %>%
  rename(FeaturePresent = q206) %>%
  mutate(Source = "Stagnant Water")

#presence of vessels that can store water
kntpr_vessel_df <- knhhdata_combined %>%
  filter(q302 != 3) %>%
  group_by(q207, bi3, q302) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(q207) %>%
  mutate(percent = n / sum(n) * 100)

kntpr_vessel_df <- kntpr_vessel_df %>%
  rename(FeaturePresent = q207) %>%
  mutate(Source = "Open Vessels")

##Presence of Overgrown Vegetation
kntpr_overg_df <- knhhdata_combined %>%
  filter(q302 != 3) %>%
  group_by(q208, bi3, q302) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(q208) %>%
  mutate(percent = n / sum(n) * 100)

kntpr_overg_df <- kntpr_overg_df %>%
  rename(FeaturePresent = q208) %>%
  mutate(Source = "Overgrown Vegetation")

##Presence of open drainage
kntpr_open_df <- knhhdata_combined %>%
  filter(q302 != 3) %>%
  group_by(q209, bi3, q302) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(q209) %>%
  mutate(percent = n / sum(n) * 100)

kntpr_open_df <- kntpr_open_df %>%
  rename(FeaturePresent = q209) %>%
  mutate(Source = "Open Drainages")


##Have a garden or Farm
kntpr_garden_df <- knhhdata_combined %>%
  filter(q302 != 3) %>%
  group_by(q211, bi3, q302) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(q211) %>%
  mutate(percent = n / sum(n) * 100)

kntpr_garden_df <- kntpr_garden_df %>%
  rename(FeaturePresent = q211) %>%
  mutate(Source = "Garden for farming")

neighbour_tpr_kndf <- bind_rows(kntpr_bush_df, kntpr_dump_df, kntpr_stagnant_df, kntpr_dirty_df,
                                kntpr_vessel_df, kntpr_overg_df, kntpr_open_df, kntpr_garden_df)

##Remove NAs
neighbour_tpr_kndf <- neighbour_tpr_kndf %>%
  filter(!is.na(FeaturePresent))


# Convert to factor with labels
neighbour_tpr_kndf <- neighbour_tpr_kndf %>%
  mutate(
    q302 = factor(as.character(q302), levels = c("1", "2"), labels = c("Positive", "Negative")),
    FeaturePresent = factor(as.character(FeaturePresent), levels = c("1", "2"), labels = c("Yes", "No")),
    bi3 = factor(as.character(bi3), levels = c("1","2","3"), labels = c("Formal", "Informal", "Slum"))
  )


# Filter only positives
neighbour_tpr_kndfpos <- neighbour_tpr_kndf %>%
  filter(q302 == "Positive", 
         FeaturePresent == "Yes")



###Make plot
ggplot(neighbour_tpr_kndfpos, aes(x = Source, y = percent, color = bi3, group = FeaturePresent)) +
  geom_point(alpha = 0.7, size = 3.5, position = position_dodge(width = 0.5)) +
  labs(
    x = "Neighbourhood Feature",
    y = "Percent Positive (TPR)",
    color = "Settlement Type",
    title = "TPR among individuals in households with these Neighbourhood Features (Kano)"
  ) +
  scale_color_manual(
    name = "Settlement Type",
    values = c(
      "Formal" = "#1f77b4", 
      "Informal" = "plum", 
      "Slum" = "brown"
    )
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 2.5),
        legend.position = "top") +
  theme_manuscript()




##Compute Weighted TPR by Wealth quintile

# Prepare data ---
kn_wealth_df <- knhhdata_combined %>%
  # Keep only valid test results (exclude missing or invalid)
  filter(q302 %in% c(1, 2)) %>%  # assuming 1 = POSITIVE, 2 = NEGATIVE
  mutate(
    pos = ifelse(q302 == 1, 1, 0)  # binary outcome
  )

# Define survey design object ---
design_kn_wealth <- svydesign(
  ids = ~1,                     # 
  weights = ~overall_hh_weight,         # 
  data = kn_wealth_df
)

#  Compute weighted TPR (mean of 'pos') ---
kn_wealth_tpr <- svyby(
  ~pos,                         # variable to summarize
  ~wealth_iqr_group + bi3, # groups
  design_kn_wealth,
  svymean,                      # computes weighted mean
  vartype = c("ci"),            # include confidence intervals
  na.rm = TRUE
)

# Clean up and rename columns for clarity ---
kn_wealth_tpr <- kn_wealth_tpr %>%
  rename(
    tpr = pos,
    lower = ci_l,
    upper = ci_u
  ) %>%
  mutate(
    tpr_percent = tpr * 100,
    lower_percent = lower * 100,
    upper_percent = upper * 100
  )

##Make new plot
wealth_tpr_kn <- ggplot(kn_wealth_tpr, aes(x = factor(wealth_iqr_group, 
                                                      levels = c("Low", "Middle", "High")), 
                                           y = tpr_percent, fill = factor(bi3))) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  geom_errorbar(aes(ymin = lower_percent, ymax = upper_percent),
                position = position_dodge(width = 0.8), width = 0.2) +
  geom_text(aes(label = paste0(round(tpr_percent, 1), "%")),
            position = position_dodge(width = 0.8), 
            vjust = -0.5, hjust = 1.2, size = 3) +
  facet_wrap(~ bi3, labeller = labeller(bi3 = c(
    `1` = "Formal", `2` = "Informal", `3` = "Slum"
  ))) +
  scale_fill_manual(
    values = c("1" = "#8DD3C7", "2" = "#FDB462", "3" = "#BEBADA"),
    labels = c("1" = "Formal", "2" = "Informal", "3" = "Slum")
  ) +
  labs(
    title = "Weighted Malaria Test Positivity Rate by Wealth Quintile and Settlement(Kano)",
    x = "Wealth Quintile",
    y = "Weighted TPR (%)",
    fill = "Settlement Type"
  ) +
  theme_manuscript()

ggsave(paste0(LuDir, '/plots/', Sys.Date(), 'Weighted Malaria TPR by Wealth Quintile and Settlement(Kano).pdf'), wealth_tpr_kn, width = 11, height = 8)


##Net Ownership and Household Use
# Prepare the data
##Summarize net ownership at household level
knnet_df <- knhhdata_combined %>%
  filter(q302 != 3) %>%  # exclude q302 == 3
  group_by(nh101a, bi3, q302) %>%
  summarise(count = n(), .groups = "drop") %>%  # compute counts
  group_by(nh101a, bi3) %>%
  mutate(percent = count / sum(count) * 100)    # compute percentages


##Recode variables 
knnet_df <- knnet_df %>%
  mutate(bi3 = factor(recode(as.character(bi3),
                             `1` = "Formal",
                             `2` = "Informal",
                             `3` = "Slum"),
                      levels = c("Formal", "Informal", "Slum")))

knnet_df <- knnet_df %>%
  mutate(
    nh101a = as_factor(nh101a),  # 1 → "Yes", 2 → "No"
    q302  = as_factor(q302)         # 1 → "POSITIVE", 2 → "NEGATIVE"
  )


##Remove NAs 
knnet_df <- knnet_df %>%
  filter(!is.na(nh101a))

#Recode settlement to 2 categories
library(forcats) 
knnet_df <- knnet_df %>% 
  mutate(
    bi3 = fct_collapse(bi3,
                       Informal = c("Informal", "Slum")
    ))

# Plot with facet_wrap by settlement
library(ggplot2)

ggplot(knnet_df, aes(x = factor(nh101a), y = count, fill = factor(q302))) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(round(percent, 1), "%")),
            position = position_stack(vjust = 0.5), size = 3) +
  facet_wrap(~ bi3) +
  labs(
    x = "Net Ownership",
    y = "Count",
    fill = "Malaria Test Result",
    title = "Malaria Positivity by Net Ownership and Settlement Type (Kano)"
  ) +
  scale_fill_manual(
    values = c("1" = "lightyellow", "2" = "plum"),
    labels = c("1" = "Positive", "2" = "Negative")
  ) +
  theme_manuscript()



#Summarize Net Use
knnetuse_df <- knhhdata_combined %>%
  filter(q302 != 3) %>%  # exclude q302 == 3
  group_by(nh113, settlement, q302) %>%
  summarise(count = n(), .groups = "drop") %>%  # compute counts
  group_by(nh113, settlement) %>%
  mutate(percent = count / sum(count) * 100)  %>% # compute percentages
  filter(!is.na(nh113))   


knnetuse_df <- knnetuse_df %>%
  mutate(
    nh113 = as_factor(nh113),  # 1 → "Formal", 2 → "Informal", 3 → "Slum"
    q302  = as_factor(q302)         # 1 → "POSITIVE", 2 → "NEGATIVE"
  )

knnetuse_df <- knnetuse_df %>% 
  rename(RDT_Result = q302)

# Plot with facet_wrap by settlement
ggplot(knnetuse_df, aes(x = factor(nh113), y = count, fill = factor(RDT_Result))) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(round(percent, 1), "%")),
            position = position_stack(vjust = 0.5), size = 3.0) +
  facet_wrap(~ settlement) +
  labs(
    x = "Pattern of Household Net Use",
    y = "Count",
    fill = "RDT_Result",
    title = "Distribution of TPR by household net use and settlement type(Kano)"
  ) +
  scale_fill_manual(
    values = c("1" = "lightyellow", "2" = "plum"),
    labels = c("1" = "Positive", "2" = "Negative")
  ) +
  theme_manuscript()

#Remake plot for household net use

# Filter for only positive RDT results
knnetuse_positive <- knnetuse_df %>%
  filter(RDT_Result == "1")

# Plot
ggplot(knnetuse_positive, aes(x = factor(nh113), y = count, fill = factor(nh113))) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(round(percent, 1), "%")),
            position = position_stack(vjust = 0.5), size = 3.0) +
  facet_wrap(~ settlement) +
  labs(
    x = "Pattern of Household Net Use",
    y = "Count",
    fill = "RDT_Result",
    title = "Distribution of Positive TPR by household net use and settlement type"
  ) +
  scale_fill_manual(
    values = c("Yes" = "lightgreen", "No" = "plum"),
  ) +
  theme_manuscript()

