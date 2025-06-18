#loadpath
user <- Sys.getenv("USERNAME")
Drive <- file.path(gsub("[\\]", "/", gsub("Documents", "", Sys.getenv("HOME"))))
LuDir <- file.path(Drive, "Documents")
LuPDir <- file.path(Drive, "Downloads")

# #load packages
# source(functions.R)
library(readxl)
library(corrplot)

#Read in shapefile
## read ibadan ward shape files
df_ib <- st_read(file.path(LuDir, "kano_ibadan_shape_files", "ibadan_metro_ward_fiveLGAs", "Ibadan_metro_fiveLGAs.shp")) %>%
  mutate(WardName = ifelse(WardName == 'Oranyan' & LGACode == '31007', 'Oranyan_7', WardName))

#Plot location of wards visited
pd <- ggplot(df_ib) +
  geom_sf(aes(fill = WardName), color = "black") +
  geom_text_repel(
    data = df_ib,
    aes(label =  WardName, geometry = geometry),color ='black',
    stat = "sf_coordinates", min.segment.length = 0, size = 3.5, force = 1)+
  scale_fill_manual(values = c(
    "Agugu" = "plum",  # Replace with actual ward names and desired colors
    "Challenge" = "coral",
    "Olopomewa" = "lightgreen"
  ), na.value = "white")+
  map_theme()+ 
  labs(title= "Wards in Ibadan visited for entomology study ")+
  coord_sf()

ggsave(paste0(LuDir, '/plots/', Sys.Date(), "/", 'ibadan ento study sites.pdf'), pd, width = 8, height = 6)

##Split Ibadan shape file into working wards
df_ib_c <- df_ib %>%
  dplyr::filter(WardName == 'Challenge')

df_ib_a <- df_ib %>%
  dplyr::filter(WardName == 'Agugu')

df_ib_o <- df_ib %>%
  dplyr::filter(WardName == 'Olopomewa')



##Read in dry season larva dataset
lav_df_jf <- read_excel(file.path(LuDir ,"Osun-excel", "Larva prospection January and Feb updated April 2023.xlsx"))

lav_df_m <- read_excel(file.path(LuDir ,"Osun-excel", "MARCH LARVA IBADAN AND KANO.xlsx"))

lav_df_dry <- rbind(lav_df_jf, lav_df_m) %>% 
  dplyr::filter(State == "Oyo")

lav_df_dry[44, 27] <- "No"

##Read in wet season larval dataset
lav_df_wet <- read_excel(file.path(LuPDir , "WET_SEASON_ENTO_COLLECTION_LARVAL_PROSPECTION_-_all_versions_-_labels_-_2024-08-12-21-21-06.xlsx"))

lav_df_wet  <- lav_df_wet  %>% 
  mutate(`Household Code/Number` = 1:272)

lav_df_wet  <- slice(lav_df_wet , -(1:2))

lav_df_wet  <- slice(lav_df_wet , -(6))

lav_df_wet  <- lav_df_wet  %>% 
  mutate(Anopheles_Caught = ifelse(`Number of Anopheles` > 0, "Yes", "No"))


##Breeding site analysis
##Dry Season
##Recode breeding sites to match
lav_df_dry <- lav_df_dry %>% 
  mutate(Breeding_Site_Recode = recode(`Breeding site`,
                                       "Abandoned well" = "Dug Well",
                                       "OpenDrain/Puddle" = "Open Drain/Puddles",
                                       "Tank" = "Open Tank",
                                       "Stream" = "Canal"))



# Summarize data by breeding site type
breeding_site_sum_dry <- lav_df_dry %>% 
  dplyr::filter(State=="Oyo") %>% 
  group_by(Breeding_Site_Recode) %>%  # Group by breeding site type
  summarize(
    SitesVisited = n(),  # Number of sites visited per type
  ) 

# Calculate percentages
breeding_site_sum_dry <- breeding_site_sum_dry %>%
  mutate(Percentage = SitesVisited / sum(SitesVisited) * 100,
         Label = paste0(SitesVisited, " (", round(Percentage, 1), "%)"))  # Combine count and percentage

#Number and type of breeding sites
bs_dry1 <- ggplot(data = breeding_site_sum_dry, aes(x = "", y = SitesVisited, fill = Breeding_Site_Recode)) +
  geom_col(width = 1) +
  coord_polar(theta = "y") +  # Convert bar chart to pie chart
  scale_fill_brewer(palette = "Pastel1")+
  geom_text(aes(label = Label), position = position_stack(vjust = 0.5), color = "black", size = 2.5) +  # Center text with both count and percentage
  theme_void() +  # Clean theme for pie charts
  theme(legend.position = "right") +  # Optional: Adjust legend
  ggtitle("Number and type of breeding sites visited in Ibadan, Jan-March, 2023")

ggsave(paste0(LuDir, '/plots/', Sys.Date(), 'dry season breeding sites visited in Ibadan2.pdf'), bs_dry1, width = 8, height = 6)

###New breeding site analysis(Two groups)
##Dry Season
##Recode breeding sites to match
lav_df_dry <- lav_df_dry %>% 
  mutate(Breeding_Site_Recode2 = recode(Breeding_Site_Recode,
                                       "Artificial Containers" = "Artificial",
                                        "Dug Well" = "Artificial",
                                        "Open Drain/Puddles" = "Artificial",
                                        "Open Tank" = "Artificial",
                                       "Tyre tracks" = "Artificial",
                                       "Tyres" = "Artificial",
                                       "Refuse /Sewage" = "Artificial", 
                                       "Drainage/Gutter/Ditch" = "Permanent",
                                       "Canal" = "Permanent"))



# Summarize data by breeding site type
breeding_site_sum_dry2 <- lav_df_dry %>% 
  dplyr::filter(State=="Oyo") %>% 
  group_by(Breeding_Site_Recode2) %>%  # Group by breeding site type
  summarize(
    SitesVisited = n(),  # Number of sites visited per type
  ) 

# Calculate percentages
breeding_site_sum_dry2 <- breeding_site_sum_dry2 %>%
  mutate(Percentage = SitesVisited / sum(SitesVisited) * 100,
         Label = paste0(SitesVisited, " (", round(Percentage, 1), "%)"))  # Combine count and percentage

#Number and type of breeding sites
bs_dry2 <- ggplot(data = breeding_site_sum_dry2, aes(x = "", y = SitesVisited, fill = Breeding_Site_Recode2)) +
  geom_col(width = 1) +
  coord_polar(theta = "y") +  # Convert bar chart to pie chart
  scale_fill_brewer(palette = "Pastel1")+
  geom_text(aes(label = Label), position = position_stack(vjust = 0.5), color = "black", size = 2.5) +  # Center text with both count and percentage
  theme_void() +  # Clean theme for pie charts
  theme(legend.position = "right") +  # Optional: Adjust legend
  ggtitle("Number and type of breeding sites visited in Ibadan, Jan-March, 2023")

ggsave(paste0(LuDir, '/plots/', Sys.Date(), 'dry season breeding sites visited in Ibadan2grps.pdf'), bs_dry2, width = 8, height = 6)


#Breeding site analysis
##Wet Season
##Fill missing entry
if (is.na(lav_df_wet[35, 8]) || lav_df_wet[35, 8] == "") {
  lav_df_wet[35, 8] <- "Puddles"
}

if (is.na(lav_df_wet[157, 8]) || lav_df_wet[157, 8] == "") {
  lav_df_wet[157, 8] <- "Gutter"
}

if (is.na(lav_df_wet[184, 6]) || lav_df_wet[184, 6] == "") {
  lav_df_wet[184, 6] <- "Slum"
}

lav_df_wet[163, 6] <- "Slum"

##Recode breeding sites to macth
lav_df_wet <- lav_df_wet %>% 
  mutate(Breeding_Site_Recode = recode(`Type of breeding site`,
                                       "Drainage" = "Drainage/Gutter/Ditch",
                                       "Gutter" = "Drainage/Gutter/Ditch",
                                       "Ditch" = "Drainage/Gutter/Ditch",
                                       "Earthen Pot" = "Artificial Containers",
                                       "Abandoned Well" = "Dug Well",
                                       "Protected Well" = "Dug Well",
                                       "Unprotected Well" = "Dug Well",
                                       "Tunnel" = "Canal",
                                       "Puddles" = "Open Drain/Puddles",
                                       "Pit" = "Open Drain/Puddles",
                                       "Plastic Bowls" = "Artificial Containers",
                                       "Tyre" = "Tyres",
                                       "Sewage" = "Refuse /Sewage"))

# Summarize data by breeding site type
breeding_site_sum_wet <- lav_df_wet %>% 
  filter(!is.na(`Breeding_Site_Recode`)) %>%
  group_by(`Breeding_Site_Recode`) %>%  # Group by breeding site type
  summarize(
    SitesVisited = n(),  # Number of sites visited per type
  ) 

# Calculate percentages
breeding_site_sum_wet <- breeding_site_sum_wet %>%
  mutate(Percentage = SitesVisited / sum(SitesVisited) * 100,
         Label = paste0(SitesVisited, " (", round(Percentage, 1), "%)"))  # Combine count and percentage

#Number and type of breeding sites
##Make plots
bs_wet1 <- ggplot(data = breeding_site_sum_wet, aes(x = "", y = SitesVisited, fill = `Breeding_Site_Recode`)) +
  geom_col(width = 1) +
  coord_polar(theta = "y") +  # Convert bar chart to pie chart
  scale_fill_brewer(palette = "Pastel1")+
  geom_text(aes(label = Label), position = position_stack(vjust = 0.5), color = "black", size = 2.5) +  # Center text with both count and percentage
  theme_void() +  # Clean theme for pie charts
  theme(legend.position = "right") +  # Optional: Adjust legend
  ggtitle("Number and type of breeding sites visited in Ibadan, July-August, 2024")

ggsave(paste0(LuDir, '/plots/', Sys.Date(), 'wet season breeding sites visited in Ibadan2.pdf'), bs_wet1, width = 8, height = 6)

##New Breeding site analysis(2 groups)
##Recode breeding sites to macth
lav_df_wet <- lav_df_wet %>% 
  mutate(Breeding_Site_Recode2 = recode(Breeding_Site_Recode,
                                        "Artificial Containers" = "Artificial",
                                        "Dug Well" = "Artificial",
                                        "Open Drain/Puddles" = "Artificial",
                                        "Open Tank" = "Artificial",
                                        "Tyre tracks" = "Artificial",
                                        "Tyres" = "Artificial",
                                        "Refuse /Sewage" = "Artificial", 
                                        "Drainage/Gutter/Ditch" = "Permanent",
                                        "Canal" = "Permanent"))

# Summarize data by breeding site type
breeding_site_sum_wet2 <- lav_df_wet %>% 
  filter(!is.na(`Breeding_Site_Recode`)) %>%
  group_by(`Breeding_Site_Recode2`) %>%  # Group by breeding site type
  summarize(
    SitesVisited = n(),  # Number of sites visited per type
  ) 

# Calculate percentages
breeding_site_sum_wet2 <- breeding_site_sum_wet2 %>%
  mutate(Percentage = SitesVisited / sum(SitesVisited) * 100,
         Label = paste0(SitesVisited, " (", round(Percentage, 1), "%)"))  # Combine count and percentage

#Number and type of breeding sites
##Make plots
bs_wet2 <- ggplot(data = breeding_site_sum_wet2, aes(x = "", y = SitesVisited, fill = `Breeding_Site_Recode2`)) +
  geom_col(width = 1) +
  coord_polar(theta = "y") +  # Convert bar chart to pie chart
  scale_fill_brewer(palette = "Pastel1")+
  geom_text(aes(label = Label), position = position_stack(vjust = 0.5), color = "black", size = 2.5) +  # Center text with both count and percentage
  theme_void() +  # Clean theme for pie charts
  theme(legend.position = "right") +  # Optional: Adjust legend
  ggtitle("Number and type of breeding sites visited in Ibadan, July-August, 2024")

ggsave(paste0(LuDir, '/plots/', Sys.Date(), 'wet season breeding sites visited in Ibadan2grps.pdf'), bs_wet2, width = 8, height = 6)


##Settlement Type Analysis
#Dry Season
# Summarize data by breeding site type
breeding_site_sum_dry_sett <- lav_df_dry %>% 
  filter(!is.na(`Breeding_Site_Recode`)) %>%
  group_by(`Settlement Type`,`Breeding_Site_Recode`) %>%  # Group by breeding site type
  summarize(
    SitesVisited = n(),  # Number of sites visited per type
  ) 

# Calculate percentages
breeding_site_sum_dry_sett <- breeding_site_sum_dry_sett %>%
  mutate(Percentage = SitesVisited / sum(SitesVisited) * 100,
         Label = paste0(SitesVisited, " (", round(Percentage, 1), "%)"))

breeding_site_sum_dry_sett$Season <- "Dry"

##Some data wrangling
breeding_site_summary_dry_settx <- b_site_ano_sum_dry_wide_sett %>%
  group_by(Breeding_Site_Recode, `Settlement Type`) %>%
  summarise(
    TotalSitesVisitedn0 = sum(Total_Sites))

breeding_site_summary_dry_sett0 <- b_site_ano_sum_dry_wide_sett %>%
  group_by(`Settlement Type`, Breeding_Site_Recode) %>%
  summarise(
    TotalSitesVisitedn0 = sum(Total_Sites))

# Step 1: Calculate the total number of sites per settlement type
total_sites_per_settlement_dry <- breeding_site_summary_dry_settx %>%
  group_by(Breeding_Site_Recode) %>%
  summarise(TotalSites = sum(TotalSitesVisitedn0))

# Step 2: Merge the total with the original data and compute the proportion
breeding_sites_with_proportion_dry <- breeding_site_summary_dry_sett0 %>%
  group_by(Breeding_Site_Recode, `Settlement Type`) %>%
  summarise(TotalSites = sum(TotalSitesVisitedn0), .groups = 'drop') %>%  # summing TotalSitesVisitedn0 per group
  left_join(total_sites_per_settlement_dry, by = "Breeding_Site_Recode") %>%        # Joining with total sites per settlement
  mutate(Proportion = (TotalSites.x / TotalSites.y) * 100)                   # Computing the proportion

Fig4a <- ggplot(breeding_sites_with_proportion_dry, aes(y = as.factor(Breeding_Site_Recode), x = Proportion, fill = `Settlement Type`)) + 
  geom_col(position = "stack") +   # Stack bars by Settlement Type
  scale_fill_manual(values = c(Formal = "#fe9c8f", Informal = "#fec8c1", Slum = "#f9caa7"))+
  geom_text(aes(label = round(Proportion, 1)), 
            position = position_stack(vjust = 0.5),   # Center labels within each stack
            size = 3.5) +
  ylab("Breeding Site") +   # y-axis is now Breeding Site
  xlab("Proportion") +
  theme_manuscript() +
  theme(
    axis.text.y = element_text(size = 10),  # Adjust y-axis text (Breeding Site names)
  )

ggsave(paste0(LuDir, '/plots/', Sys.Date(), 'Breeding sites visited in Ibadan by settlement(Dry).pdf'), Fig4a, width = 11, height = 6)




##Settlement Type Analysis
##Wet Season
# Summarize data by breeding site type
breeding_site_sum_wet_sett <- lav_df_wet %>% 
  filter(!is.na(`Breeding_Site_Recode`)) %>%
  group_by(`Settlement Type`,`Breeding_Site_Recode`) %>%  # Group by breeding site type
  summarize(
    SitesVisited = n(),  # Number of sites visited per type
  ) 

# Calculate percentages
breeding_site_sum_wet_sett <- breeding_site_sum_wet_sett %>%
  mutate(Percentage = SitesVisited / sum(SitesVisited) * 100,
         Label = paste0(SitesVisited, " (", round(Percentage, 1), "%)"))


##Some data wrangling
breeding_site_summary_wet_settx <- b_site_ano_sum_wet_wide_sett %>%
  group_by(Breeding_Site_Recode, `Settlement Type`) %>%
  summarise(
    TotalSitesVisitedn0 = sum(Total_Sites))

breeding_site_summary_wet_sett0 <- b_site_ano_sum_wet_wide_sett %>%
  group_by(`Settlement Type`, Breeding_Site_Recode) %>%
  summarise(
    TotalSitesVisitedn0 = sum(Total_Sites))

# Step 1: Calculate the total number of sites per settlement type
total_sites_per_settlement_wet <- breeding_site_summary_wet_settx %>%
  group_by(Breeding_Site_Recode) %>%
  summarise(TotalSites = sum(TotalSitesVisitedn0))

# Step 2: Merge the total with the original data and compute the proportion
breeding_sites_with_proportion_wet <- breeding_site_summary_wet_sett0 %>%
  group_by(Breeding_Site_Recode, `Settlement Type`) %>%
  summarise(TotalSites = sum(TotalSitesVisitedn0), .groups = 'drop') %>%  # summing TotalSitesVisitedn0 per group
  left_join(total_sites_per_settlement_wet, by = "Breeding_Site_Recode") %>%        # Joining with total sites per settlement
  mutate(Proportion = (TotalSites.x / TotalSites.y) * 100)                   # Computing the proportion

Fig4b <- ggplot(breeding_sites_with_proportion_wet, aes(y = as.factor(Breeding_Site_Recode), x = Proportion, fill = `Settlement Type`)) + 
  geom_col(position = "stack") +   # Stack bars by Settlement Type
  scale_fill_manual(values = c(Formal = "#fe9c8f", Informal = "#fec8c1", Slum = "#f9caa7"))+
  geom_text(aes(label = round(Proportion, 1)), 
            position = position_stack(vjust = 0.5),   # Center labels within each stack
            size = 3.5) +
  ylab("Breeding Site") +   # y-axis is now Breeding Site
  xlab("Proportion") +
  theme_manuscript() +
  theme(
    axis.text.y = element_text(size = 10),  # Adjust y-axis text (Breeding Site names)
  )

ggsave(paste0(LuDir, '/plots/', Sys.Date(), 'Breeding sites visited in Ibadan by settlement(Wet).pdf'), Fig4b, width = 11, height = 6)

##Combine breeding site summary
breeding_site_sum_all_sett <- rbind(breeding_site_sum_dry_sett, breeding_site_sum_wet_sett)

breeding_site_summary_all_sett <- breeding_site_sum_all_sett %>%
  group_by(`Settlement Type`) %>%
  summarise(
    TotalSitesVisited = sum(SitesVisited))

breeding_site_summary_all_sett0 <- breeding_site_sum_all_sett %>%
  group_by(`Settlement Type`, Breeding_Site_Recode) %>%
  summarise(
    TotalSitesVisitedn0 = sum(SitesVisited))

##Some data wrangling
breeding_site_summary_all_settx<- breeding_site_sum_all_sett %>%
  group_by(Breeding_Site_Recode, `Settlement Type`) %>%
  summarise(
    TotalSitesVisitedn0 = sum(SitesVisited))

# Step 1: Calculate the total number of sites per settlement type
total_sites_per_settlement <- breeding_site_summary_all_settx %>%
  group_by(Breeding_Site_Recode) %>%
  summarise(TotalSites = sum(TotalSitesVisitedn0))

# Step 2: Merge the total with the original data and compute the proportion
breeding_sites_with_proportion <- breeding_site_summary_all_sett0 %>%
  group_by(Breeding_Site_Recode, `Settlement Type`) %>%
  summarise(TotalSites = sum(TotalSitesVisitedn0), .groups = 'drop') %>%  # summing TotalSitesVisitedn0 per group
  left_join(total_sites_per_settlement, by = "Breeding_Site_Recode") %>%        # Joining with total sites per settlement
  mutate(Proportion = (TotalSites.x / TotalSites.y) * 100)                   # Computing the proportion


breeding_site_summary_all_sett1 <- breeding_site_sum_all_sett %>%
  group_by(`Settlement Type`, Season) %>%
  summarise(
    TotalSitesVisitedn = sum(SitesVisited))

breeding_site_summary_all_sett2 <- breeding_site_sum_all_sett %>%
  group_by(`Settlement Type`, Breeding_Site_Recode, Season) %>%
  summarise(
    TotalSitesVisitedn2 = sum(SitesVisited))

# Step 2: Calculate the overall total for all breeding sites
overall_total_sitesn <- sum(breeding_site_summary_all_sett$TotalSitesVisited)

# Step 3: Compute the percentage for each Breeding_Site_Recode
breeding_site_summary_all_sett <- breeding_site_summary_all_sett %>%
  mutate(
    OverallPercentage = (TotalSitesVisited / overall_total_sitesn) * 100,
    Label = paste(TotalSitesVisited, "(", round(OverallPercentage, 1), "%)", sep = "")
  )

Fig4 <- ggplot(breeding_sites_with_proportion, aes(y = as.factor(Breeding_Site_Recode), x = Proportion, fill = `Settlement Type`)) + 
  geom_col(position = "stack") +   # Stack bars by Settlement Type
  scale_fill_manual(values = c(Formal = "#fe9c8f", Informal = "#fec8c1", Slum = "#f9caa7"))+
  geom_text(aes(label = round(Proportion, 1)), 
            position = position_stack(vjust = 0.5),   # Center labels within each stack
            size = 3.5) +
  ylab("Breeding Site") +   # y-axis is now Breeding Site
  xlab("Proportion") +
  theme_manuscript() +
  theme(
    axis.text.y = element_text(size = 10),  # Adjust y-axis text (Breeding Site names)
  )

ggsave(paste0(LuDir, '/plots/', Sys.Date(), 'Breeding sites visited in Ibadan by settlement.pdf'), Fig4, width = 11, height = 6)



###Number and type of breeding sites anopheles was caught

##Dry Season
b_site_ano_sum_dry_sett <- lav_df_dry %>% 
  filter(!is.na(`Breeding_Site_Recode`)) %>% 
  group_by(`Settlement Type`, `Breeding_Site_Recode`, `Anopheles_Caught`) %>%  # Group by breeding site type
  summarize(
    SitesVisited = n(),  # Number of sites visited per type
  )

##Convert data to wide to compute proportion of breeding site with larva
b_site_ano_sum_dry_wide_sett <- b_site_ano_sum_dry_sett %>% 
  pivot_wider(names_from = Anopheles_Caught, values_from = SitesVisited, values_fill = 0) %>%
  mutate(
    Total_Sites = No + Yes, 
    Proportion_Larvae_Caught = Yes / Total_Sites * 100   # Proportion of sites with larvae caught
  )

b_site_ano_sum_dry_wide_sett$Season <- "Dry"

##Wet Season
b_site_ano_sum_wet_sett <- lav_df_wet %>% 
  filter(!is.na(`Breeding_Site_Recode`)) %>% 
  group_by(`Settlement Type`, `Breeding_Site_Recode`, `Anopheles_Caught`) %>%  # Group by breeding site type
  summarize(
    SitesVisited = n(),  # Number of sites visited per type
  )

##Convert data to wide to compute proportion of breeding site with larva
b_site_ano_sum_wet_wide_sett <- b_site_ano_sum_wet_sett %>% 
  pivot_wider(names_from = Anopheles_Caught, values_from = SitesVisited, values_fill = 0) %>%
  mutate(
    Total_Sites = No + Yes, 
    Proportion_Larvae_Caught = Yes / Total_Sites * 100   # Proportion of sites with larvae caught
  )

b_site_ano_sum_wet_wide_sett$Season <- "Wet"


##Combine Dry and Wet Season
b_site_ano_sum_all_wide_sett <- rbind(b_site_ano_sum_dry_wide_sett, b_site_ano_sum_wet_wide_sett)

b_site_ano_sum_all_wide_sett_y <- b_site_ano_sum_all_wide_sett %>% 
  dplyr::filter(Yes > 0, )


Fig7 <- ggplot(b_site_ano_sum_all_wide_sett_y, aes(x = Breeding_Site_Recode, y = Yes)) +
  geom_point(aes(color = `Settlement Type`, size = 4.5), , alpha = 0.7) +  
  facet_wrap(~ `Season`)+
  scale_color_manual(values = c(Formal = "#f57362", Slum = "#f9caa7"))+
  geom_text(aes(label = Yes), vjust = -1.2, hjust = 0.5, size = 3) + 
  geom_text(aes(label = Breeding_Site_Recode), vjust = -1.2, hjust = 0.5, size = 3) +
  scale_size_continuous(range = c(2, 10)) + 
  labs(title = "Number and type of Breeding Sites by settlement type ",
       y = "Number of Breeding sites",
       size = "Number of Breeding Sites Visited") +
  guides(size = FALSE)+
  theme_manuscript()

ggsave(paste0(LuDir, '/plots/', Sys.Date(), 'Breeding sites with anopheles by settlement.pdf'), Fig7, width = 8, height = 6)


Fig8 <- ggplot(b_site_ano_sum_all_wide_sett_y, aes(x = Breeding_Site_Recode, y = Proportion_Larvae_Caught, size = Total_Sites)) +
  geom_point(aes(color = `Settlement Type`, size = Total_Sites), , alpha = 0.7) +  
  facet_wrap(~ `Season`)+
  scale_color_manual(values = c(Formal = "#f57362", Slum = "#f9caa7"))+
  geom_text(aes(label = Breeding_Site_Recode), vjust = -1.2, hjust = 0.5, size = 3) + 
  scale_size_continuous(range = c(2, 10),
                        breaks = c(5, 10, 15, 20, 25, 30, 35, 40, 45, 75),     # Custom breaks for size legend
                        #labels = c("5 Sites", "10 Sites", "15 Sites", "20 Sites")  # Custom labels for the breaks
  ) + 
  labs(title = "Proportion of Anopheles Caught Across Breeding Sites",
       y = "Proportion of breeding site yielding Anopheles(%)",
       size = "Number of Breeding Sites Visited") +
  guides(
    size = guide_legend(
      title = "No of breeding sites visited",  # Customize the title of the size legend
      override.aes = list(shape = 21, fill = "#07c7f7"),  # Customize legend appearance
      keywidth = 1, keyheight = 1,   # Adjust size of the legend keys
      label.position = "bottom",     # Customize label positioning
      nrow = 3                       # Arrange legend in a single row
    )
  ) +
  theme_manuscript() 


ggsave(paste0(LuDir, '/plots/', Sys.Date(), 'Proportion of Breeding sites with anopheles by settlement1.pdf'), Fig8, width = 8, height = 6) 


##Larval Density Analysis
##Dry Season
lav_ib_dry <- lav_df_dry %>% 
  dplyr::filter(State=="Oyo")

subset_lav <- lav_ib_dry[lav_ib_dry$`Anopheles` > 0, ]

lav_den_sum <- subset_lav %>% 
  mutate(Larva_Density = `Anopheles`/`No of dips`)

# Recode Site Codes for better understanding
lav_den_sum <- lav_den_sum %>% 
  mutate(`Site Code` = case_when(
    `Site Code` ==  "1" ~ "1",
    `Site Code` == "6" ~ "6",
    `Site Code` == "IB/AG/14" ~ "14",
    `Site Code` == "IB/OL/10" ~ "10",
    `Site Code` == "IB/OL/20" ~ "20"
  ))

lav_den_sum$season <- "Dry"

##Estimating total anopheles
lav_den_sum %>%
  summarise(Total_Anopheles = sum(Anopheles, na.rm = TRUE))

##Wet Season
subset_lav_wet <- lav_df_wet[lav_df_wet$`Number of Anopheles` > 0, ]

lav_den_sum_wet <- subset_lav_wet %>% 
  mutate(Larva_Density = `Number of Anopheles`/`Number of Dips`)

lav_den_sum_wet$season <- "Wet"

##Compute Av. Larval density
#Dry
lav_den_sum_dry <- lav_den_sum %>% 
  group_by(`Settlement Type`, `Breeding_Site_Recode`) %>%  # Group by breeding site type
  summarize(
    AvgLD = mean(`Larva_Density`, na.rm = TRUE)  # Average number of Anopheles caught per site
  )
lav_den_sum_dry$season <- "Dry"

#Wet
lav_den_sum_wett <- lav_den_sum_wet %>% 
  group_by(`Settlement Type`, `Breeding_Site_Recode`) %>%  # Group by breeding site type
  summarize(
    AvgLD = mean(`Larva_Density`, na.rm = TRUE)  # Average number of Anopheles caught per site
  )

lav_den_sum_wett$season <- "Wet"

##Combine Larval Density Data
lav_den_sum_all <- rbind(lav_den_sum_dry, lav_den_sum_wett)


Fig9 <- ggplot(lav_den_sum_all, aes(x = Breeding_Site_Recode, y = AvgLD)) +
  geom_point(aes(color = `Settlement Type`, size = 4.5), , alpha = 0.7) +  
  facet_wrap(~ `season`)+
  scale_color_manual(values = c(Formal = "#f57362", Slum = "#f9caa7"))+
  geom_text(aes(label = AvgLD), vjust = -1.2, hjust = 0.5, size = 3) + 
  geom_text(aes(label = Breeding_Site_Recode), vjust = -1.2, hjust = 0.5, size = 3) +
  scale_size_continuous(range = c(2, 10)) + 
  labs(title = "Average Larval Density per Breeding Sites by settlement type ",
       y = "Average Larval Density",
       size = "Average Larval Density") +
  guides(size = FALSE)+
  theme_manuscript()

ggsave(paste0(LuDir, '/plots/', Sys.Date(), 'Larval Density of Breeding sites by settlement.pdf'), Fig9, width = 8, height = 6) 


##Morphological classification Analysis
##Wet Season
lav_mol_df <- read_excel(file.path(LuPDir , "Molecular ID for larval mosquitoes.xlsx"))

lav_mol_df_rec <- lav_mol_df %>%
  group_by(`Location`, `Breeding site`) %>%
  summarise(count = n())

lav_mol_df_rec$Species <- "An_coluzii"

lav_mol_df_rec$season <- "Wet"

lav_mol_df_wet <- lav_mol_df_rec %>%
  mutate(settlement = case_when(
    Location == "Agugu" ~ "Slum",
    Location == "Challenge" ~ "Formal"
  ))

lav_mol_df_wet <- lav_mol_df_wet %>%
  mutate(`Breeding site` = case_when(
    `Breeding site` == "Puddle" ~ "Open Drain/Puddles",
    `Breeding site` == "Tyre" ~ "Tyre",
    `Breeding site` == "Plastic" ~ "Artificial containers",
    `Breeding site` == "Gutter" ~ "Drainage/Gutter/Dithces",
  ))

colnames(lav_mol_df_wet)[1] <- "Ward"


# Create dataframe for Dry Season
dt <- data.frame(
  Ward = c("Agugu", "Olopomeji", "Challenge", "Agugu", "Olopomeji", "Challenge"),
  Location = c("Drainage/Gutter/Dithces", "Drainage/Gutter/Dithces", "Drainage/Gutter/Dithces", "Tyre tracks", "Tyre tracks", "Tyre tracks"),
  An_gambiae_ss = c(8, 3, 0, 2, 0, 0),
  An_coluzzii = c(2, 0, 0, 0, 0, 0)
)

# Convert data to long format
data_long <- pivot_longer(dt, cols = c(An_gambiae_ss, An_coluzzii), names_to = "Species", values_to = "Value")

lav_mol_df_dry <- data_long[data_long$Value > 0, ]

lav_mol_df_dry$season <- "Dry"


lav_mol_df_dry <- lav_mol_df_dry %>%
  mutate(settlement = case_when(
    Ward == "Agugu" ~ "Slum",
    Ward == "Olopomeji" ~ "Formal"
  ))

colnames(lav_mol_df_dry)[2] <- "Breeding site"

colnames(lav_mol_df_dry)[4] <- "count"

#Combine Dry and Wet season 
lav_mol_df_all <- rbind(lav_mol_df_dry, lav_mol_df_wet)

##Adult larva by settlement type and breeding site
Fig12 <- ggplot(lav_mol_df_all, aes(x = "", y = count)) +
  geom_point(aes(color = `settlement`, size = 6.5)) +  
  facet_wrap(~ `season`)+
  scale_color_manual(values = c(Formal = "#f57362", Slum = "#f9caa7"))+
  geom_text(aes(label = count), vjust = 1, hjust = 0.5, size = 3) + 
  geom_text(aes(label = `Breeding site` ), vjust = -1.2, hjust = 0.5, size = 3) + 
  scale_size_continuous(range = c(2, 10),
                        breaks = c(2, 3, 8, 10),     # Custom breaks for size legend
                        #labels = c("5 Sites", "10 Sites", "15 Sites", "20 Sites")  # Custom labels for the breaks
  ) + 
  labs(title = "Distribution of anopheles reared to adulthood by season and settlement",
       y = "Number of Adult Anopheles",
       size = "Number of Adult larva") +
  guides(size = FALSE)+
  theme_manuscript() 

ggsave(paste0(LuDir, '/plots/', Sys.Date(), 'Distribution of anopheles reared to adulthood by season, bs and settlement.pdf'), Fig12, width = 8, height = 6) 






##Spatial Distribution plots
##Dry Season
#Agugu
lav_a <- lav_df_dry %>% 
  dplyr::filter(State=="Oyo", Locality == "Agugu")

lav_a_df <- sf::st_as_sf(lav_a, coords=c('Longitude', 'Latitude'), crs=4326)

lav_a_dff <- st_intersection(lav_a_df, df_ib_a)

lav_a_df_y <- lav_a_df %>% 
  dplyr::filter(Anopheles_Caught =="Yes")

lav_a_df_n <- lav_a_dff %>% 
  dplyr::filter(Anopheles_Caught =="No")

##Olopomewa
lav_o <- lav_df_dry %>% 
  dplyr::filter(State=="Oyo", Locality == "Olopomewa")

lav_o_df <- sf::st_as_sf(lav_o, coords=c('Longitude', 'Latitude'), crs=4326)

lav_o_dff <- st_intersection(lav_o_df, df_ib_o)

lav_o_df_y <- lav_o_dff %>% 
  dplyr::filter(Anopheles_Caught =="Yes")

lav_o_df_n <- lav_o_dff %>% 
  dplyr::filter(Anopheles_Caught =="No")

##Challenge
lav_c <- lav_df_dry %>% 
  dplyr::filter(State=="Oyo", Locality == "Challenge")

lav_c_df <- sf::st_as_sf(lav_c, coords=c('Longitude', 'Latitude'), crs=4326)

lav_c_dff <- st_intersection(lav_c_df, df_ib_c)

##Agugu
geo_agu_dry <- ggplot() +
  geom_sf(data = df_ib_a, fill = "aliceblue", color = "black") +  # Background shapefile
  geom_sf(data = lav_a_dff, color = "seagreen", size = 2.5, alpha = 0.5) +  # Breeding sites with no larval
  geom_sf(data = lav_a_df_y, color = "red", size = 2, alpha = 0.7) +  # Breeding sites with Larval
  #geom_sf(data = hh_pos_a_dfff, color = "red", size = 2) +  # Households with positive cases
  #geom_sf_text(data = hh_pos_a_dfff, aes(label = sn), size = 3, nudge_y = 0.001, color = "black")+
  #geom_sf(data = buffers, fill = NA, color = "brown", size = 3) +  # Convex hull polygon
  #geom_point(data = aglegend_data, aes(x = Inf, y = Inf, color = label), size = 3) +  # Invisible points for legend
  #geom_text(data = agugu_shp_m_coords, aes(X, Y, label = round(area_m2, 1)),  # Correct rounding of area_m2
  #  color = 'black', size = 3.5, hjust = 0, vjust = 1) +  # Adjust label positions
  scale_color_manual(
    name = "Legend",
    values = c("Breeding sites prospected" = "seagreen")
  ) +
  labs(title = "Geo-location of breeding sites visited in Agugu(Dry Season)") +
  theme_manuscript()

ggsave(paste0(LuDir, '/plots/', Sys.Date(), 'Geo-location covered by breeding sites in Agugu(dry1).pdf'), geo_agu_dry, width = 12, height = 8)

##New plot
library(ggspatial)

ag_dry <- ggplot() +
  # Plot ward boundary
  geom_sf(data = df_ib_a, fill = "lightblue", color = "black") +
  
  # Plot breeding sites with larvae
  geom_sf(data = lav_a_df_y,
          aes(color = "Breeding sites with Larval"), size = 3) +
  
  # Plot breeding sites without larvae
  geom_sf(data = lav_a_df_n,
          aes(color = "Breeding sites with no Larval"), size = 3, alpha = 0.5) +
  
  # North arrow
  annotation_north_arrow(location = "tl", which_north = "true",
                         style = north_arrow_fancy_orienteering,
                         height = unit(1.5, "cm"), width = unit(1.5, "cm")) +
  
  # Scale bar (units will be inferred from CRS)
  annotation_scale(location = "bl",
                   width_hint = 0.3,
                   text_cex = 0.8,
                   bar_cols = c("grey60", "white"),
                   text_col = "black") +
  
  # Set color manual values
  scale_color_manual(
    values = c(
      "Breeding sites with Larval" = "red",
      "Breeding sites with no Larval" = "darkgreen"
    )
  ) +
  
  # Map title
  labs(title = "Geo-location of Area Covered During Larval Prospection\nin Slum Settlement(Dry Season)",
       color = "Legend") +
  
  # Clean theme
  theme_void() +
  theme(legend.position = "right",
        plot.title = element_text(hjust = 0.5, face = "bold", size = 14))

ggsave(paste0(LuDir, '/plots/', Sys.Date(), 'Geo-location covered by breeding sites in Slumdry.pdf'), ag_dry, width = 10, height = 11)



##Olopomewa
geo_olop_dry <- ggplot() +
  geom_sf(data = df_ib_o, fill = "aliceblue", color = "black") +  # Background shapefile
  geom_sf(data = lav_o_dff, color = "seagreen", size = 2.5, alpha = 0.5) +  # Breeding sites with no larval
  geom_sf(data = lav_o_df_y, color = "red", size = 2) +  # Breeding sites with Larval
  #geom_sf(data = breeding_sites_olop_polygon, fill = NA, color = "brown", size = 3) +  # Convex hull polygon
  #geom_sf(data = o_buffers, fill = NA, color = "brown", size = 3) +  # Convex hull polygon
  #geom_point(data = ollegend_data, aes(x = Inf, y = Inf, color = label), size = 3) +  # Invisible points for legend
  scale_color_manual(
    name = "Legend",
    values = c("Breeding sites prospected" = "seagreen")
  ) +
  labs(title = "Geo-location of breeding sites prospected in Olopomewa(dry season)") +
  map_theme()+
  theme_manuscript()

ggsave(paste0(LuDir, '/plots/', Sys.Date(), 'Geo-location covered by breeding sites in Olopomewa1.pdf'), geo_olop_dry, width = 10, height = 11)

#New plot

olop_dry <- ggplot() +
  # Plot ward boundary
  geom_sf(data = df_ib_o, fill = "lightblue", color = "black") +
  
  # Plot breeding sites with larvae
  geom_sf(data = lav_o_df_y,
          aes(color = "Breeding sites with Larval"), size = 3) +
  
  # Plot breeding sites without larvae
  geom_sf(data = lav_o_df_n,
          aes(color = "Breeding sites with no Larval"), size = 3, alpha = 0.5) +
  
  # North arrow
  annotation_north_arrow(location = "tl", which_north = "true",
                         style = north_arrow_fancy_orienteering,
                         height = unit(1.5, "cm"), width = unit(1.5, "cm")) +
  
  # Scale bar (units will be inferred from CRS)
  annotation_scale(location = "bl",
                   width_hint = 0.3,
                   text_cex = 0.8,
                   bar_cols = c("grey60", "white"),
                   text_col = "black") +
  
  # Set color manual values
  scale_color_manual(
    values = c(
      "Breeding sites with Larval" = "red",
      "Breeding sites with no Larval" = "darkgreen"
    )
  ) +
  
  # Map title
  labs(title = "Geo-location of Area Covered During Larval Prospection\nin Formal Settlement (Dry Season)",
       color = "Legend") +
  
  
  # Clean theme
  theme_void() +
  theme(legend.position = "right",
        plot.title = element_text(hjust = 0.5, face = "bold", size = 14))

ggsave(paste0(LuDir, '/plots/', Sys.Date(), 'Geo-location covered by breeding sites in Formaldry.pdf'), olop_dry, width = 10, height = 11)


##Combine dry season output
geo_dry <- gridExtra::arrangeGrob(ag_dry, olop_dry, ncol = 2)

ggsave(paste0(LuDir, '/plots/', Sys.Date(), 'Geo-location covered by breeding sites in Dry Season.pdf'), geo_dry, width = 10, height = 11)

##Challenge
geo_chal_dry <- ggplot() +
  geom_sf(data = df_ib_c, fill = "aliceblue", color = "black") +  # Background shapefile
  geom_sf(data = lav_c_dff, color = "seagreen", size = 2.5, alpha = 0.5) +  # Breeding sites with no larval
  #geom_sf(data = lav_o_df_y, color = "red", size = 2) +  # Breeding sites with Larval
  #geom_sf(data = breeding_sites_olop_polygon, fill = NA, color = "brown", size = 3) +  # Convex hull polygon
  #geom_sf(data = o_buffers, fill = NA, color = "brown", size = 3) +  # Convex hull polygon
  #geom_point(data = ollegend_data, aes(x = Inf, y = Inf, color = label), size = 3) +  # Invisible points for legend
  scale_color_manual(
    name = "Legend",
    values = c("Breeding sites with no Larval" = "seagreen", "Breeding sites with Larval" = "red")
  ) +
  labs(title = "Geo-location of breeding sites prospected in Challenge (dry season)") +
  map_theme()+
  theme_manuscript()

ggsave(paste0(LuDir, '/plots/', Sys.Date(), 'Geo-location covered by breeding sites in Challenge(Dry).pdf'), geo_chal_dry, width = 10, height = 11)



##Wet Season
#Agugu
lav_aw <- lav_df_wet %>% 
  dplyr::filter(`Ward Name` == "Agugu")

lav_a_dfw <- sf::st_as_sf(lav_aw, coords=c("_Breeding site coordinates_longitude",
                                           "_Breeding site coordinates_latitude"), crs=4326)

lav_a_dffw <- st_intersection(lav_a_dfw, df_ib_a)

lav_w_a_df_y <- lav_a_dffw  %>% 
  dplyr::filter(Anopheles_Caught == "Yes")

lav_w_a_df_n <- lav_a_dffw  %>% 
  dplyr::filter(Anopheles_Caught == "No")


#Challenge
lav_cw <- lav_df_wet %>% 
  dplyr::filter(`Ward Name` == "Challenge")

lav_c_dfw <- sf::st_as_sf(lav_cw, coords=c("_Breeding site coordinates_longitude",
                                           "_Breeding site coordinates_latitude"), crs=4326)

lav_c_dffw <- st_intersection(lav_c_dfw, df_ib_c)

lav_w_c_df_y <- lav_c_dffw %>% 
  dplyr::filter(Anopheles_Caught =="Yes")

lav_w_c_df_y$Breeding.site.coordinates [4] <- "7.345800 3.881000 0 2400"
#st_geometry(lav_w_c_df_y)[4] <- st_point(c(7.345800, 3.881000))

lav_w_c_df_y$Breeding.site.coordinates [3] <- "7.345796 3.880981 0 92.9000015258789"

lav_w_c_df_n <- lav_c_dffw %>% 
  dplyr::filter(Anopheles_Caught =="No")


#Agugu
geo_agu_wet <- ggplot() +
  geom_sf(data = df_ib_a, fill = "aliceblue", color = "black") +  # Background shapefile
  geom_sf(data = lav_w_a_df_n, color = "seagreen", size = 2.5, alpha = 0.5) +  # Breeding sites with no larval
  geom_sf(data = lav_w_a_df_y, color = "red", size = 2, shape = 17) +  # Breeding sites with Larval
  #geom_sf(data = aw_buffers, fill = NA, color = "brown", size = 3) +  # Convex hull polygon
  #geom_point(data = awlegend_data, aes(x = Inf, y = Inf, color = label), size = 3) +  # Invisible points for legend
  scale_color_manual(
    name = "Legend",
    values = c("Breeding sites with no Larval" = "seagreen", "Breeding sites with Larval" = "red")
  ) +
  labs(title = "Geo-location of breeding sites prospected in Agugu(wet season)") +
  map_theme()+
  theme_manuscript()

ggsave(paste0(LuDir, '/plots/', Sys.Date(), 'Geo-location covered by breeding sites in Agugu(wet)1.pdf'), geo_agu_wet, width = 10, height = 11)


##New Plot
#New plot

agu_wet <- ggplot() +
  # Plot ward boundary
  geom_sf(data = df_ib_a, fill = "lightblue", color = "black") +
  
  # Plot breeding sites with larvae
  geom_sf(data = lav_w_a_df_y,
          aes(color = "Breeding sites with Larval"), size = 3) +
  
  # Plot breeding sites without larvae
  geom_sf(data = lav_w_a_df_n,
          aes(color = "Breeding sites with no Larval"), size = 3, alpha = 0.5) +
  
  # North arrow
  annotation_north_arrow(location = "tl", which_north = "true",
                         style = north_arrow_fancy_orienteering,
                         height = unit(1.5, "cm"), width = unit(1.5, "cm")) +
  
  # Scale bar (units will be inferred from CRS)
  annotation_scale(location = "bl",
                   width_hint = 0.3,
                   text_cex = 0.8,
                   bar_cols = c("grey60", "white"),
                   text_col = "black") +
  
  # Set color manual values
  scale_color_manual(
    values = c(
      "Breeding sites with Larval" = "red",
      "Breeding sites with no Larval" = "darkgreen"
    )
  ) +
  
  # Map title
  labs(title = "Geo-location of Area Covered During Larval Prospection\nin Slum Settlement (Wet Season)",
       color = "Legend") +
  
  
  # Clean theme
  theme_void() +
  theme(legend.position = "right",
        plot.title = element_text(hjust = 0.5, face = "bold", size = 14))

ggsave(paste0(LuDir, '/plots/', Sys.Date(), 'Geo-location covered by breeding sites inSlumWet.pdf'), agu_wet, width = 10, height = 11)


##Challenge
geo_chal_wet <- ggplot() +
  geom_sf(data = df_ib_c, fill = "aliceblue", color = "black") +  # Background shapefile
  geom_sf(data = lav_c_dffw, color = "seagreen", size = 2.5, alpha = 0.5) +  # Breeding sites with no larval
  #geom_sf(data = lav_w_c_df_y, color = "red", size = 2) +  # Breeding sites with Larval
  #geom_sf(data = c_buffers, fill = NA, color = "brown", size = 3) +  # Convex hull polygon
  #geom_point(data = cllegend_data, aes(x = Inf, y = Inf, color = label), size = 3) +  # Invisible points for legend
  scale_color_manual(
    name = "Legend",
    values = c("Breeding sites with no Larval" = "seagreen", "Breeding sites with Larval" = "red")
  ) +
  labs(title = "Geo-location of breeding sites prospected in challenge (wet season)") +
  map_theme()+
  theme_manuscript()

ggsave(paste0(LuDir, '/plots/', Sys.Date(), 'Geo-location covered by breeding sites in Challenge1.pdf'), geo_chal_wet, width = 10, height = 11)


##New Plot
chal_wet <- ggplot() +
  # Plot ward boundary
  geom_sf(data = df_ib_c, fill = "lightblue", color = "black") +
  
  # Plot breeding sites with larvae
  geom_sf(data = lav_w_c_df_y,
          aes(color = "Breeding sites with Larval"), size = 3) +
  
  # Labels for sites with larvae
  # geom_text_repel(data = lav_w_c_df_y,
  #                 aes(geometry = geometry, label =Breeding.site.coordinates),
  #                 stat = "sf_coordinates",
  #                 size = 3, color = "black") +
  
  # Plot breeding sites without larvae
  geom_sf(data = lav_w_c_df_n,
          aes(color = "Breeding sites with no Larval"), size = 3, alpha = 0.7) +
  
  # North arrow
  annotation_north_arrow(location = "tl", which_north = "true",
                         style = north_arrow_fancy_orienteering,
                         height = unit(1.5, "cm"), width = unit(1.5, "cm")) +
  
  # Scale bar (units will be inferred from CRS)
  annotation_scale(location = "bl",
                   width_hint = 0.3,
                   text_cex = 0.8,
                   bar_cols = c("grey60", "white"),
                   text_col = "black") +
  
  # Set color manual values
  scale_color_manual(
    values = c(
      "Breeding sites with Larval" = "red",
      "Breeding sites with no Larval" = "darkgreen"
    )
  ) +
  
  # Map title
  labs(title = "Geo-location of Area Covered During Larval Prospection\nin Formal Settlement (Wet Season)",
       color = "Legend") +
  
  
  # Clean theme
  theme_void() +
  theme(legend.position = "right",
        plot.title = element_text(hjust = 0.5, face = "bold", size = 14))

ggsave(paste0(LuDir, '/plots/', Sys.Date(), 'Geo-location covered by breeding sites in FormalWet.pdf'), chal_wet, width = 10, height = 11)



###Extra manuscript analysis
lav_df_dry$season <- "Dry"

lav_df_wet$season <- "Wet"

lav_dfd <- lav_df_dry %>% 
  dplyr::select(`Settlement Type`, Anopheles_Caught,
                season, Breeding_Site_Recode, Breeding_Site_Recode2)

lav_dfw <- lav_df_wet %>% 
  dplyr::select(`Settlement Type`, Anopheles_Caught,
                season, Breeding_Site_Recode, Breeding_Site_Recode2)

lav_overall <- rbind(lav_dfd, lav_dfw)

write.csv(lav_overall, file.path(LuDir, "lav_dataset.csv"))

# Create contingency table(Breeding site vs Season)
table_chi <- table(lav_overall$Breeding_Site_Recode2, lav_overall$season)

# Run Chi-square test
chisq.test(table_chi)

# Create contingency table(Breeding site vs `Settlement Type`)
table_chi2 <- table(lav_overall$Breeding_Site_Recode2, lav_overall$`Settlement Type`)

# Run Chi-square test
chisq.test(table_chi2)

lav_overall %>%
  group_by(season) %>%
  group_split() %>%
  lapply(function(df){
    tbl <- table(df$Breeding_Site_Recode2, df$`Settlement Type`)
    chisq_result <- chisq.test(tbl)
    return(list(season = unique(df$season), result = chisq_result))
  })

###Anova Analysis
# Run one-way ANOVA for each season separately
lav_den_sum_all %>%
  group_by(season) %>%
  group_map(~ {
    cat("ANOVA results for season:", unique(.x$season), "\n")
    print(summary(aov(AvgLD ~ `Settlement Type`, data = .x)))
    cat("\n")
  })


#Boxplot
ld_bxp <- ggplot(lav_den_sum_all, aes(x = `Settlement Type`, y = AvgLD, fill = `Settlement Type`)) +
  geom_boxplot() +
  facet_wrap(~season) +
  scale_fill_manual(values = c(Formal = "#f57362", Slum = "#f9caa7"))+
  theme_manuscript() +
  labs(title = "Average Larval Density by Settlement Type and Season",
       y = "Average Larval Density",
       x = "Settlement Type")

ggsave(paste0(LuDir, '/plots/', Sys.Date(), 'Box plot of larval densitities by settlement n season.pdf'), ld_bxp, width = 10, height = 11)


##Exploring total anopheles per season and settlement
##Some data wrangling before computation
lav_anop_sum_dry <-lav_den_sum %>%
  dplyr::select(`Settlement Type`, Anopheles, Breeding_Site_Recode, Breeding_Site_Recode2, Larva_Density,season)

lav_anop_sum_wet <-lav_den_sum_wet %>%
  dplyr::select(`Settlement Type`, `Number of Anopheles`, Breeding_Site_Recode, Breeding_Site_Recode2, Larva_Density,season)

colnames(lav_anop_sum_wet)[2] <- "Anopheles"


lav_anop_sum_all <- rbind(lav_anop_sum_dry, lav_anop_sum_wet)

##Estimating total anopheles
lav_anop_sum_all %>%
  summarise(Total_Anopheles = sum(Anopheles, na.rm = TRUE))

anoph_sum <- lav_anop_sum_all %>%
  group_by(season, `Settlement Type`, Breeding_Site_Recode) %>%
  summarise(Total_Anopheles = sum(Anopheles, na.rm = TRUE)) %>%
  arrange(season, `Settlement Type`, Breeding_Site_Recode)

anoph_sum <- lav_anop_sum_all %>%
  group_by(season, Breeding_Site_Recode) %>%
  summarise(Total_Anopheles = sum(Anopheles, na.rm = TRUE)) %>%
  arrange(season, Breeding_Site_Recode)


##Kruskal Wallis (due to small sample sizes)

#Function to run Kruskal-Wallis test by Season
run_kruskal_test <- function(season_name, data){
  season_data <- data %>% filter(season == season_name)
  
  if(length(unique(season_data$`Settlement Type`)) > 1){
    result <- kruskal.test(AvgLD ~ `Settlement Type`, data = season_data)
    return(result)
  } else {
    return("Not enough groups for Kruskal-Wallis Test.")
  }
}

# Run Kruskal-Wallis tests for both Dry and Wet seasons
kruskal_dry <- run_kruskal_test("Dry", lav_den_sum_all)
kruskal_wet <- run_kruskal_test("Wet", lav_den_sum_all)

# Print test results
cat("\nKruskal-Wallis Test for Dry Season:\n")
print(kruskal_dry)

cat("\nKruskal-Wallis Test for Wet Season:\n")
print(kruskal_wet)



# Summarize median larval density and sample size by Settlement Type and Season
summary_stats <- lav_den_sum_all %>%
  group_by(season, `Settlement Type`) %>%
  summarise(
    N = n(),
    Median_Larval_Density = median(AvgLD, na.rm = TRUE),
    Min = min(AvgLD, na.rm = TRUE),
    Max = max(AvgLD, na.rm = TRUE)
  ) %>%
  arrange(season, `Settlement Type`)

# Print summary table
cat("\nSummary of Median Larval Densities by Settlement Type and Season:\n")
print(summary_stats)

ggplot(lav_den_sum_all, aes(x = `Settlement Type`, y = AvgLD)) +
  geom_boxplot() +
  facet_wrap(~ season) +
  theme_minimal()

lav_den_sum_all %>% 
  group_by(season, `Settlement Type`) %>% 
  summarise(median_AvgLD = median(AvgLD, na.rm = TRUE),
            IQR = IQR(AvgLD, na.rm = TRUE),
            n = n())


##Prepare data for breeding site analysis
lav_bs_dry <- lav_df_dry %>% 
  dplyr::select(SN, `Settlement Type`, Latitude, Longitude, Anopheles_Caught, Breeding_Site_Recode) %>%
  rename(site_label = SN,
         latitude = Latitude,
         longitude = Longitude,
         anophw = Anopheles_Caught)

lav_bs_slum_dry <- lav_bs_dry %>% 
  dplyr::filter(`Settlement Type` == "Slum") %>% 
  filter(!`site_label` %in% c(17, 27, 40))

lav_bs_formal_dry <- lav_bs_dry %>% 
  dplyr::filter(`Settlement Type` == "Formal")%>% 
  filter(!`site_label` %in% c(22, 6, 9))


lav_bs_wet <- lav_df_wet %>% 
  dplyr::select(`Household Code/Number`, `Settlement Type`, `_Breeding site coordinates_latitude`,`_Breeding site coordinates_longitude`, Anopheles_Caught, Breeding_Site_Recode) %>% 
  rename(site_label = `Household Code/Number`,
       latitude =  `_Breeding site coordinates_latitude`,
       longitude = `_Breeding site coordinates_longitude`,
       anophw = Anopheles_Caught)

lav_bs_slum_wet <- lav_bs_wet %>% 
  dplyr::filter(`Settlement Type` == "Slum")

lav_bs_formal_wet <- lav_bs_wet %>% 
  dplyr::filter(`Settlement Type` == "Formal")%>% 
  filter(!`site_label` %in% c(272, 147, 98, 71))


##Boxplot of breeding site densities
den_summary_all <- rbind(Agudry_summary, Aguwet_summary, Chal_summary, Olop_summary)

# Convert to long format
den_summary_long <- den_summary_all %>%
  pivot_longer(
    cols = c(mean_density, sd_density, min_density, max_density),
    names_to = "Density_Metric",
    values_to = "Value"
  )

# View result
head(den_summary_long)


ggplot(den_summary_long, aes(x = settlment, y = Value, fill = settlment)) +
  geom_boxplot() +
  facet_wrap(~ season)+
  scale_fill_manual(values = c(Formal = "#f57362", Slum = "#f9caa7"))+
  labs(
    title = "Distribution of Breeding Site Densities by Ward",
    x = "Ward",
    y = "Breeding Site Density (sites/km²)"
  ) +
  theme_manuscript() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 



 
-------------------------------------------------------------------------------
###New Analysis based on literature to strengthen manuscript
-------------------------------------------------------------------------------
##Make study site plot for methods(Figure 1)
library(sf)
library(ggplot2)
library(ggspatial)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggrepel)

# Load Nigeria state boundaries
nigeria_lgas <- st_read("C:/Users/ebamgboye/Urban Malaria Proj Dropbox/urban_malaria/data/nigeria/nigeria_shapefiles/shapefiles/gadm36_NGA_shp/gadm36_NGA_2.shp")


pd <- ggplot(selected_lgas) +
  geom_sf() +
  geom_text_repel(
    data = df_ib,
    aes(label =  WardName, geometry = geometry),color ='black',
    stat = "sf_coordinates", min.segment.length = 0, size = 3.5, force = 1)+
  scale_fill_manual(values = c(
    "Agugu" = "plum",  # Replace with actual ward names and desired colors
    "Challenge" = "coral",
    "Olopomewa" = "lightgreen"
  ), na.value = "white")+
  map_theme()+ 
  labs(title= "Wards in Ibadan visited for entomology study ")+
  coord_sf()

ggsave(paste0(LuDir, '/plots/', Sys.Date(), "/", 'ibadan ento study sites.pdf'), pd, width = 8, height = 6)

# Filter Oyo State
oyo_state <- nigeria_lgas[nigeria_lgas$NAME_1 == "Oyo", ]

# Select Ibadan LGAs by name
ibadan_lgas <- c("IbadanNorth", "IbadanNorth-East", "IbadanNorth-West", 
                 "IbadanSouth-East", "IbadanSouth-West")

# Subset those LGAs
selected_lgas <- oyo_state[oyo_state$NAME_2 %in% ibadan_lgas, ]

# Load larva site data 
larva_df <- read.csv("C:/Users/ebamgboye/OneDrive - Loyola University Chicago/Documents/wlav_coords.csv")

lav_df_dry <- sf::st_as_sf(lav_df_dry, coords=c('Longitude', 'Latitude'), crs=4326)

# Convert to sf object using lat/long columns
larva_sites <- st_as_sf(larva_df, coords = c("Longitude", "Latitude"), crs = 4326)

lav_df_wet <- sf::st_as_sf(lav_df_wet, coords=c('_Breeding site coordinates_longitude', '_Breeding site coordinates_latitude'), crs=4326)

# Get West Africa boundary (optional — for inset)
west_africa <- ne_countries(scale = "medium", continent = "Africa", returnclass = "sf")

# Nigeria boundary (for inset)
nigeria <- west_africa[west_africa$name == "Nigeria", ]

# Main map of Ibadan with selected LGAs and larva sites
ibadan_map <- ggplot() +
  geom_sf(data = oyo_state, fill = "grey90", color = "black") +
  geom_sf(data = df_ib, fill = "lightpink", color = "black") +
  #geom_sf(data = lav_df_dry, aes(color = "Dry Season"), size = 2) +
  #geom_sf(data = lav_df_wet, aes(color = "Wet SEason"), size = 2) +
  #geom_sf_text(data = df_ib, aes(label = WardName), size = 3, color = "black") +
  #scale_color_manual(values = c("Larva Sites" = "red")) +
  theme_minimal() +
  labs(title = "Map of Oyo State showing selected LGAs",
       color = "Legend") +
  annotation_scale(location = "bl", width_hint = 0.3) +
  annotation_north_arrow(location = "tl", which_north = "true", 
                         style = north_arrow_fancy_orienteering) +
  coord_sf()

# Inset map of Nigeria highlighting Oyo State
inset_map <- ggplot() +
  #geom_sf(data = west_africa, fill = "white", color = "grey70") +
  geom_sf(data = nigeria, fill = "grey90") +
  geom_sf(data = oyo_state, fill = "lightgreen", color = "black") +
  theme_void()

ibadan_zoom <- ggplot() +
  geom_sf(data = df_ib, fill = "aliceblue", color = "black") +
  geom_sf(data = lav_df_dry, aes(color = "Dry Season"), size = 2) +
  geom_sf(data = lav_df_wet, aes(color = "Wet Season"), size = 2) +
  scale_color_manual(values = c("Dry Season" = "red", "Wet Season" = "deepskyblue")) +
  annotation_scale(location = "bl", width_hint = 0.3) +
  annotation_north_arrow(location = "tl", which_north = "true",
                         style = north_arrow_fancy_orienteering) +
  coord_sf() +
  theme_minimal() +
  labs(title = "Larva Sampling Sites in Ibadan",
       color = "Season")


ibadan_zoom

# Display map
ibadan_map
inset_map

library(cowplot)

final_map <- plot_grid(
  inset_map, NULL,
  ibadan_map, ibadan_zoom,
  ncol = 2, rel_widths = c(1, 1)
)

final_map <- plot_grid(
  inset_map, ibadan_map, ibadan_zoom,
  ncol = 3, rel_widths = c(1, 1, 1)
)

final_map
------------------------------------------------------------------------------
  ##Prediction of suitable anopheles larval habitats
-------------------------------------------------------------------------------
### Presence only approach using Maximum Entropy
  install.packages(c("terra", "sf", "dismo", "randomForest", "gbm", "caret", "rJava"))
library(terra)
library(sf)
library(dismo)
library(randomForest)
library(gbm)
library(caret)
library(rJava)  # Needed for MaxEnt
library(GGally)

options(java.home="C:/Program Files/Eclipse Adoptium/jdk-21.0.7.6-hotspot")
library(rJava)
rJava::.jinit()

##Read in Nigeria shapefile
ng_shp <- st_read("C:/Users/ebamgboye/OneDrive - Loyola University Chicago/Downloads/Boundary_VaccWards_Export/Boundary_VaccWards_Export.shp")

oyo_wardshp <- ng_shp %>% 
  dplyr::filter(StateCode == "OY")
library(terra)

##Load raster files
dwb_rast <- rast("C:/Users/ebamgboye/Urban Malaria Proj Dropbox/urban_malaria/data/nigeria/Raster_files/distance_to_water_bodies/distance_to_water.tif")

ndwi_rast <- rast("C:/Users/ebamgboye/Urban Malaria Proj Dropbox/urban_malaria/data/nigeria/Raster_files/NDWI/Nigeria_NDWI_2023.tif")

hq_rast <- rast("C:/Users/ebamgboye/Urban Malaria Proj Dropbox/urban_malaria/data/nigeria/Raster_files/housing/2019_Nature_Africa_Housing_2015_NGA.tiff")

pop_den_rast <- rast("C:/Users/ebamgboye/Urban Malaria Proj Dropbox/urban_malaria/data/nigeria/Raster_files/NGA_pop_density/gpw_v4_population_density_rev11_2020_1_deg.tif")

soil_rast <- rast("C:/Users/ebamgboye/Urban Malaria Proj Dropbox/urban_malaria/data/nigeria/Raster_files/surface_soil_wetness/GIOVANNI-g4.timeAvgMap.M2TMNXLND_5_12_4_GWETTOP.20181201-20181231.180W_90S_180E_90N.tif")

elev_rast <- rast("C:/Users/ebamgboye/Urban Malaria Proj Dropbox/urban_malaria/data/nigeria/Raster_files/Elevation/ELE.tif")

temp_val <- read.csv("C:/Users/ebamgboye/Urban Malaria Proj Dropbox/urban_malaria/data/nigeria/extracted_landsurface_temperature/Oyo_Monthly_LST_Per_Ward.csv")

-----------------------------------------------------------------------
##Explore raster files
# Crop dwb raster to ward extent
cropped_raster <- crop(dwb_rast, df_ib_a)

# Mask raster to exact ward shape
masked_raster <- mask(cropped_raster, df_ib_a)

# Plot result
plot(masked_raster)
lines(df_ib_a, col="red", lwd=2)
---------------------------------------------------------------------
# Crop ndwi raster to ward extent
cropped_raster_nd <- crop(ndwi_rast, df_ib_a)

# Mask raster to exact ward shape
masked_raster_nd <- mask(cropped_raster_nd, df_ib_a)

# Plot result
plot(masked_raster_nd)
lines(df_ib_a, col="red", lwd=2)
---------------------------------------------------------------------
# # Crop hq raster to ward extent
# cropped_raster_hq <- crop(hq_rast, df_ib_a)
# 
# # Mask raster to exact ward shape
# masked_raster_hq <- mask(cropped_raster_hq, df_ib_a)
# 
# # Plot result
# plot(masked_raster_hq)
# lines(df_ib_a, col="red", lwd=2)
----------------------------------------------------------------------
# Crop popden raster to ward extent
cropped_raster_pd <- crop(pop_den_rast, df_ib_a)

# Mask raster to exact ward shape
masked_raster_pd <- mask(cropped_raster_pd, df_ib_a)

# Plot result
plot(masked_raster_pd)
lines(df_ib_a, col="red", lwd=2)
---------------------------------------------------------------------
# # Crop soil wetness raster to ward extent
# cropped_raster_sl <- crop(soil_rast, df_ib_a)
# 
# # Mask raster to exact ward shape
# masked_raster_sl <- mask(cropped_raster_sl, df_ib_a)
# 
# # Plot result
# plot(masked_raster_sl)
# lines(df_ib_a, col="red", lwd=2)
--------------------------------------------------------------------
# Crop Elevation raster to ward extent
cropped_raster_ele <- crop(elev_rast, df_ib_a)

# Mask raster to exact ward shape
masked_raster_ele <- mask(cropped_raster_ele, df_ib_a)

# Plot result
plot(masked_raster_ele)
lines(df_ib_a, col="red", lwd=2)
---------------------------------------------------------------------
  
##Temperature data to fit study location and time
temp_val_agu <- temp_val %>% 
  dplyr::filter(WardName == "Agugu")

library(ggplot2)
library(dplyr)
library(viridis)

# Convert Month numbers to month names and make them ordered factors
temp_val_agu <- temp_val_agu %>%
  mutate(Month = factor(Month, 
                        levels = 1:12, 
                        labels = c("January", "February", "March", "April", 
                                   "May", "June", "July", "August", 
                                   "September", "October", "November", "December")))

# Heatmap 
ggplot(ward_temp24, aes(x = Year, y = Month, fill = MeanLST)) +
  geom_tile(color = "white") +
  scale_fill_viridis_c(option = "inferno") +
  scale_y_discrete(limits = rev(levels(temp_val_agu$Month))) +  # Puts January on top
  theme_minimal() +
  labs(title = "Monthly Land Surface Temperature Distribution (2018-2025)",
       x = "Year",
       y = "Month",
       fill = "Temp (°C)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid = element_blank())


###Visualizations
# Merge temperature data with shapefile via WardName
ward_temp <- df_ib_a %>%
  left_join(temp_val_agu, by = "WardName")

ward_temp24 <- ward_temp %>% 
  dplyr::filter(Year == "2024")

ggplot() +
  geom_sf(data = ward_temp24, aes(fill = MeanLST), color = "grey50") +
  scale_fill_viridis_c(option = "inferno", na.value = "grey90") +
  facet_wrap(~ Month) +
  theme_minimal() +
  labs(title = "Monthly Land Surface Temperature by Ward",
       fill = "Temp (°Kelvin)")

library(gganimate)

p <- ggplot() +
  geom_sf(data = ward_temp, aes(fill = MeanLST), color = "white") +
  scale_fill_viridis_c(option = "inferno", na.value = "grey90") +
  theme_minimal() +
  labs(title = 'Land Surface Temperature: Month {closest_state}',
       subtitle = 'Year: {frame_time}',
       fill = "Temp (°Kelvin)") +
  transition_states(Month, transition_length = 2, state_length = 1) +
  ease_aes('linear')

# Run animation
anim <- animate(p, nframes = 60, width = 900, height = 700)

##Temperature wrangling for model prediction

rainy_months <- c("May", "June", "July", "August", "September", "October")
dry_months   <- c("November", "December", "January", "February", "March", "April")

rainy_months <- c("May", "June", "July", "August", "September", "October")
dry_months   <- c("November", "December", "January", "February", "March", "April")

# Rainy season mean LST
ag_rainy_mean <- ward_temp24 %>%
  filter(Month %in% rainy_months) %>%
  group_by(WardName) %>%
  summarise(mean_LSTr = mean(MeanLST, na.rm = TRUE))

# Dry season mean LST
ag_dry_mean <- ward_temp24 %>%
  filter(Month %in% dry_months) %>%
  group_by(WardName) %>%
  summarise(mean_LSTd = mean(MeanLST, na.rm = TRUE))

### Convert temp values to spatial points
temp_vect_r <- vect(ag_rainy_mean)

temp_vect_d <- vect(ag_dry_mean)

##Rasterize temperature variable(mean LST)
temp_rast_d <- rasterize(temp_vect_d, dwb_rast, field = "mean_LSTd")

temp_rast_r <- rasterize(temp_vect_r, dwb_rast, field = "mean_LSTr")
-------------------------------------------------------------------------------
###Prepare environmental covariates data for analysis
  ##Stack Rasters
  

# Reproject rasters to match dwb_rast (to ensure ease of stacking)
ndwi_rast <- project(ndwi_rast, dwb_rast)
pop_den_rast <- project(pop_den_rast, dwb_rast)
elev_rast <- project(elev_rast, dwb_rast)
temp_rast_d <- project(temp_rast_d, dwb_rast)
temp_rast_r <- project(temp_rast_r, dwb_rast)


# Resample rasters to match dwb_rast's extent and resolution
ndwi_rast <- resample(ndwi_rast, dwb_rast)
pop_den_rast <- resample(pop_den_rast, dwb_rast)
elev_rast <- resample(elev_rast, dwb_rast)
temp_rast_d <- resample(temp_rast_d, dwb_rast)
temp_rast_r <- resample(temp_rast_r, dwb_rast)

# Step 2: Stack rasters together
env_stack <- c(dwb_rast, ndwi_rast, pop_den_rast, temp_rast_d, temp_rast_r)

# Check final stack
plot(env_stack)


# Step 3: Read in larval habitat points
points <- lav_df_wet

# Convert your data frame to SpatVector
points_vect <- vect(points, geom = c("_Breeding site coordinates_longitude", "_Breeding site coordinates_latitude"), crs = crs(env_stack))

# Confirm CRS is now attached
crs(points_vect)

# Step 4: Check CRS match and reproject points if needed
if (crs(env_stack) != crs(points_vect)) {
  points <- st_transform(points_vect, crs(env_stack))
}

# Step 5: Extract raster values at point locations
extract_df <- terra::extract(env_stack, points_vect)


# Step 6: Combine extracted values with point attributes
points_df <- cbind(st_drop_geometry(points), extract_df)

# Step 7: Save extracted data (optional)
#write.csv(points_df, "extracted_raster_values.csv", row.names = FALSE)

# Step 8: Check correlation matrix among environmental variables
# Select only the raster variable columns — adjust column names if needed
env_vars <- points_df %>%
  dplyr::select(distance_to_water, NDWI, gpw_v4_population_density_rev11_2020_1_deg, mean_LSTd, mean_LSTr)

# Compute correlation matrix (Pearson)
cor_matrix <- cor(env_vars, use = "complete.obs", method = "pearson")

# View correlation matrix in console
print(cor_matrix)

# Step 9: Visualize correlation matrix as a heatmap
corrplot(cor_matrix, method = "color", type = "upper",
         tl.cex = 0.8, addCoef.col = "black", number.cex = 0.7)

# Optional: Pairwise scatter plots with correlation values
ggpairs(env_vars)


##Assessment with VIF  
# Load libraries
library(car)
library(usdm)
library(performance)
# Assuming you have a dataframe with your extracted raster values for each point
# e.g.

##habitat_data <- data.frame(distance_to_water, NDWI, population_density, ELE)
# Add a dummy response variable (since lm() needs a response)
env_vars$response <- rnorm(nrow(env_vars))

# Fit a linear model with all predictors (excluding the response column)
vif_model <- lm(response ~ . , data = env_vars)

# Check VIF values
vif_results <- check_collinearity(vif_model)

# View VIF table
print(vif_results)

# Filter variables with high VIF 
high_vif_vars <- subset(vif_results, VIF > 10)
print(high_vif_vars)
  

##Read in wet season larval data to compute time difference
larva_wet <- read_xlsx("C:/Users/ebamgboye/Downloads/WET_SEASON_ENTO_COLLECTION_LARVAL_PROSPECTION_-_all_versions_-_labels_-_2025-06-17-20-42-08.xlsx")

# Load the lubridate package
library(lubridate)
library(readxl)

# Load lubridate
library(lubridate)

# Example dataframe
df <- data.frame(
  start = c("2024-07-19T07:54:10.516+01:00", "2024-07-19T10:00:00.000+01:00"),
  end   = c("2024-07-19T13:20:09.785+01:00", "2024-07-19T14:30:00.000+01:00")
)

# Parse to datetime
larva_wet$start_parsed <- ymd_hms(larva_wet$start)
larva_wet$end_parsed   <- ymd_hms(larva_wet$end)

# Compute time difference (in minutes)
larva_wet$time_diff_mins <- as.numeric(difftime(larva_wet$end_parsed, larva_wet$start_parsed, units = "mins"))

# View result
print(larva_wet)

pos_lav_wet <- larva_wet %>% 
  dplyr::filter(`Number of Anopheles` > 0)
