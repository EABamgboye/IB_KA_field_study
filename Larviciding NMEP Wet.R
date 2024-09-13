user <- Sys.getenv("USERNAME")
Drive <- file.path(gsub("[\\]", "/", gsub("Documents", "", Sys.getenv("HOME"))))
LuDir <- file.path(Drive, "Documents")
LuPDir <- file.path(Drive, "Downloads")

## read ibadan ward shape files
df_ib <- st_read(file.path(LuDir, "kano_ibadan_shape_files", "ibadan_metro_ward_fiveLGAs", "Ibadan_metro_fiveLGAs.shp")) %>%
  mutate(WardName = ifelse(WardName == 'Oranyan' & LGACode == '31007', 'Oranyan_7', WardName))

agugu_shp <- st_read(file.path(LuPDir, "Agugu", "Agugu.shp"))

pw <- ggplot(df_ib) +
  geom_sf(aes(fill = WardName), color = "black") +
  geom_text_repel(
    data = df_ib,
    aes(label =  WardName, geometry = geometry),color ='black',
    stat = "sf_coordinates", min.segment.length = 0, size = 3.5, force = 1)+
  scale_fill_manual(values = c(
    "Agugu" = "plum",  # Replace with actual ward names and desired colors
    "Challenge" = "lightgreen"
    ), na.value = "white")+
  map_theme()+ 
  labs(title= "Wards in Ibadan visited for wet season entomological study")+
  coord_sf()

ggsave(paste0(LuDir, '/plots/', Sys.Date(), "/", 'dry season study sites.pdf'), pw, width = 8, height = 6)


##Split Ibadan shapefile into working wards
df_ib_c <- df_ib %>%
  dplyr::filter(WardName == 'Challenge')

df_ib_a <- df_ib %>%
  dplyr::filter(WardName == 'Agugu')

df_ib_o <- df_ib %>%
  dplyr::filter(WardName == 'Olopomewa')



##Read in dataset
library(readxl)
lav_df_wet <- read_excel(file.path(LuPDir , "WET_SEASON_ENTO_COLLECTION_LARVAL_PROSPECTION_-_all_versions_-_labels_-_2024-08-12-21-21-06.xlsx"))

lav_df_wet  <- lav_df_wet  %>% 
  mutate(`Household Code/Number` = 1:272)

lav_df_wet  <- slice(lav_df_wet , -(1:2))

lav_df_wet  <- slice(lav_df_wet , -(6))

lav_df_wet  <- lav_df_wet  %>% 
  mutate(Anopheles_Caught = ifelse(`Number of Anopheles` > 0, "Yes", "No"))


##Wet Season Larval Habitat - Ibadan

##Location of breeding sites
#Agugu
lav_aw <- lav_df_wet %>% 
  dplyr::filter(`Ward Name` == "Agugu")

lav_a_dfw <- sf::st_as_sf(lav_aw, coords=c("_Breeding site coordinates_longitude",
                                           "_Breeding site coordinates_latitude"), crs=4326)

lav_a_dffw <- st_intersection(lav_a_dfw, df_ib_a)

agl_wet <- ggplot(df_ib_a) +
  geom_sf(fill= "NA")+
  geom_point(data= lav_a_dffw,  aes(geometry = geometry, size = 0.05, alpha = 0.01, col = `Anopheles_Caught`), stat= "sf_coordinates")+
  scale_color_manual(values = c(Yes = "seagreen", No = "red"))+
  # scale_shape_manual(values = c(Formal = 16,  Informal= 17, Slum = 14))+
  geom_text_repel(
    data = df_ib_a,
    aes(label =  `WardName`, geometry = geometry),color ='black',
    stat = "sf_coordinates", min.segment.length = 0, size = 2.5, force = 1, max.overlaps = Inf)+
  guides(alpha = FALSE, size = FALSE) +
  map_theme()+ 
  ylab("")+
  xlab("")+
  labs(title= "Sites for Wet Season Larval Collection(Agugu)")+
  coord_sf()

ggsave(paste0(LuDir, '/plots/', Sys.Date(), "/", 'wet season locations_larva prospection_agugu.pdf'), agl_wet, width = 8, height = 6)

#Challenge
lav_cw <- lav_df_wet %>% 
  dplyr::filter(`Ward Name` == "Challenge")

lav_c_dfw <- sf::st_as_sf(lav_cw, coords=c("_Breeding site coordinates_longitude",
                                           "_Breeding site coordinates_latitude"), crs=4326)

lav_c_dffw <- st_intersection(lav_c_dfw, df_ib_c)

chal_wet <- ggplot(df_ib_c) +
  geom_sf(fill= "NA")+
  geom_point(data= lav_c_dffw,  aes(geometry = geometry, size = 0.05, alpha = 0.01, col = `Anopheles_Caught`), stat= "sf_coordinates")+
  scale_color_manual(values = c(Yes = "seagreen", No = "red"))+
  # scale_shape_manual(values = c(Formal = 16,  Informal= 17, Slum = 14))+
  geom_text_repel(
    data = df_ib_c,
    aes(label =  `WardName`, geometry = geometry),color ='black',
    stat = "sf_coordinates", min.segment.length = 0, size = 2.5, force = 1, max.overlaps = Inf)+
  guides(alpha = FALSE, size = FALSE) +
  map_theme()+
  ylab("")+
  xlab("")+
  labs(title= "Sites for Wet Season Larval Collection(Challenge)")+
  coord_sf()

ggsave(paste0(LuDir, '/plots/', Sys.Date(), "/", 'wet season locations_larva prospection_challenge.pdf'), chal_wet, width = 8, height = 6)


##Breeding site analysis

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

##Recode breeding sites to macth Dry Season
lav_df_wet <- lav_df_wet %>% 
  mutate(Breeding_Site_Recode = recode(`Type of breeding site`,
                                       "Drainage" = "Drainage/Gutter/Ditch",
                                       "Gutter" = "Drainage/Gutter/Ditch",
                                       "Ditch" = "Drainage/Gutter/Ditch"))

# Summarize data by breeding site type
breeding_site_sum_wet <- lav_df_wet %>% 
  group_by(`Ward Name`,`Breeding_Site_Recode`) %>%  # Group by breeding site type
  summarize(
    SitesVisited = n(),  # Number of sites visited per type
  ) 

#Number and type of breeding sites
bs_wet <- ggplot(data=breeding_site_sum_wet, aes(x = `Breeding_Site_Recode`, y=SitesVisited, fill = `Breeding_Site_Recode`)) +
  geom_bar(stat="identity", position = position_dodge(width = 0.8))+
  facet_wrap(~ `Ward Name`, scales = "free_x")+
  geom_text(aes(label=SitesVisited), vjust = -0.5,
            color="black", size=3.5)+
  theme_manuscript()+
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),  # Adjust x-axis label angle and size
    legend.position = "none",  # Remove the legend
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6)
  ) +
  ylim(0,100)+
  ggtitle("Number and type of breeding sites visited in Ibadan, July-August, 2024")

ggsave(paste0(LuDir, '/plots/', Sys.Date(), 'wet season breeding sites visited in Ibadan.pdf'), bs_wet, width = 8, height = 6)

###Number and type of breeding sites anopheles was caught
b_site_ano_sum_wet <- lav_df_wet %>% 
  group_by(`Ward Name`,`Breeding_Site_Recode`, `Anopheles_Caught`) %>%  # Group by breeding site type
  summarize(
    SitesVisited = n(),  # Number of sites visited per type
  )

bs_ano_wet <- ggplot(data=b_site_ano_sum_wet, aes(x=`Breeding_Site_Recode`, y=SitesVisited, fill = `Anopheles_Caught`)) +
  geom_bar(stat="identity", position = "stack")+
  facet_wrap(~ `Ward Name`, scales = "free_x")+
  geom_text(aes(label=SitesVisited), vjust = 0.9,
            color="black", size=3.5)+
  theme_manuscript()+
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),  # Adjust x-axis label angle and size
    #legend.position = "none",  # Remove the legend
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6)
  ) +
  ylim(0,50)+
  ggtitle("Number of breeding sites visted and number where \n anopheles larva were found in Ibadan, July-August, 2024")+
  scale_fill_manual(values = c(Yes = "bisque2", No = "coral"))

ggsave(paste0(LuDir, '/plots/', Sys.Date(), 'wet season breeding sites with anopheles in Ibadan.pdf'), bs_ano_wet, width = 8, height = 6)

#Total number of anopheles caught by type of breeding sites
# Summarize data by breeding site type
ano_caught_sum_wet <- lav_df_wet %>% 
  group_by(`Settlement Type`, `Ward Name`,`Breeding_Site_Recode`) %>%  # Group by breeding site type
  summarize(
    SitesVisited = n(),  # Number of sites visited per type
    NoAnophelesPerSite = sum(`Number of Anopheles`, na.rm = TRUE)  # Average number of Anopheles caught per site
  ) 


ano_bs_wet <- ggplot(data=ano_caught_sum_wet, aes(x=`Breeding_Site_Recode`, y=NoAnophelesPerSite, fill = `Breeding_Site_Recode`)) +
  geom_bar(stat="identity", position = position_dodge(width = 0.8))+
  facet_wrap(~ `Ward Name`, scales = "free_x")+
  geom_text(aes(label=NoAnophelesPerSite), vjust = -0.5,
            color="black", size=3.5)+
  theme_manuscript()+
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),  # Adjust x-axis label angle and size
    legend.position = "none",  # Remove the legend
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6)
  ) +
  ylim(0,700)+
  ggtitle("Number of anopheles caught per breeding sites visted in Ibadan, July-August, 2024")

ggsave(paste0(LuDir, '/plots/', Sys.Date(), 'wet season anopheles per breeding sites in Ibadan.pdf'), ano_bs_wet, width = 8, height = 6)

#Larval Density
##Calculations(Larval density)
# lav_ib_dry <- lav_df_dry %>% 
#   dplyr::filter(State=="Oyo")

subset_lav_wet <- lav_df_wet[lav_df_wet$`Number of Anopheles` > 0, ]

lav_den_sum_wet <- subset_lav_wet %>% 
  mutate(Larva_Density = `Number of Anopheles`/`Number of Dips`)


# ##Compute Av. Larval density
# lav_den_sum_wett <- lav_den_sum_wet %>% 
#   group_by(`Ward Name`, `Breeding_Site_Recode`) %>%  # Group by breeding site type
#   summarize(
#     AvgLD = mean(`Larva_Density`, na.rm = TRUE)  # Average number of Anopheles caught per site
#   )

##Plot LD per site
lav_den_sum_wet$`Site Code` <- paste0(lav_den_sum_wet$`Household Code/Number`, 
                                        lav_den_sum_wet$`Breeding_Site_Recode`)

# # Customize Site Code Legend for better understanding

# Challenge: 10 "Tyre" , 125 "Drainage/Gutter/Ditch", 127 "Drainage/Gutter/Ditch ", 180 "Puddles"
# 228 "Drainage/Gutter/Ditch", 249 "Drainage/Gutter/Ditch", 6  "Plastic Bowls", 95 "Drainage/Gutter/Ditch"
# 98 "Drainage/Gutter/Ditch "
# 
# Agugu: 109 "Puddles", 117 "Tyre" , 166 "Drainage/Gutter/Ditch ", 167 "Plastic Bowls" , 190 "Puddles" 
# 207 "Drainage/Gutter/Ditch", 208 "Drainage/Gutter/Ditch" , 243 "Abandoned Well", 244 "Plastic Bowls"                 
# 246 "Canal", 247 "Open Tank", 248 "Puddles", 34 "Puddles", 35 "Puddles", 37 "Drainage/Gutter/Ditch" 
# 47 "Puddles", 57 "Unprotected Well"

##Overall by Settlement Type
lav_den_wet_s <- ggplot(lav_den_sum_wet)+
  #geom_bar(aes(x = `Type of breeding site.x`, y = Anopheles_caught), stat = "identity", fill = "plum", width = 0.5, position = "stack")+
  geom_point(aes(x = as.character(`Household Code/Number`), y = Larva_Density, group = 1), stat = "identity", color = "red", size = 3.5)+
  facet_wrap(~`Ward Name`)+
  geom_text(aes(x = as.character(`Household Code/Number`), y = Larva_Density, 
                label = round(Larva_Density,1)), 
            vjust = -0.7, color = "black", size = 3.5) +
  labs(title = "Distribution of breeding sites with presence of anopheles larva and \n corresponding larva density",
       x = "Breeding Site Code",
       #y = "Number of Breeding Sites")+
       #scale_y_continuous(
       # name = "Numberof breeding sites",  # Primary y-axis label
       #sec.axis = sec_axis(~ ./1, name = "Larval Density")  # Secondary y-axis with scaling
  ) +
  # scale_y_continuous(sec.axis=sec_axis(~./ 10, name = "Larval Density"))+
  theme_manuscript()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8))+
  theme(strip.background = element_rect(fill = "yellow", color = "black"))+
  ylim(0,100)

ggsave(paste0(LuDir, '/plots/', Sys.Date(), 'wet season larva density per breeding sites in Ibadan2.pdf'), lav_den_wet_s, width = 8, height = 6)

##Agugu
lav_den_wet_a <- ggplot(lav_den_sum_wet %>% dplyr::filter(`Ward Name` == "Agugu"))+
  #geom_bar(aes(x = `Type of breeding site.x`, y = Anopheles_caught), stat = "identity", fill = "plum", width = 0.5, position = "stack")+
  geom_point(aes(x = as.character(`Household Code/Number`), y = Larva_Density, group = 1), stat = "identity", color = "red", size = 3.5)+
  facet_wrap(~`Water Nature`)+
  geom_text(aes(x = as.character(`Household Code/Number`), y = Larva_Density, 
                label = round(Larva_Density,1)), 
            vjust = -0.7, color = "black", size = 3.5) +
  labs(title = "Distribution of breeding sites with presence of anopheles larva and \n corresponding larva density by water nature(Agugu)",
       x = "Breeding Site Code",
       #y = "Number of Breeding Sites")+
       #scale_y_continuous(
       # name = "Numberof breeding sites",  # Primary y-axis label
       #sec.axis = sec_axis(~ ./1, name = "Larval Density")  # Secondary y-axis with scaling
  ) +
  # scale_y_continuous(sec.axis=sec_axis(~./ 10, name = "Larval Density"))+
  theme_manuscript()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8))+
  theme(strip.background = element_rect(fill = "yellow", color = "black"))+
  ylim(0,100)

ggsave(paste0(LuDir, '/plots/', Sys.Date(), 'wet season larva density per breeding sites in Agugu(Ibadan).pdf'), lav_den_wet_a, width = 8, height = 6)

##Challenge
lav_den_wet_c <- ggplot(lav_den_sum_wet %>% dplyr::filter(`Ward Name` == "Challenge"))+
  #geom_bar(aes(x = `Type of breeding site.x`, y = Anopheles_caught), stat = "identity", fill = "plum", width = 0.5, position = "stack")+
  geom_point(aes(x = as.character(`Household Code/Number`), y = Larva_Density, group = 1), stat = "identity", color = "red", size = 3.5)+
  facet_wrap(~`Water Nature`)+
  geom_text(aes(x = as.character(`Household Code/Number`), y = Larva_Density, 
                label = round(Larva_Density,1)), 
            vjust = -0.7, color = "black", size = 3.5) +
  labs(title = "Distribution of breeding sites with presence of anopheles larva and \n corresponding larva density by water nature(Challenge)",
       x = "Breeding Site Code",
       #y = "Number of Breeding Sites")+
       #scale_y_continuous(
       # name = "Numberof breeding sites",  # Primary y-axis label
       #sec.axis = sec_axis(~ ./1, name = "Larval Density")  # Secondary y-axis with scaling
  ) +
  # scale_y_continuous(sec.axis=sec_axis(~./ 10, name = "Larval Density"))+
  theme_manuscript()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8))+
  theme(strip.background = element_rect(fill = "yellow", color = "black"))+
  ylim(0,100)

ggsave(paste0(LuDir, '/plots/', Sys.Date(), 'wet season larva density per breeding sites in Chall(Ibadan).pdf'), lav_den_wet_c, width = 8, height = 6)

##Breeding site characteristics and larval density
#Origin of water
ggplot(lav_den_sum_wet)+
  #geom_bar(aes(x = `Type of breeding site.x`, y = Anopheles_caught), stat = "identity", fill = "plum", width = 0.5, position = "stack")+
  geom_point(aes(x=`Site Code`, y = Larva_Density, group = 1), stat = "identity", color = "green", size = 3.5)+
  facet_wrap(~`Origin of Water`)+
  labs(title = "Distribution of Number of breeding sites with presence of anopheles larva \n
       and corresponding larva density by Origin of water",
       x = "Settlement Type",
       #y = "Number of Breeding Sites")+
       #scale_y_continuous(
       # name = "Numberof breeding sites",  # Primary y-axis label
       #sec.axis = sec_axis(~ ./1, name = "Larval Density")  # Secondary y-axis with scaling
  ) +
  # scale_y_continuous(sec.axis=sec_axis(~./ 10, name = "Larval Density"))+
  theme_manuscript()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8))+
  theme(strip.background = element_rect(fill = "plum", color = "black"))

##Water Nature
water_ano_caught_sum_wet <- lav_den_sum_wet %>% 
  group_by(`Breeding_Site_Recode`, `Water Nature`) %>%  # Group by breeding site type
  summarize(
    NoAnophelesPerSite = sum(`Number of Anopheles`, na.rm = TRUE)  # Average number of Anopheles caught per site
  ) 

ggplot(water_ano_caught_sum_wet)+
  geom_bar(aes(x = `Breeding_Site_Recode`, y = NoAnophelesPerSite), stat = "identity", fill = "plum", width = 0.5, position = "stack")+
  #geom_point(aes(x=`Breeding_Site_Recode`, y = Larva_Density, group = 1), stat = "identity", color = "cadetblue", size = 3.5)+
  facet_wrap(~`Water Nature`)+
  geom_text(aes(x = `Breeding_Site_Recode`, y = NoAnophelesPerSite, 
                label = NoAnophelesPerSite), 
            vjust = -0.5, color = "black", size = 3.5) +
  labs(title = "Distribution of Number of anopheles larva caught \n
       and corresponding by Water nature",
       x = "Settlement Type",
       #y = "Number of Breeding Sites")+
       #scale_y_continuous(
       # name = "Numberof breeding sites",  # Primary y-axis label
       #sec.axis = sec_axis(~ ./1, name = "Larval Density")  # Secondary y-axis with scaling
  ) +
  # scale_y_continuous(sec.axis=sec_axis(~./ 10, name = "Larval Density"))+
  theme_manuscript()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8))+
  theme(strip.background = element_rect(fill = "snow", color = "black"))






##Combine plots for presentation
##Further summary on larval data
library(ggplot2)

# Create the plot
ggplot(breeding_site_summary, aes(x = `Type of breeding site`)) +
  geom_bar(aes(y = NoAnophelesPerSite), stat = "identity", fill = "skyblue", width = 0.5) +  # Primary axis: bar plot for SitesVisited
  geom_text(aes(y = NoAnophelesPerSite, label = NoAnophelesPerSite), vjust = -0.2, hjust = 1.0, color = "blue")+
  geom_line(aes(y = SitesVisited * 10, group = 1), color = "red", size = 1) +  # Secondary axis: line plot for AvgAnophelesPerSite, scaled for visibility
  geom_point(aes(y = SitesVisited * 10), color = "red", size = 3) +  # Points on the line plot for AvgAnophelesPerSite
  geom_text(aes(y = SitesVisited * 10, label = round(SitesVisited, 1)), 
            color = "black", vjust = -1)+
  facet_wrap(~ `Ward Name`, scales = "free_x")+
  scale_y_continuous(
    name = "NoAnophelesPerSite",  # Primary y-axis label
    sec.axis = sec_axis(~ . / 10, name = "No Breeding Site Visited")  # Secondary y-axis with scaling
  ) +
  labs(x = "Breeding Site Type", title = "Summary of Anopheles Captured by Breeding Site Type") +
  theme_manuscript()+
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),  # Adjust x-axis label angle and size
    legend.position = "none",  # Remove the legend
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6)
  )


###AGUGU-ANALYSIS
##Extract households with a known distance from selected LBS points
# Choose the particular point from points_6 (e.g., the first point)
psc_w_a_df_y <- ento_psc_w_all_df %>% 
  dplyr::filter(`Ward Name` == "Agugu", Anopheles_Caught =="Yes")

psc_w_a_df_n <- ento_psc_w_all_df %>% 
  dplyr::filter(`Ward Name` == "Agugu", Anopheles_Caught =="No") %>% 
  distinct(geometry, .keep_all = TRUE)

lav_w_a_df_y <- lav_a_dffw %>% 
  dplyr::filter(Anopheles_Caught =="Yes")

lav_w_a_df_n <- lav_a_dffw %>% 
  dplyr::filter(Anopheles_Caught =="No")

particular_psc <-psc_w_a_df_y[18, ]


##Map circles around specific LBS points

# Transform to a CRS with meters as units (e.g., UTM)
lav_wsites_utm<- st_transform(lav_w_a_df_n, crs = 32633)  # Replace with appropriate UTM zone for your area
df_ib_a_utm <- st_transform(df_ib_a, crs = 32633)  # Same UTM zone
ag_lav_wsites_an_utm <- st_transform(lav_w_a_df_y, crs = 32633)
psc_pt_wa_utm <- st_transform(psc_w_a_df_y, crs = 32633)
psc_bpt_wa_utm <- st_transform(particular_psc, crs = 32633)##Buffer point

# Create 5-meter buffers around each point
#buffers <- st_buffer(cdc_pt_utm, dist = 100)

# # Plot using ggplot2
# ggplot() +
#   geom_sf(data = df_ib_a_utm, fill = "lightblue", color = "black") +  # Shapefile
#   geom_sf(data = buffers, fill = NA, color = "red") +  # Buffers
#   geom_sf(data = lav_sites_utm, color = "blue", size = 2) +  # Points
#   labs(title = "50m Buffers Around Geo-Points on Shapefile") +
#   theme_manuscript()

# Assuming you have another dataframe with points, e.g., `other_points_utm`

# Create 100-meter buffers around each point
awbuffers <- st_buffer(psc_bpt_wa_utm, dist = 200)

# Dummy data to create a manual legend
awlegend_data <- data.frame(
  label = c("PSC HH", "Breeding sites with no Larval", "Breeding sites with Larval"),
  color = c("seagreen", "blue", "coral"),
  shape = c(16, 16, 17)
)

# Plot using ggplot2
buf_agu_wet <- ggplot() +
  geom_sf(data = df_ib_a_utm, fill = "aliceblue", color = "black") +  # Shapefile
  geom_sf(data =psc_bpt_wa_utm, color = "seagreen", size = 2) +  # CDC HHs
  #geom_sf(data =psc_a_dff, color = "brown", size = 3) +  # PSC HHs
  geom_sf(data =awbuffers, fill = NA,  color = "red") +  # Buffers
  geom_sf(data = lav_wsites_utm, color = "blue", size = 2, alpha = 0.7) +  # Points from original dataframe
  geom_sf(data = ag_lav_wsites_an_utm, color = "coral", size = 2.5, shape = 17) +  # Points from the other dataframe
  geom_point(data = awlegend_data, aes(x = Inf, y = Inf, color = label), size = 3) +  # Invisible points for legend
  scale_color_manual(
    name = "Legend",
    values = c("PSC HH" = "seagreen", "Breeding sites with no Larval" = "blue", "Breeding sites with Larval" = "coral")
  ) +
  labs(title = "Geo-location of breeding sites covered within 300m of a household with adult mosquito") +
  theme_manuscript()

ggsave(paste0(LuDir, '/plots/', Sys.Date(), 'Geo-location and buffer of breeding sites in Agugu(wet).pdf'), buf_agu_wet, width = 10, height = 11)


##Mapping out the polygon of area of visit
# Create a convex hull around the breeding sites
wabreeding_sites_hull <- st_convex_hull(st_union(lav_a_dffw))

# Intersect the convex hull with the shapefile to limit the polygon to a specific area
wabreeding_sites_polygon <- st_intersection(wabreeding_sites_hull, df_ib_a)

library(ggplot2)

geo_agu_wet <- ggplot() +
  geom_sf(data = df_ib_a, fill = "aliceblue", color = "black") +  # Background shapefile
  geom_sf(data = lav_w_a_df_n, color = "lightgreen", size = 2, alpha = 0.7) +  # Breeding sites with no larval
  geom_sf(data = lav_w_a_df_y, color = "red", size = 2) +  # Breeding sites with Larval
  geom_sf(data = wabreeding_sites_polygon, fill = NA, color = "brown", size = 3) +  # Convex hull polygon
  geom_point(data = aglegend_data, aes(x = Inf, y = Inf, color = label), size = 3) +  # Invisible points for legend
  scale_color_manual(
    name = "Legend",
    values = c("Breeding sites with no Larval" = "seagreen", "Breeding sites with Larval" = "red")
  ) +
  labs(title = "Geo-location of area covered during larval prospection and breeding sites with anopheles larva(wet season)") +
  map_theme()+
  theme_manuscript()

ggsave(paste0(LuDir, '/plots/', Sys.Date(), 'Geo-location covered by breeding sites in Agugu(wet).pdf'), geo_agu_wet, width = 10, height = 11)

# Calculate the area of the buffer in square meters
wabuffer_area <- st_area(wabreeding_sites_polygon)
wabuffer_area_sqm <- as.numeric(wabuffer_area)  # Convert to numeric if necessary

##Convert area to square kilometer
wabuffer_area_sqkm <- wabuffer_area_sqm / 1e6

# Number of breeding sites with larvae caught
larva_sites_count <- length(ag_lav_sites_an_utm)
walarva_sites_count <- 15
# Total number of breeding sites visited (assuming this is available)
watotal_sites_count <-107  # Replace with your actual variable

# Step 3: Calculate the proportion of sites with larvae
walarva_proportion <- larva_sites_count / total_sites_count

# Estimate the density of larva breeding sites per square meter
wadensity_per_sqkm <- walarva_sites_count / wabuffer_area_sqkm

# Step 5: Adjust the density considering the total sites visited
wadjusted_density_per_sqkm <- wadensity_per_sqkm * walarva_proportion


###CHALLENGE:-ANALYSIS

##Extract households with a known distance from selected LBS points
# Choose the particular point from points_6 (e.g., the first point)
psc_w_c_df_y <- ento_psc_w_all_df %>% 
  dplyr::filter(`Ward Name` == "Challenge", Anopheles_Caught =="Yes")

psc_w_c_df_n <- ento_psc_w_all_df %>% 
  dplyr::filter(`Ward Name` == "Challenge", Anopheles_Caught =="No") %>% 
  distinct(geometry, .keep_all = TRUE)

lav_w_c_df_y <- lav_c_dffw %>% 
  dplyr::filter(Anopheles_Caught =="Yes")

lav_w_c_df_n <- lav_c_dffw %>% 
  dplyr::filter(Anopheles_Caught =="No")

particular_psc <-psc_w_c_df_y[6, ]

##Mapping out the polygon of area of visit
# Create a convex hull around the breeding sites
wcbreeding_sites_hull <- st_convex_hull(st_union(lav_c_dffw))

# Intersect the convex hull with the shapefile to limit the polygon to a specific area
wcbreeding_sites_polygon <- st_intersection(wcbreeding_sites_hull, df_ib_c)

library(ggplot2)
# Dummy data to create a manual legend
cllegend_data <- data.frame(
  label = c("Breeding sites with no Larval", "Breeding sites with Larval"),
  color = c("seagreen", "red"),
  shape = c(16, 18)
)

geo_chal_wet <- ggplot() +
  geom_sf(data = df_ib_c, fill = "aliceblue", color = "black") +  # Background shapefile
  geom_sf(data = lav_w_c_df_n, color = "seagreen", size = 2, alpha = 0.7) +  # Breeding sites with no larval
  geom_sf(data = lav_w_c_df_y, color = "red", size = 2) +  # Breeding sites with Larval
  geom_sf(data = wcbreeding_sites_polygon, fill = NA, color = "brown", size = 3) +  # Convex hull polygon
  geom_point(data = cllegend_data, aes(x = Inf, y = Inf, color = label), size = 3) +  # Invisible points for legend
  scale_color_manual(
    name = "Legend",
    values = c("Breeding sites with no Larval" = "seagreen", "Breeding sites with Larval" = "red")
  ) +
  labs(title = "Geo-location of area covered during larval prospection and breeding sites with anopheles larva(wet season)") +
  map_theme()+
  theme_manuscript()

ggsave(paste0(LuDir, '/plots/', Sys.Date(), 'Geo-location covered by breeding sites in Challenge.pdf'), geo_chal_wet, width = 10, height = 11)


##Computing estimates of the density per geo-location

# Transform to UTM (example using UTM zone 32N, EPSG:32632)
wcbreeding_sites_polygon_utm <- st_transform(wcbreeding_sites_polygon, crs = 32632)

# Calculate the area of the buffer in square meters
buffer_area_chal_sqm <- st_area(wcbreeding_sites_polygon_utm)
buffer_area_chal_sqm <- as.numeric(buffer_area_chal_sqm)  # Convert to numeric if necessary

##Convert to square kilometer
buffer_area_chal_sqkm <- buffer_area_chal_sqm / 1e6

# Number of breeding sites with larvae caught
chal_larva_sites_count <- dim(lav_w_c_df_y)[1]
chal_larva_sites_count <- 9
# Total number of breeding sites visited (assuming this is available)
chal_total_sites_count <- dim(lav_c_dffw)[1]
chal_total_sites_count <-117  # Replace with your actual variable

# Step 3: Calculate the proportion of sites with larvae
chal_larva_proportion <- chal_larva_sites_count / chal_total_sites_count

# Estimate the density of larva breeding sites per square meter
chal_density_per_sqkm <- chal_larva_sites_count / buffer_area_chal_sqkm

# Step 5: Adjust the density considering the total sites visited
chal_adjusted_density_per_sqkm <- chal_density_per_sqkm * chal_larva_proportion




##Map circles around specific LBS points

# Transform to a CRS with meters as units (e.g., UTM)
lav_sites_c_utm <- st_transform(lav_w_c_df_n, crs = 32633)  # Replace with appropriate UTM zone for your area
df_ib_c_utm <- st_transform(df_ib_c, crs = 32633)  # Same UTM zone
chal_lav_sites_an_utm <- st_transform(lav_w_c_df_y, crs = 32633)
psc_pt_c_utm <- st_transform(psc_w_c_df_y, crs = 32633)

# Assuming you have another dataframe with points, e.g., `other_points_utm`

# Create 100-meter buffers around each point
chlbuffers <- st_buffer(particular_psc, dist = 300)

st_area(chlbuffers)

library(ggplot2)
library(sf)

# Assuming your map_theme() and theme_manuscript() functions are already defined

# Dummy data to create a manual legend
chlegend_data <- data.frame(
  label = c("PSC HH", "Breeding sites with no Larval", "Breeding sites with Larval"),
  color = c("brown", "blue", "coral"),
  shape = c(16, 16, 17)
)

# Adding the manual legend
buf_chal_wet <- ggplot() +
  geom_sf(data = df_ib_c_utm, fill = "aliceblue", color = "black")+  # Shapefile
  geom_sf(data = particular_psc, color = "brown", size = 2) +  # PSC HHs
  geom_sf(data = chlbuffers, fill = NA,  color = "red") +  # Buffers
  geom_sf(data = lav_sites_c_utm, color = "blue", size = 2, alpha = 0.7) +  # Original Points
  geom_sf(data = chal_lav_sites_an_utm, color = "coral", size = 2.5, shape = 17) +  # Other Points
  geom_point(data = chlegend_data, aes(x = Inf, y = Inf, color = label), size = 3) +  # Invisible points for legend
  scale_color_manual(
    name = "Legend",
    values = c("PSC HH" = "brown", "Breeding sites with no Larval" = "blue", "Breeding sites with Larval" = "coral")
  ) + 
  # scale_shape_manual(
  #   name = "Legend",
  #   values = c("PSC HHs" = 16, "Breeding sites with no Larval" = 16, "Breeding sites with Larval" = 17)
  # ) +
  labs(title = "Geo-location of breeding sites covered within 300m of a PSC household") +
  map_theme() +
  theme_manuscript()

ggsave(paste0(LuDir, '/plots/', Sys.Date(), 'Geo-location and buffer of breeding sites in Challenge.pdf'), buf_chal_wet, width = 8, height = 11)



# Extract coordinates
coords_lav_sites <- st_coordinates(lav_a_dff)

write.csv(coords_lav_sites, file.path(LuDir, "lav_coords.csv"))

