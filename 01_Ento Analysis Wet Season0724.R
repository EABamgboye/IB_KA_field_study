user <- Sys.getenv("USERNAME")
Drive <- file.path(gsub("[\\]", "/", gsub("Documents", "", Sys.getenv("HOME"))))
LuDir <- file.path(Drive, "Documents")
LuPDir <- file.path(Drive, "Downloads")


###Ento Wet Season collection Q and A

library(readxl)

##CDC Data
ento_cdc_w_all <- read_excel(file.path(LuPDir , "WET_SEASON_ENTO_COLLECTION_CDC_-_all_versions_-_labels_-_2024-08-12-21-20-44.xlsx")) %>% 
  slice(-(1:3))

#Week 1
unique_cdc_wet1 <- ento_cdc_w_all %>% 
  dplyr::filter(`Night of Data Collection` == 5,
                `Location of CDC` == "Outdoor")

ento_cdc_w1 <- unique_cdc_wet1 %>% 
  dplyr::select("Ward Name",                                          
                "Settlement Type", 
                "Household Code/Number",
                "_Household coordinates_latitude",
                "_Household coordinates_longitude" 
  ) 

ento_cdc_wet1 <- ento_cdc_w1 %>% 
  mutate(type = "CDC")

colnames(ento_cdc_wet1)[4] <- "Latitude"
colnames(ento_cdc_wet1)[5] <- "Longitude"


##Week 2
unique_cdc_wet2 <- ento_cdc_w_all %>% 
  dplyr::filter(`Night of Data Collection` == 15,
                `Location of CDC` == "Outdoor")

ento_cdc_w2 <- unique_cdc_wet2 %>% 
  dplyr::select("Ward Name",                                          
                "Settlement Type", 
                "Household Code/Number",
                "_Household coordinates_latitude",
                "_Household coordinates_longitude" 
  ) 

ento_cdc_wet2 <- ento_cdc_w2 %>% 
  mutate(type = "CDC2")

colnames(ento_cdc_wet2)[4] <- "Latitude"
colnames(ento_cdc_wet2)[5] <- "Longitude"



##PSC Data
ento_psc_w_all <- read_excel(file.path(LuPDir , "WET_SEASON_ENTO_COLLECTION_PSC_-_all_versions_-_labels_-_2024-08-12-21-20-23.xlsx")) %>% 
  slice(-(1:4))

ento_psc_w_all <- ento_psc_w_all %>% 
  mutate(Anopheles_Caught = ifelse(`Total Number of Anopheles` > 0, "Yes", "No"))

ento_psc_w_all_df <- sf::st_as_sf(ento_psc_w_all, coords=c('_Household coordinates_longitude', 
                                                           '_Household coordinates_latitude'), crs=4326)

ento_psc_w <- ento_psc_w_all %>% 
  dplyr::select("Ward Name",                                          
                "Settlement Type",
                "Household Code/Number",
                "_Household coordinates_latitude",
                "_Household coordinates_longitude" 
  )

#ento_psc_w <- slice(ento_psc_w, -(1:4))

ento_psc_wet <- ento_psc_w %>% 
  mutate(type = "PSC")

colnames(ento_psc_wet)[4] <- "Latitude"
colnames(ento_psc_wet)[5] <- "Longitude"


##Larval Data
ento_lav_w_all <- read_excel(file.path(LuPDir , "WET_SEASON_ENTO_COLLECTION_LARVAL_PROSPECTION_-_all_versions_-_labels_-_2024-08-12-21-21-06.xlsx"))

ento_lav_w_all <- ento_lav_w_all %>% 
  mutate(`Household Code/Number` = 1:272)

ento_lav_w_all <- slice(ento_lav_w_all, -(1:2))

ento_lav_w_all <- slice(ento_lav_w_all, -(6))

ento_lav_w_all <- ento_lav_w_all %>% 
  mutate(Anopheles_Caught = ifelse(`Number of Anopheles` > 0, "Yes", "No"))

ento_lav_w <- ento_lav_w_all %>% 
  dplyr::select("Ward Name",                                          
                "Settlement Type",
                "Household Code/Number",
                "_Breeding site coordinates_latitude",
                "_Breeding site coordinates_longitude",
                "Type of breeding site",
                "Anopheles_Caught",
  ) 

#ento_lav_w <- slice(ento_lav_w, -(1:2))

ento_lav_wet <- ento_lav_w %>% 
  mutate(type = "LAV")

colnames(ento_lav_wet)[4] <- "Latitude"
colnames(ento_lav_wet)[5] <- "Longitude"

ento_lav_wet_df <- sf::st_as_sf(ento_lav_wet, coords=c('Longitude', 'Latitude'), crs=4326)

ento_lav_wet_df <- sf::st_as_sf(ento_lav_wet, coords=c('Longitude', 'Latitude'), crs=4326)

##Combining CDC and PSC 
ento_wet <- rbind(ento_cdc_wet1, ento_cdc_wet2, ento_psc_wet, ento_lav_wet)

ento_wet_df <- sf::st_as_sf(ento_wet, coords=c('Longitude', 'Latitude'), crs=4326)

## read ibadan ward shape files
df_ib <- st_read(file.path(LuDir, "kano_ibadan_shape_files", "ibadan_metro_ward_fiveLGAs", "Ibadan_metro_fiveLGAs.shp")) %>%
  mutate(WardName = ifelse(WardName == 'Oranyan' & LGACode == '31007', 'Oranyan_7', WardName))

p <- ggplot(df_ib) +
  geom_sf(fill = NA) +
  geom_text_repel(
    data = df_ib,
    aes(label =  WardName, geometry = geometry),color ='black',
    stat = "sf_coordinates", min.segment.length = 0, size = 3.5, force = 1)+
  map_theme()+ 
  labs(title= "Wards in Ibadan ")+
  coord_sf()

##Split Ibadan shapefile into working wards
df_ib_c <- df_ib %>%
  dplyr::filter(WardName == 'Challenge')

df_ib_a <- df_ib %>%
  dplyr::filter(WardName == 'Agugu')

df_ib_o <- df_ib %>%
  dplyr::filter(WardName == 'Olopomewa')


st_write(df_ib_a, file.path(LuDir, "Agugu.shp"))

#Perform st_transformation
st_crs(df_ib_a) <- 4326

st_crs(df_ib_c) <- 4326

st_crs(ento_wet_df) <- 4326

st_crs(ento_lav_wet_df) <- 4326

ento_wet_df_int <- st_intersection(ento_wet_df, df_ib )

ento_wet_df_int_a <- st_intersection(ento_wet_df, df_ib_a )

ento_wet_df_int_c <- st_intersection(ento_wet_df, df_ib_c )


##Fill missing entry
if (is.na(ento_wet_df_int_a[206, 2]) || df[206, 2] == "") {
  ento_lav_w_all[206, 2] <- "Slum"
}

##Plot location(Agugu)
ggplot(df_ib_a) +
  geom_sf(fill= "NA")+
  geom_point(data= ento_wet_df_int_a ,  aes(geometry = geometry, size = 0.05, alpha = 0.01, col = `type`, shape = `Settlement.Type`), stat= "sf_coordinates")+
  scale_color_manual(values = c(CDC = "navyblue", CDC2 = "seagreen",  PSC = "tomato", LAV = "plum"))+
  scale_shape_manual(values = c(Formal = 16,  Informal= 17, Slum = 14))+
  geom_text_repel(
    data = df_ib_a,
    aes(label =  `WardName`, geometry = geometry),color ='black',
    stat = "sf_coordinates", min.segment.length = 0, size = 2.5, force = 1, max.overlaps = Inf)+
  guides(alpha = FALSE, size = FALSE) +
  map_theme()+ 
  ylab("")+
  xlab("")+
  labs(title= "Sites for Wet Season Ento(Agugu)")+
  coord_sf()

##Plot Location by Type of Collection
##CDC 
ggplot(df_ib_a) +
  geom_sf(fill= "NA")+
  geom_point(data= dplyr::filter(ento_wet_df_int_a, type== "CDC" | type == "CDC2") ,  aes(geometry = geometry, size = 0.05, alpha = 0.01, col = `type`, shape = `Settlement.Type`), stat= "sf_coordinates")+
  scale_color_manual(values = c(CDC = "navyblue", CDC2 = "seagreen"))+
  scale_shape_manual(values = c(Formal = 16,  Informal= 17, Slum = 14))+
  geom_text_repel(
    data = df_ib_a,
    aes(label =  `WardName`, geometry = geometry),color ='black',
    stat = "sf_coordinates", min.segment.length = 0, size = 2.5, force = 1, max.overlaps = Inf)+
  guides(alpha = FALSE, size = FALSE) +
  map_theme()+ 
  ylab("")+
  xlab("")+
  labs(title= "Sites for Wet Season CDC Ento Collection(Agugu)")+
  coord_sf()


##Plot location(Challenge)
ggplot(df_ib_c) +
  geom_sf(fill= NA)+
  geom_point(data= ento_wet_df_int_c ,  aes(geometry = geometry, size = 0.01, alpha = 0.001, col = `type`, shape = `Settlement.Type`), stat= "sf_coordinates")+
  scale_color_manual(values = c(CDC = "navyblue", CDC2 = "seagreen", PSC = "tomato", LAV = "plum"))+
  scale_shape_manual(values = c(Formal = 16,  Informal= 17, Slum = 14))+
  geom_text_repel(
    data = df_ib_c,
    aes(label =  `WardName`, geometry = geometry),color ='black',
    stat = "sf_coordinates", min.segment.length = 0, size = 2.5, force = 1, max.overlaps = Inf)+
  guides(alpha = FALSE, size = FALSE) +
  map_theme()+ 
  ylab("")+
  xlab("")+
  labs(title= "Sites for Wet Season Ento (Challenge)")+
  coord_sf()


##Plot Location by Type of Collection
##CDC 
ggplot(df_ib_c) +
  geom_sf(fill= "NA")+
  geom_point(data= dplyr::filter(ento_wet_df_int_c, type== "CDC" | type == "CDC2") ,  aes(geometry = geometry, size = 0.05, alpha = 0.01, col = `type`, shape = `Settlement.Type`), stat= "sf_coordinates")+
  scale_color_manual(values = c(CDC = "navyblue", CDC2 = "seagreen"))+
  scale_shape_manual(values = c(Formal = 16,  Informal= 17, Slum = 14))+
  geom_text_repel(
    data = df_ib_c,
    aes(label =  `WardName`, geometry = geometry),color ='black',
    stat = "sf_coordinates", min.segment.length = 0, size = 2.5, force = 1, max.overlaps = Inf)+
  guides(alpha = FALSE, size = FALSE) +
  map_theme()+ 
  ylab("")+
  xlab("")+
  labs(title= "Sites for Wet Season CDC Ento Collection(Challenge)")+
  coord_sf()




##Data Exploration
##LARVAL DATA
##Plot Location 

ento_lav_wet_df_int_a <- st_intersection(ento_lav_wet_df, df_ib_a )
 
ento_lav_wet_df_int_c <- st_intersection(ento_lav_wet_df, df_ib_c )

ggplot(df_ib_a) +
  geom_sf(fill= "NA")+
  geom_point(data= ento_lav_wet_df_int_a,  aes(geometry = geometry, size = 0.05, alpha = 0.01, col = `Anopheles_Caught`), stat= "sf_coordinates")+
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


##Plot location(Challenge)
ggplot(df_ib_c) +
  geom_sf(fill= NA)+
  geom_point(data= ento_lav_wet_df_int_c,  aes(geometry = geometry, size = 0.05, alpha = 0.01, col = `Anopheles_Caught`), stat= "sf_coordinates")+
  scale_color_manual(values = c(Yes = "seagreen", No = "red"))+
  #scale_shape_manual(values = c(Formal = 16,  Informal= 17, Slum = 14))+
  geom_text_repel(
    data = df_ib_c,
    aes(label =  `WardName`, geometry = geometry),color ='black',
    stat = "sf_coordinates", min.segment.length = 0, size = 2.5, force = 1, max.overlaps = Inf)+
  guides(alpha = FALSE, size = FALSE) +
  map_theme()+ 
  ylab("")+
  xlab("")+
  labs(title= "Sites for Wet Season Larval Collection (Challenge)")+
  coord_sf()



##Fill missing entry
if (is.na(ento_lav_w_all[184, 6]) || ento_lav_w_all[184, 6] == "") {
  ento_lav_w_all[184, 6] <- "Slum"
}

ento_lav_w_all[163, 6] <- "Slum"

##Larval Habitat - Ibadan
lav_ib <- ento_lav_w_all %>%  
  group_by(`Settlement Type`, `Ward Name`) %>% 
  summarise(total_mosquitoes = sum(`Number of Anopheles`, na.rm = T)) %>% 
  ungroup()


p <- ggplot(data=lav_ib, aes(x= `Ward Name`, y=total_mosquitoes, group = `Settlement Type`,
                             colour = `Settlement Type`))+
  geom_point(size = 5.0)+
  geom_text(aes(label= total_mosquitoes), vjust = -0.5, hjust = 1.2, size = 3.5) +
  labs(y= "Total number of larva of anopheles mosquitos caught", x = "Location of Collection (Ibadan)")+
  ggtitle("Larva of Anopheles mosquito collected through Larva Prospection, July-August, 2024")+
  theme(plot.title = element_text(size = 12))+
  theme_manuscript() +
  theme(
    legend.position = c(.95, .95),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6)
  ) +
  ylim(0,1000)


##Breeding Site Analysis
# ##Replace wrong entry
# ento_lav_w_all[87, 6] <- "Slum"
# ento_lav_w_all[163, 6] <- "Slum"


##Number of Anopheles per breeding site
df_bss <- ento_lav_w_all %>% 
  group_by(`Settlement Type`, `Ward Name`, `Type of breeding site`, `Anopheles_Caught`) %>% 
  summarise(total_mosquitoes = sum(`Number of Anopheles`, na.rm = T)) %>% 
  ungroup()

##Fill missing entry
if (is.na(ento_lav_w_all[35, 8]) || ento_lav_w_all[35, 8] == "") {
  ento_lav_w_all[35, 8] <- "Puddles"
}

if (is.na(ento_lav_w_all[157, 8]) || ento_lav_w_all[157, 8] == "") {
  ento_lav_w_all[157, 8] <- "Gutter"
}

if (is.na(ento_lav_w_all[184, 6]) || ento_lav_w_all[184, 6] == "") {
  ento_lav_w_all[184, 6] <- "Slum"
}

ento_lav_w_all[163, 6] <- "Slum"

##Number of Breeding sites where larva were caught
##Recode breeding sites to macth Dry Season
ento_lav_w_all <- ento_lav_w_all %>% 
  mutate(Breeding_Site_Recode = recode(`Type of breeding site`,
                                       "Drainage" = "Drainage/Gutter/Ditch",
                                       "Gutter" = "Drainage/Gutter/Ditch",
                                       "Ditch" = "Drainage/Gutter/Ditch"))

# Summarize data by breeding site type
breeding_site_summary <- ento_lav_w_all %>%
  group_by(`Settlement Type`, `Ward Name`,`Breeding_Site_Recode`, `Anopheles_Caught`) %>%  # Group by breeding site type
  summarize(
    TotalAnophelesCaught = sum(`Number of Anopheles`, na.rm = TRUE),  # Total number of Anopheles caught per type
    SitesVisited = n(),  # Number of sites visited per type
    NoAnophelesPerSite = sum(`Number of Anopheles`, na.rm = TRUE)  # Average number of Anopheles caught per site
  ) 

ggplot(data=breeding_site_summary, aes(x = Breeding_Site_Recode, y=NoAnophelesPerSite, fill = `Breeding_Site_Recode`)) +
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
  ggtitle("Type of breeding sites where anopheles larva were caught in Ibadan, July-August, 2024")



ggplot(data=breeding_site_summary, aes(x=`Breeding_Site_Recode`, y=SitesVisited, fill = `Breeding_Site_Recode`)) +
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
  ggtitle("Number of breeding sites visted in Ibadan, July-August, 2024")


ggplot(data=breeding_site_summary, aes(x=`Breeding_Site_Recode`, y=SitesVisited, fill = `Anopheles_Caught`)) +
  geom_bar(stat="identity", position = "stack")+
  facet_wrap(~ `Ward Name`, scales = "free_x")+
  geom_text(aes(label=SitesVisited), vjust = 0.7,
            color="black", size=2.5)+
  theme_manuscript()+
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),  # Adjust x-axis label angle and size
    #legend.position = "none",  # Remove the legend
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6)
  ) +
  ylim(0,100)+
  ggtitle("Number of breeding sites visted and number where /n anopheles larva were found in Ibadan, July-August, 2024")

library(ggplot2)
library(dplyr)

ggplot(data = df_bss %>% filter(`Ward Name` == "Challenge"), aes(x = `Ward Name`, y = total_mosquitoes, fill = `Type of breeding site`)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  geom_text(aes(label = total_mosquitoes), 
            position = position_dodge(width = 0.9), 
            vjust = -0.5, 
            color = "black", 
            size = 3.5) +
  ylim(0, 100) +
  ggtitle("Distribution of breeding sites visited in Ibadan, July-August, 2024") +
  theme_manuscript()


ggplot(data = df_bss %>% filter(`Ward Name` == "Agugu"), aes(x = `Type of breeding site`, y = total_mosquitoes, fill = `Type of breeding site`)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  geom_text(aes(label = total_mosquitoes), 
            position = position_dodge(width = 0.9), 
            vjust = -0.5, 
            color = "black", 
            size = 3.5) +
  ylim(0, 100) +
  ggtitle("Distribution of breeding sites visited in Ibadan, July_August, 2024") +
  theme_manuscript()

# ento_lav_wet_df <- sf::st_as_sf(ento_lav_w_all, coords=c('_Breeding site coordinates_longitude', '_Breeding site coordinates_latitude'), crs=4326)
# 
# ggplot(df_ib_a) +
#   geom_sf(fill= NA)+
#   geom_point(data= dplyr::filter(ento_lav_wet_df, `Ward Name` == "Agugu", Anopheles_Caught == "Yes"),  aes(geometry = geometry, size = `Number of Anopheles`, alpha = 0.001, col = `Settlement Type`), stat= "sf_coordinates")+
#   scale_color_manual(values = c(Formal = "navyblue", Informal = "tomato", Slum = "plum"))+
#   #scale_shape_manual(values = c(Formal = 16,  Informal= 17, Slum = 14))+
#   # geom_text_repel(
#   #           data = ento_lav_wet_df,
#   #           aes(label =  `Type of breeding site`, geometry = geometry),color ='black',
#   #           stat = "sf_coordinates", min.segment.length = 0, size = 2.5, force = 1, max.overlaps = Inf)+
#   #           guides(alpha = FALSE, size = FALSE) +
#   map_theme()+ 
#   ylab("")+
#   xlab("")+
#   labs(title= "Sites for Wet Season Ento (Larval)")+
#   coord_sf()
# 
# 
# ggplot(df_ib_c) +
#   geom_sf(fill= NA)+
#   geom_point(data= dplyr::filter(ento_lav_wet_df, `Ward Name` == "Challenge", Anopheles_Caught == "Yes"),  aes(geometry = geometry, size = `Number of Anopheles`, alpha = 0.001, col = `Settlement Type`), stat= "sf_coordinates")+
#   scale_color_manual(values = c(Formal = "navyblue", Informal = "tomato", Slum = "plum"))+
#   #scale_shape_manual(values = c(Formal = 16,  Informal= 17, Slum = 14))+
#   # geom_text_repel(
#   #           data = ento_lav_wet_df,
#   #           aes(label =  `Type of breeding site`, geometry = geometry),color ='black',
#   #           stat = "sf_coordinates", min.segment.length = 0, size = 2.5, force = 1, max.overlaps = Inf)+
#   #           guides(alpha = FALSE, size = FALSE) +
#   map_theme()+ 
#   ylab("")+
#   xlab("")+
#   labs(title= "Sites for Wet Season Ento (Larval)")+
#   coord_sf()





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



##Calculations(Larval density)
view(lav)

subset_lav <- ento_lav_w_all[ento_lav_w_all$`Number of Anopheles` > 0, ]

lav_dips_sb <- subset_lav %>%  group_by(`Household Code/Number`, `Settlement Type`, `Type of breeding site`) %>% 
  summarise(total_dips = sum(`Number of Dips`)) %>% ungroup()

lav_dips_s <- subset_lav %>%  dplyr::filter(State == "Oyo") %>%  group_by(`Settlement Classification`) %>% 
  summarise(total_dips = sum(`No of dips`)) %>% ungroup()

Ano_catch_s <- subset_lav %>% group_by(`Settlement Classification`) %>% 
  summarise(Anopheles_caught = sum(`Anopheles`)) %>% ungroup()

Ano_catch_sb <- subset_lav %>% group_by(`Household Code/Number`,`Settlement Type`, `Type of breeding site`) %>% 
  summarise(Anopheles_caught = sum(`Number of Anopheles`)) %>% ungroup()

lav_den_df <- merge(lav_dips_sb, Ano_catch_sb, by = "Household Code/Number")

lav_den_sum <- lav_den_df %>% 
  mutate(Larva_Density = Anopheles_caught/total_dips)

# divide_columns <- function(Ano_catch_sb, Anopheles_caught, lav_dips_sb, total_dips) {
#   result <- Ano_catch_sb[[Anopheles_caught]] / lav_dips_sb[[total_dips]]
#   return(result)
# }
# 
# result <- divide_columns(Ano_catch_sb, "Anopheles_caught", lav_dips_sb, "total_dips")


##Larval Density
sett <- c("Formal", "Informal", "Slum", "Formal", "Informal", "Slum")
meth <- c("Drainage", "Drainage", "Drainage", "Tyre Tracks", "Tyre Tracks", "Tyre Tracks")
ld <- c(1.93, 0, 7.6, 0, 0, 2.7)
freq <- c(3, 0, 2, 0, 0, 1)

ld_df <- data.frame(sett, meth, ld, freq)

ggplot(lav_den_sum)+
  #geom_bar(aes(x = `Type of breeding site.x`, y = Anopheles_caught), stat = "identity", fill = "plum", width = 0.5, position = "stack")+
  geom_line(aes(x=`Type of breeding site.x`, y = Larva_Density, group = 1), stat = "identity", color = "red", size = 1.5)+
  facet_wrap(~`Settlement Type.x`)+
  labs(title = "Distribution of Number of breeding sites with presence of anopheles larva \n
       and corresponding larva density by settlement type",
       x = "Settlement Type",
       y = "Number of Breeding Sites")+
  scale_y_continuous(
    name = "Numberof breeding sites",  # Primary y-axis label
    sec.axis = sec_axis(~ ./10, name = "Larval Density")  # Secondary y-axis with scaling
  ) +
  # scale_y_continuous(sec.axis=sec_axis(~./ 10, name = "Larval Density"))+
  theme_manuscript()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8))+
  theme(strip.background = element_rect(fill = "yellow", color = "black"))


ggplot(ld_df)+
  geom_bar(aes(x = sett, y = freq), stat = "identity", fill = "plum", width = 0.5, position = "stack")+
  geom_line(aes(x=sett, y = ld, group = 1), stat = "identity", color = "red", size = 3.0)+
  facet_wrap(~meth)+
  geom_text(aes(x=sett, y = ld, label = ld), size = 4.5, vjust = 0.8, hjust = 1.3, color = "blue")+
  labs(title = "Distribution of Number of breeding sites with presence of anopheles larva \n
       and corresponding larva density by settlement type",
       x = "Settlement Type",
       y = "Number of Breeding Sites")+
  scale_y_continuous(sec.axis=sec_axis(~.* 1, name = "Larval Density"))+
  theme_manuscript()+
  theme(strip.background = element_rect(fill = "yellow", color = "black"))


# library(dplyr)
# 
# # Summarize data by breeding site type
# breeding_site_summary <- ento_lav_w_all %>%
#   group_by(`Settlement Type`, `Ward Name`,`Type of breeding site`) %>%  # Group by breeding site type
#   summarize(
#     TotalAnophelesCaught = sum(`Number of Anopheles`, na.rm = TRUE),  # Total number of Anopheles caught per type
#     SitesVisited = n(),  # Number of sites visited per type
#     NoAnophelesPerSite = sum(`Number of Anopheles`, na.rm = TRUE)  # Average number of Anopheles caught per site
#   ) 
# 
# # View the summary
# print(breeding_site_summary)

##CDC

# Load necessary libraries
library(tidyr)
library(dplyr)
library(ggplot2)

ento_cdc_working_df <- ento_cdc_w_all %>% 
  dplyr::select("Ward Name","Settlement Type",                                   
                "EA Name and Code", "Household Code/Number", 
                "Location of CDC", "Night of Data Collection", 
                "6-7pm(CDC Collection)", "6-7pm", "Total Number of Anopheles...16",  
                "7-8pm(CDC Collection)" , "7-8pm" ,  "Total Number of Anopheles...25",
                "8-9pm(CDC Collection)", "8-9pm", "Total Number of Anopheles...34", 
                "9-10pm(CDC Collection)",  "9-10pm", "Total Number of Anopheles...43",
                "10-11pm(CDC Collection)", "10-11pm", "Total Number of Anopheles...52",    
                "11-12am(CDC Collection)",  "11-12am", "Total Number of Anopheles...61", 
                "12-1am(CDC Collection)", "12-1am", "Total Number of Anopheles...70", 
                "1-2am(CDC Collection)", "1-2am", "Total Number of Anopheles...79", 
                "2-3am(CDC Collection)", "2-3am", "Total Number of Anopheles...88", 
                "3-4am(CDC Collection)",  "3-4am", "Total Number of Anopheles...97",
                "4-5am(CDC Collection)", "4-5am", "Total Number of Anopheles...106" ,
                "5-6am(CDC Collection)",  "5-6am", "Total Number of Anopheles...115",
                "_Household coordinates_latitude" , "_Household coordinates_longitude", "_index"   
 )

##Generate Variable for HHs where Anopheles was caught
columns_to_check <- c("Total Number of Anopheles...16",  
                      "Total Number of Anopheles...25",
                      "Total Number of Anopheles...34", 
                      "Total Number of Anopheles...43",
                      "Total Number of Anopheles...52",    
                      "Total Number of Anopheles...61", 
                      "Total Number of Anopheles...70", 
                      "Total Number of Anopheles...79", 
                      "Total Number of Anopheles...88", 
                      "Total Number of Anopheles...97",
                      "Total Number of Anopheles...106",
                      "Total Number of Anopheles...115")


ento_cdc_working_dfn <- ento_cdc_working_df %>%
  rowwise() %>%
  mutate(Anopheles_Caught = if_else(any(c_across(all_of(columns_to_check)) > 0), "Yes", "No")) %>%
  ungroup()

ento_cdc_working_dfng <- ento_cdc_working_dfn %>%
  group_by(`Ward Name`, `Location of CDC`) %>%
  summarise(
    tot67 = sum(as.numeric(`Total Number of Anopheles...16`), na.rm = TRUE),
    tot78 = sum(as.numeric(`Total Number of Anopheles...25`), na.rm = TRUE),
    tot89 = sum(as.numeric(`Total Number of Anopheles...34`), na.rm = TRUE),
    tot910 = sum(as.numeric(`Total Number of Anopheles...43`), na.rm = TRUE),
    tot1011 = sum(as.numeric(`Total Number of Anopheles...52`), na.rm = TRUE),
    tot1112 = sum(as.numeric(`Total Number of Anopheles...61`), na.rm = TRUE),
    tot121 = sum(as.numeric(`Total Number of Anopheles...70`), na.rm = TRUE),
    tot12 = sum(as.numeric(`Total Number of Anopheles...79`), na.rm = TRUE),
     tot23 = sum(as.numeric(`Total Number of Anopheles...88`), na.rm = TRUE),
     tot34 = sum(as.numeric(`Total Number of Anopheles...97`), na.rm = TRUE),
     tot45 = sum(as.numeric(`Total Number of Anopheles...106`), na.rm = TRUE),
     tot56 = sum(as.numeric(`Total Number of Anopheles...115`), na.rm = TRUE)
  )
  

ento_cdc_long <- ento_cdc_working_dfng %>%
  pivot_longer(
    cols = c(`tot67`, `tot78`, `tot89`, `tot910`, `tot1011`, `tot1112`,
             `tot121`,`tot12`,`tot23`, `tot34`, `tot45`, `tot56`),  # List all relevant columns here
    names_to = "Time",
    values_to = "Number_of_Anopheles"
  )


# Group by WardName, Location, and Time, and then summarize the total number of Anopheles
ento_cdc_summary <- ento_cdc_long %>%
  group_by(`Ward Name`, `Location of CDC`, Time) %>%
  summarize(Total_Anopheles = sum(Number_of_Anopheles)) %>%
  ungroup()

##Outdoor Transmission- Kano###
table(out_cdc$Day, out_cdc$Month)

out_cdc <- ento_cdc_summary %>%  dplyr::filter(Location == "Outdoor", `Ward Name` == "Agugu") %>%  group_by(`Settlement Classification`, `Time of Collection`) %>% 
  summarise(total_mosquitoes = sum(`Total Anopheles`)) %>% ungroup()

p <- ggplot(data= ento_cdc_summary  %>%  dplyr::filter(Location == "Outdoor", `Ward Name` == "Agugu"),
            aes(x= `Time of Collection`, y=total_mosquitoes, group = `Settlement Classification`,
                               colour = `Settlement Classification`))+
  scale_x_discrete(limits = c("6-7pm", "7-8pm", "8-9pm", "9-10pm", "10-11pm", "11-12am", "12-1am", "1-2am", "2-3am", "3-4am", "5-6am"))+
  geom_point() +labs(y= "Total number of anopheles mosquitos caught/hr.", x = "Time of collection (Kano)")+
  geom_line()+
  ggtitle("Hourly outdoor biting of anopheles mosquito aggregated \n over fourteen days, January - March, 2023")+geom_point(size = 3.0) +
  theme(plot.title = element_text(size = 12))+
  theme_manuscript() +
  theme(
    legend.position = c(.95, .95),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6)
  ) +
  ylim(0,2)

##CDC Anopheles
ento_cdc_w_all_df <- ento_cdc_w_all %>% 
  mutate(Total_Anopheles_6_7pm = sum(`Total Number of Anopheles...16`))

Total_Anopheles_6_7pm <- sum(ento_cdc_w_all$`Total Number of Anopheles...16`, na.rm = TRUE)          
Total_Anopheles_7_8pm <- sum(ento_cdc_w_all$`Total Number of Anopheles...25`, na.rm = TRUE)          
Total_Anopheles_8_9pm <- sum(ento_cdc_w_all$`Total Number of Anopheles...34`, na.rm = TRUE)          
Total_Anopheles_9_10pm <- sum(ento_cdc_w_all$`Total Number of Anopheles...43`, na.rm = TRUE)
Total_Anopheles_10_11pm <- sum(ento_cdc_w_all$`Total Number of Anopheles...52`, na.rm = TRUE)          
Total_Anopheles_11_12am <- sum(ento_cdc_w_all$`Total Number of Anopheles...61`, na.rm = TRUE)
Total_Anopheles_12_1am <- sum(ento_cdc_w_all$`Total Number of Anopheles...70`, na.rm = TRUE)          
Total_Anopheles_1_2am <- sum(ento_cdc_w_all$`Total Number of Anopheles...79`, na.rm = TRUE)
Total_Anopheles_2_3am <- sum(ento_cdc_w_all$`Total Number of Anopheles...88`, na.rm = TRUE)          
Total_Anopheles_3_4am <- sum(ento_cdc_w_all$`Total Number of Anopheles...97`, na.rm = TRUE)
Total_Anopheles_4_5am <- sum(ento_cdc_w_all$`Total Number of Anopheles...106`, na.rm = TRUE)          
Total_Anopheles_5_6am <- sum(ento_cdc_w_all$`Total Number of Anopheles...115`, na.rm = TRUE)

# `Total Number of Anopheles...25` + `Total Number of Anopheles...34`,
# `Total Number of Anopheles...43` + `Total Number of Anopheles...52` + `Total Number of Anopheles...61`,
# `Total Number of Anopheles...70` + `Total Number of Anopheles...79` + `Total Number of Anopheles...88`,
# `Total Number of Anopheles...97` + `Total Number of Anopheles...106` + `Total Number of Anopheles...115`)
# 



###PSC Anopheles
ento_psc_w_all <- slice(ento_psc_w_all, -(11))

# Modify the 'settlement_type' column based on the 'Ward' column
ento_psc_w_all <- ento_psc_w_all %>%
  mutate(`Settlement Type` = if_else(`Ward Name` == "Agugu" & `Settlement Type` == "Informal", "Slum", `Settlement Type`))

ento_psc_wet_df <- sf::st_as_sf(ento_psc_w_all, coords=c('_Household coordinates_longitude', '_Household coordinates_latitude'), crs=4326)

psc_ib <- ento_psc_w_all %>%  
  group_by(`Settlement Type`, `Ward Name`) %>% 
  summarise(total_mosquitoes = sum(`Total Number of Anopheles`, na.rm = T)) %>% 
  ungroup()

psc_hh_ib <- ento_psc_w_all %>%  
  group_by(`Ward Name`, `Household Code/Number`, Anopheles_Caught) %>% 
  summarise(total_mosquitoes = sum(`Total Number of Anopheles`, na.rm = T)) %>% 
  ungroup()

psc_hh_ib_sum <- psc_hh_ib %>% 
  dplyr::filter(Anopheles_Caught == "Yes")

##Plot number of anopheles by site 
p1 <- ggplot(data=psc_ib, aes(x= `Ward Name`, y=total_mosquitoes, group = `Settlement Type`,
                              colour = `Settlement Type`))+
  #scale_x_discrete(limits=c("March")) +
  geom_point() +labs(y= "Total number of anopheles mosquitos caught", x = "Location of Collection (Ibadan)")+
  #geom_line()+
  ggtitle("Anopheles mosquito collected through PSC, July 2024")+geom_point(size = 3.0) +
  theme(plot.title = element_text(size = 12))+
  theme_manuscript() +
  theme(
    legend.position = c(.95, .95),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6)
  ) +
  ylim(0,100)



##Plot location of households with Anopheles(PSC)
ggplot(df_ib_a) +
  geom_sf(fill= NA)+
  geom_point(data= dplyr::filter(ento_psc_wet_df, `Ward Name` == "Agugu", Anopheles_Caught == "Yes"),  aes(geometry = geometry, size = `Total Number of Anopheles`, alpha = 0.001, col = `Settlement Type`), stat= "sf_coordinates")+
  scale_color_manual(values = c(Formal = "navyblue", Informal = "tomato", Slum = "plum"))+
  #scale_shape_manual(values = c(Formal = 16,  Informal= 17, Slum = 14))+
  # geom_text_repel(
  #           data = ento_lav_wet_df,
  #           aes(label =  `Type of breeding site`, geometry = geometry),color ='black',
  #           stat = "sf_coordinates", min.segment.length = 0, size = 2.5, force = 1, max.overlaps = Inf)+
  #           guides(alpha = FALSE, size = FALSE) +
  map_theme()+ 
  ylab("")+
  xlab("")+
  labs(title= "Household in Agugu with Anophelese for Wet Season Ento (PSC)")+
  coord_sf()

##Challenge
ggplot(df_ib_c) +
  geom_sf(fill= NA)+
  geom_point(data= dplyr::filter(ento_psc_wet_df, `Ward Name` == "Challenge", Anopheles_Caught == "Yes"),  aes(geometry = geometry, size = `Total Number of Anopheles`, alpha = 0.001, col = `Settlement Type`), stat= "sf_coordinates")+
  scale_color_manual(values = c(Formal = "navyblue", Informal = "tomato", Slum = "plum"))+
  #scale_shape_manual(values = c(Formal = 16,  Informal= 17, Slum = 14))+
  # geom_text_repel(
  #           data = ento_lav_wet_df,
  #           aes(label =  `Type of breeding site`, geometry = geometry),color ='black',
  #           stat = "sf_coordinates", min.segment.length = 0, size = 2.5, force = 1, max.overlaps = Inf)+
  #           guides(alpha = FALSE, size = FALSE) +
  map_theme()+ 
  ylab("")+
  xlab("")+
  labs(title= "Household in Challenge with Anophelese for Wet Season Ento (PSC)")+
  coord_sf()


##Checking location of new CDC households
cdc_new <- data.frame(
  latitude = c(7.381532, 7.381533, 7.380827, 7.381327),
  longitude = c(3.920199, 3.919552, 3.919823, 3.920117)
)

cdc_new_df <- sf::st_as_sf(cdc_new, coords=c('longitude', 'latitude'), crs=4326)

