user <- Sys.getenv("USERNAME")
Drive <- file.path(gsub("[\\]", "/", gsub("Documents", "", Sys.getenv("HOME"))))
LuDir <- file.path(Drive, "Documents")
LuPDir <- file.path(Drive, "Downloads")

## read ibadan ward shape files
df_ib <- st_read(file.path(LuDir, "kano_ibadan_shape_files", "ibadan_metro_ward_fiveLGAs", "Ibadan_metro_fiveLGAs.shp")) %>%
  mutate(WardName = ifelse(WardName == 'Oranyan' & LGACode == '31007', 'Oranyan_7', WardName))

# agugu_shp <- st_read(file.path(LuPDir, "Agugu", "Agugu.shp"))
# 
# # Reproject to a CRS that uses meters
# agugu_shp_m <- st_transform(agugu_shp, crs = 32633)  # Replace 32633 with the appropriate EPSG code for your area
# 
# # Recalculate the area with the new CRS
# areas_m <- st_area(agugu_shp_m)
# 
# # Print the areas in square meters
# print(areas_m)
# 
# ##Add to dataframe
# agugu_shp_m$area_m2 <- st_area(agugu_shp_m)
# 
# agugu_shp_m$centroid <- st_centroid(agugu_shp_m$geometry)

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
  labs(title= "Wards in Ibadan visited for dry season entomology study ")+
  coord_sf()

ggsave(paste0(LuDir, '/plots/', Sys.Date(), "/", 'dry season study sites.pdf'), pd, width = 8, height = 6)

# library(ggplot2)
# library(sf)
# library(ggrepel)
# library(units)  # Load the units package
# 
# # Convert area to numeric to avoid units-related issues
# agugu_shp_m$area_m2 <- as.numeric(agugu_shp_m$area_m2)
# 
# # Calculate the centroids of each polygon to position the labels
# agugu_shp_m$centroid <- st_centroid(agugu_shp_m$geometry)
# 
# # Plot with ggplot2
# ggplot(agugu_shp_m) +
#   geom_sf(fill = NA) +
#   geom_text_repel(
#     data = agugu_shp_m,
#     aes(
#       label = round(area_m2, 1),  # Correct rounding and label placement
#       geometry = centroid
#     ), 
#     stat = "sf_coordinates",
#     min.segment.length = 0, 
#     size = 3.5, 
#     color = 'black',
#     force = 1,
#     max.overlaps = Inf  # Allow all labels to be displayed, adjust as needed
#   ) +
#   map_theme() +  # Assuming this is a custom theme you defined
#   labs(title = "Wards in Ibadan") +
#   coord_sf()


##Split Ibadan shapefile into working wards
df_ib_c <- df_ib %>%
  dplyr::filter(WardName == 'Challenge')

df_ib_a <- df_ib %>%
  dplyr::filter(WardName == 'Agugu')

df_ib_o <- df_ib %>%
  dplyr::filter(WardName == 'Olopomewa')



##Read in dataset
library(readxl)
lav_df_jf <- read_excel(file.path(LuDir ,"Osun-excel", "Larva prospection January and Feb updated April 2023.xlsx"))

lav_df_m <- read_excel(file.path(LuDir ,"Osun-excel", "MARCH LARVA IBADAN AND KANO.xlsx"))

lav_df_dry <- rbind(lav_df_jf, lav_df_m) %>% 
  dplyr::filter(State == "Oyo")

lav_df_dry[44, 27] <- "No"


##Dry Season Larval Habitat - Ibadan

##Location of breeding sites
#Agugu
lav_a <- lav_df_dry %>% 
  dplyr::filter(State=="Oyo", Locality == "Agugu")

lav_a_df <- sf::st_as_sf(lav_a, coords=c('Longitude', 'Latitude'), crs=4326)

lav_a_dff <- st_intersection(lav_a_df, df_ib_a)

agl_dry <- ggplot(df_ib_a) +
  geom_sf(fill= "NA")+
  geom_point(data= lav_a_dff,  aes(geometry = geometry, size = 0.01, alpha = 0.01, col = `Anopheles_Caught`), stat= "sf_coordinates")+
  scale_color_manual(values = c(Yes = "seagreen", No = "red"))+
  # scale_shape_manual(values = c(Formal = 16,  Informal= 17, Slum = 14))+
  geom_text_repel(
    data = df_ib_a,
    aes(label =  WardName, geometry = geometry),color ='black',
    stat = "sf_coordinates", min.segment.length = 0, size = 2.5, force = 1, max.overlaps = Inf)+
  guides(alpha = FALSE, size = FALSE) +
  map_theme()+ 
  ylab("")+
  xlab("")+
  labs(title= "Sites for Dry Season Larval Collection(Agugu)")+
  coord_sf()

ggsave(paste0(LuDir, '/plots/', Sys.Date(), "/", 'dry season locations_larva prospection_agugu.pdf'), agl_dry, width = 8, height = 6)

##Olopomewa
lav_o <- lav_df_dry %>% 
  dplyr::filter(State=="Oyo", Locality == "Olopomewa")

lav_o_df <- sf::st_as_sf(lav_o, coords=c('Longitude', 'Latitude'), crs=4326)

lav_o_dff <- st_intersection(lav_o_df, df_ib_o)

olopl_dry <- ggplot(df_ib_o) +
  geom_sf(fill= "NA")+
  geom_point(data= lav_o_dff,  aes(geometry = geometry, size = 0.05, alpha = 0.01, col = `Anopheles_Caught`), stat= "sf_coordinates")+
  scale_color_manual(values = c(Yes = "seagreen", No = "red"))+
  # scale_shape_manual(values = c(Formal = 16,  Informal= 17, Slum = 14))+
  geom_text_repel(
    data = df_ib_o,
    aes(label =  `WardName`, geometry = geometry),color ='black',
    stat = "sf_coordinates", min.segment.length = 0, size = 2.5, force = 1, max.overlaps = Inf)+
  guides(alpha = FALSE, size = FALSE) +
  map_theme()+ 
  ylab("")+
  xlab("")+
  labs(title= "Sites for Dry Season Larval Collection(Olopomewa)")+
  coord_sf()

ggsave(paste0(LuDir, '/plots/', Sys.Date(), "/", 'dry season locations_larva prospection_olop.pdf'), olopl_dry, width = 8, height = 6)

##Breeding site analysis
# Summarize data by breeding site type
breeding_site_sum_dry <- lav_df_dry %>% 
  dplyr::filter(State=="Oyo") %>% 
  group_by(`Locality`,`Breeding site`) %>%  # Group by breeding site type
  summarize(
    SitesVisited = n(),  # Number of sites visited per type
    ) 

#Number and type of breeding sites
bs_dry <- ggplot(data=breeding_site_sum_dry, aes(x = `Breeding site`, y=SitesVisited, fill = `Breeding site`)) +
    geom_bar(stat="identity", position = position_dodge(width = 0.8))+
  facet_wrap(~ `Locality`, scales = "free_x")+
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
  ylim(0,50)+
  ggtitle("Number and type of breeding sites visited in Ibadan, Jan-March, 2023")

ggsave(paste0(LuDir, '/plots/', Sys.Date(), 'dry season breeding sites visited in Ibadan.pdf'), bs_dry, width = 8, height = 6)


###Number and type of breeding sites anopheles was caught
b_site_ano_sum_dry <- lav_df_dry %>% 
  dplyr::filter(State=="Oyo") %>% 
  group_by(`Locality`,`Breeding site`, `Anopheles_Caught`) %>%  # Group by breeding site type
  summarize(
    SitesVisited = n(),  # Number of sites visited per type
  )

bs_ano_dry <- ggplot(data=b_site_ano_sum_dry, aes(x=`Breeding site`, y=SitesVisited, fill = `Anopheles_Caught`)) +
  geom_bar(stat="identity", position = "stack")+
  facet_wrap(~ `Locality`, scales = "free_x")+
  geom_text(aes(label=SitesVisited), vjust = -0.4,
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
  ggtitle("Number of breeding sites visted and number where \n anopheles larva were found in Ibadan, Jan-March, 2023")+
  scale_fill_manual(values = c(Yes = "chocolate4", No = "coral"))

ggsave(paste0(LuDir, '/plots/', Sys.Date(), 'dry season breeding sites with anopheles in Ibadan.pdf'), bs_ano_dry, width = 8, height = 6)


#Total number of anopheles caught by type of breeding sites
# Summarize data by breeding site type
ano_caught_sum_dry <- lav_df_dry %>% 
  dplyr::filter(State=="Oyo", `Locality` == "Agugu" | `Locality` == "Olopomewa" ) %>% 
  group_by(`Settlement Type`, `Locality`,`Breeding site`, `Anopheles_Caught`) %>%  # Group by breeding site type
  summarize(
    SitesVisited = n(),  # Number of sites visited per type
    NoAnophelesPerSite = sum(`Anopheles`, na.rm = TRUE)  # Average number of Anopheles caught per site
  ) 


ano_bs_dry <- ggplot(data=ano_caught_sum_dry, aes(x=`Breeding site`, y=NoAnophelesPerSite, fill = `Breeding site`)) +
  geom_bar(stat="identity", position = position_dodge(width = 0.8))+
  facet_wrap(~ `Locality`, scales = "free_x")+
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
  ylim(0,50)+
  ggtitle("Number of anopheles caught per breeding sites visted in Ibadan, Jan-March, 2023")

ggsave(paste0(LuDir, '/plots/', Sys.Date(), 'dry season anopheles per breeding sites in Ibadan.pdf'), ano_bs_dry, width = 8, height = 6)


#Larval Density
##Calculations(Larval density)
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

lav_den_dry <- ggplot(lav_den_sum)+
  #geom_bar(aes(x = `Type of breeding site.x`, y = Anopheles_caught), stat = "identity", fill = "plum", width = 0.5, position = "stack")+
  geom_point(aes(x=`Site Code`, y = Larva_Density, group = 1), stat = "identity", color = "red", size = 3.5)+
  facet_wrap(~`Locality`)+
  geom_text(aes(x = `Site Code`, y = Larva_Density, 
                label = round(Larva_Density, 1)), 
            vjust = -0.7, color = "black", size = 3.5) +
  labs(title = "Distribution of breeding sites with presence of anopheles larva \n and corresponding larva density by nature of water",
       x = "Breeding Site",
       #y = "Number of Breeding Sites")+
  #scale_y_continuous(
   # name = "Numberof breeding sites",  # Primary y-axis label
    #sec.axis = sec_axis(~ ./1, name = "Larval Density")  # Secondary y-axis with scaling
  ) +
  # scale_y_continuous(sec.axis=sec_axis(~./ 10, name = "Larval Density"))+
  theme_manuscript()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8))+
  theme(strip.background = element_rect(fill = "yellow", color = "black"))+
  ylim(0,10)

ggsave(paste0(LuDir, '/plots/', Sys.Date(), 'dry season larva density per breeding sites in Ibadan1.pdf'), lav_den_dry, width = 8, height = 6)



##Breeding site characteristics and larval density
#Origin of water
ggplot(lav_den_sum)+
  #geom_bar(aes(x = `Type of breeding site.x`, y = Anopheles_caught), stat = "identity", fill = "plum", width = 0.5, position = "stack")+
  geom_point(aes(x=`Site Code`, y = Larva_Density, group = 1), stat = "identity", color = "green", size = 3.5)+
  facet_wrap(~`Origin of water`)+
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
lav_den_sum[5, 15] <- "clean"

lav_den_wn_dry <- ggplot(lav_den_sum)+
  #geom_bar(aes(x = `Type of breeding site.x`, y = Anopheles_caught), stat = "identity", fill = "plum", width = 0.5, position = "stack")+
  geom_point(aes(x=`Site Code`, y = Larva_Density, group = 1), stat = "identity", color = "cadetblue", size = 3.5)+
  facet_wrap(~`Water nature`)+
  labs(title = "Distribution of Number of breeding sites with presence of anopheles larva \n
       and corresponding larva density by Water nature",
       x = "Settlement Type",
       #y = "Number of Breeding Sites")+
       #scale_y_continuous(
       # name = "Numberof breeding sites",  # Primary y-axis label
       #sec.axis = sec_axis(~ ./1, name = "Larval Density")  # Secondary y-axis with scaling
  ) +
  # scale_y_continuous(sec.axis=sec_axis(~./ 10, name = "Larval Density"))+
  theme_manuscript()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8))+
  theme(strip.background = element_rect(fill = "plum1", color = "black"))


ggsave(paste0(LuDir, '/plots/', Sys.Date(), 'dry season larva density water nature n breeding sites in Ibadan.pdf'), lav_den_wn_dry, width = 8, height = 6)


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

#Water Nature
natt <- c("Clean", "Polluted", "Clean", "Polluted")
seas <- c("Dry Season", "Dry Season", "Wet Season", "Wet Season")
freq <- c(3, 2, 18, 8)

wn_df <- data.frame(natt, seas, freq)

wn_bs <- ggplot(wn_df)+
  geom_bar(aes(x = natt, y = freq, fill = natt), stat = "identity", width = 0.5, position = "stack") +
  scale_fill_manual(values = c("Clean" = "lightblue2", "Polluted" = "bisque")) +
  facet_wrap(~seas)+
  geom_text(aes(x=natt, y = freq, label = freq), size = 4.5, vjust = 0.8, hjust = 1.3, color = "black")+
  labs(title = "Distribution of Number of breeding sites with presence of anopheles larva \n and water nature by seasons",
       x = "Settlement Type",
       y = "Number of Breeding Sites")+
  #scale_y_continuous(sec.axis=sec_axis(~.* 1, name = "Larval Density"))+
  theme_manuscript()+
  theme(strip.background = element_rect(fill = "ivory3", color = "black"))


ggsave(paste0(LuDir, '/plots/', Sys.Date(), 'Water nature of breeding sites in Ibadan.png'), wn_bs, width = 8, height = 6)

###AGUGU-ANALYSIS
##Extract households with a known distance from selected LBS points

# Choose the particular point from data frame
cdc_a_df_y <- cdc_a_df %>% 
  dplyr::filter(Anopheles_Caught =="Yes")

cdc_a_df_n <- cdc_a_df %>% 
  dplyr::filter(Anopheles_Caught =="No") %>% 
  distinct(geometry, .keep_all = TRUE)

lav_a_df_y <- lav_a_df %>% 
  dplyr::filter(Anopheles_Caught =="Yes")

lav_a_df_n <- lav_a_dff %>% 
  dplyr::filter(Anopheles_Caught =="No")

hh_pos_a_df <- st_intersection(hh_pos_a, df_ib_a)

particular_cdc <-cdc_a_df[180, ]


# Latitude <- 7.37718
# Longitude <- 3.92393
# cdc_pt <- data.frame(Latitude, Longitude)

# Buffer around the particular point (500 meters)
#buffer <- st_buffer(cdc_a_df_y, dist = 100)

# # Find points within 500 meters by using spatial intersection
# nearby_points <- st_intersection(hh_pos_a, buffer)
# 
# # Check the result
# print(nearby_points)




##Mapping out the polygon of area of visit
# Create a convex hull around the breeding sites
breeding_sites_hull <- st_convex_hull(st_union(lav_a_dff))

# Intersect the convex hull with the shapefile to limit the polygon to a specific area
breeding_sites_polygon <- st_intersection(breeding_sites_hull, df_ib_a)
hh_pos_a_dff <- st_intersection(hh_pos_a, breeding_sites_polygon)

library(ggplot2)
# Dummy data to create a manual legend
aglegend_data <- data.frame(
  label = c("Breeding sites with no Larval", "Breeding sites with Larval", "Households with positive cases"),
  color = c("seagreen", "red", "blue"),
  shape = c(16, 17, 20)
)

geo_agu_dry <- ggplot() +
  geom_sf(data = df_ib_a, fill = "aliceblue", color = "black") +  # Background shapefile
  geom_sf(data = lav_a_df_n, color = "seagreen", size = 2, alpha = 0.7) +  # Breeding sites with no larval
  geom_sf(data = lav_a_df_y, color = "red", size = 2, shape = 17) +  # Breeding sites with Larval
  geom_sf(data = hh_pos_a_dff, color = "blue", size = 2) +  # Households with positive cases
  geom_sf(data = breeding_sites_polygon, fill = NA, color = "brown", size = 3) +  # Convex hull polygon
  geom_point(data = aglegend_data, aes(x = Inf, y = Inf, color = label), size = 3) +  # Invisible points for legend
  scale_color_manual(
    name = "Legend",
    values = c("Breeding sites with no Larval" = "seagreen", "Breeding sites with Larval" = "red",
               "Households with positive cases" = "blue")
  ) +
  labs(title = "Geo-location of area covered during larval prospection and breeding sites with anopheles larva(dry season)") +
  map_theme()+
  theme_manuscript()

ggsave(paste0(LuDir, '/plots/', Sys.Date(), 'Geo-location covered by breeding sites in Agugu(dry)1.pdf'), geo_agu_dry, width = 10, height = 11)


##Computing estimates of the density per geo-location

# Transform to UTM (example using UTM zone 32N, EPSG:32632)
breeding_sites_polygon_utm <- st_transform(breeding_sites_polygon, crs = 32632)

# Calculate the area of the buffer in square meters
buffer_area <- st_area(breeding_sites_polygon_utm)
buffer_area_sqm <- as.numeric(buffer_area)  # Convert to numeric if necessary

##Convert area to square kilometer
buffer_area_sqkm <- buffer_area_sqm / 1e6

# Number of breeding sites with larvae caught
larva_sites_count <- length(ag_lav_sites_an_utm)
larva_sites_count <- 2
# Total number of breeding sites visited (assuming this is available)
total_sites_count <-41  # Replace with your actual variable

# Step 3: Calculate the proportion of sites with larvae
larva_proportion <- larva_sites_count / total_sites_count

# Estimate the density of larva breeding sites per square kilometer
density_per_sqkm <- larva_sites_count / buffer_area_sqkm

# Step 5: Adjust the density considering the total sites visited
adjusted_density_per_sqkm <- density_per_sqkm * larva_proportion



##Map circles around specific LBS points
# Transform to a CRS with meters as units (e.g., UTM)
lav_sites_utm <- st_transform(lav_a_df_n, crs = 32633)  # Replace with appropriate UTM zone for your area
df_ib_a_utm <- st_transform(df_ib_a, crs = 32633)  # Same UTM zone
ag_lav_sites_an_utm <- st_transform(lav_a_df_y, crs = 32633)
cdc_pt_a_utm <- st_transform(cdc_a_df_y, crs = 32633)
psc_pt_a_utm <- st_transform(psc_a_df, crs = 32633)


##Buffer around HH points
# Create 100-meter buffers around each point
abuffers <- st_buffer(cdc_pt_a_utm, dist = 300)

# Dummy data to create a manual legend
alegend_data <- data.frame(
  label = c("CDC HH", "Breeding sites with no Larval", "Breeding sites with Larval", "CDC HH with adult mosquito"),
  color = c("seagreen", "blue", "coral", "brown"),
  shape = c(16, 16, 17, 18)
)

# Plot using ggplot2
buf_agu_dry <- ggplot() +
  geom_sf(data = df_ib_a_utm, fill = "aliceblue", color = "black") +  # Shapefile
  geom_sf(data =cdc_a_df, color = "seagreen", size = 2) +  # CDC HHs
  geom_sf(data =cdc_a_df_y, color = "brown", size = 3) +  # PSC HHs
  geom_sf(data =abuffers, fill = NA,  color = "red") +  # Buffers
  geom_sf(data = lav_sites_utm, color = "blue", size = 2, alpha= 0.7) +  # Points from original dataframe
  geom_sf(data = ag_lav_sites_an_utm, color = "coral", size = 2.5, shape = 17) +  # Points from the other dataframe
  geom_point(data = alegend_data, aes(x = Inf, y = Inf, color = label), size = 3) +  # Invisible points for legend
  scale_color_manual(
    name = "Legend",
    values = c("CDC HH" = "seagreen", "Breeding sites with no Larval" = "blue", "Breeding sites with Larval" = "coral",
               "CDC HH with adult mosquito" = "brown" )
  ) +
  labs(title = "Geo-location of breeding sites covered within 300m of a cdc household") +
  map_theme()+
  theme_manuscript()

ggsave(paste0(LuDir, '/plots/', Sys.Date(), 'Geo-location and buffer of breeding sites in Agugu.pdf'), buf_agu_dry, width = 10, height = 11)

###OLOPOMEWA:-ANALYSIS

##Extract households with a known distance from selected LBS points
# Choose the particular point from points_6 (e.g., the first point)
cdc_o_df_y <- cdc_o_df %>% 
  dplyr::filter(Anopheles_Caught =="Yes")

psc_o_df_y <- psc_o_df %>% 
  dplyr::filter(Anopheles_Caught =="Yes")

lav_o_df_y <- lav_o_dff %>% 
  dplyr::filter(Anopheles_Caught =="Yes")

lav_o_df_n <- lav_o_dff %>% 
  dplyr::filter(Anopheles_Caught =="No")

particular_o_cdc <-lav_o_df_y[1, ]


##Mapping out the polygon of area of visit
# Create a convex hull around the breeding sites
breeding_sites_olop <- st_convex_hull(st_union(lav_o_dff))

# Intersect the convex hull with the shapefile to limit the polygon to a specific area
breeding_sites_olop_polygon <- st_intersection(breeding_sites_olop, df_ib_o)

library(ggplot2)
# Dummy data to create a manual legend
ollegend_data <- data.frame(
  label = c("Breeding sites with no Larval", "Breeding sites with Larval"),
  color = c("seagreen", "red"),
  shape = c(16, 18)
)


geo_olop_dry <- ggplot() +
  geom_sf(data = df_ib_o, fill = "aliceblue", color = "black") +  # Background shapefile
  geom_sf(data = lav_o_df_n, color = "seagreen", size = 2, alpha = 0.7) +  # Breeding sites with no larval
  geom_sf(data = lav_o_df_y, color = "red", size = 2) +  # Breeding sites with Larval
  geom_sf(data = breeding_sites_olop_polygon, fill = NA, color = "brown", size = 3) +  # Convex hull polygon
  geom_point(data = ollegend_data, aes(x = Inf, y = Inf, color = label), size = 3) +  # Invisible points for legend
  scale_color_manual(
    name = "Legend",
    values = c("Breeding sites with no Larval" = "seagreen", "Breeding sites with Larval" = "red")
  ) +
  labs(title = "Geo-location of area covered during larval prospection and breeding sites with anopheles larva(dry season)") +
  map_theme()+
  theme_manuscript()

ggsave(paste0(LuDir, '/plots/', Sys.Date(), 'Geo-location covered by breeding sites in Olopomewa.pdf'), geo_olop_dry, width = 10, height = 11)


##Computing estimates of the density per geo-location
# Transform to UTM (example using UTM zone 32N, EPSG:32632)
breeding_sites_olop_polygon_utm <- st_transform(breeding_sites_olop_polygon, crs = 32632)

# Calculate the area of the buffer in square meters
buffer_area_olop <- st_area(breeding_sites_olop_polygon_utm)
buffer_area_olop_sqm <- as.numeric(buffer_area_olop)  # Convert to numeric if necessary

##Convert to square kilometer
buffer_area_olop_sqkm <- buffer_area_olop_sqm / 1e6

# Number of breeding sites with larvae caught
olop_larva_sites_count <- dim(lav_o_df_y)[1]
olop_larva_sites_count <- 2
# Total number of breeding sites visited (assuming this is available)
olop_total_sites_count <- dim(lav_o_dff)[1]
olop_total_sites_count <-32  # Replace with your actual variable

# Step 3: Calculate the proportion of sites with larvae
olop_larva_proportion <- olop_larva_sites_count / olop_total_sites_count

# Estimate the density of larva breeding sites per square meter
olop_density_per_sqkm <- olop_larva_sites_count / buffer_area_olop_sqkm

# Step 5: Adjust the density considering the total sites visited
olop_adjusted_density_per_sqkm <- olop_density_per_sqkm * olop_larva_proportion



##Map circles around specific LBS points

# Transform to a CRS with meters as units (e.g., UTM)
lav_sites_o_utm<- st_transform(lav_o_df_n, crs = 32633)  # Replace with appropriate UTM zone for your area
df_ib_o_utm <- st_transform(df_ib_o, crs = 32633)  # Same UTM zone
ol_lav_sites_an_utm <- st_transform(lav_o_df_y, crs = 32633)
cdc_pt_o_utm <- st_transform(particular_o_cdc, crs = 32633)

# Assuming you have another dataframe with points, e.g., `other_points_utm`

# Create 100-meter buffers around each point
obuffers <- st_buffer(cdc_pt_o_utm, dist = 300)

st_area(obuffers)

##Mapping out the polygon of area of visit
# Create a convex hull around the breeding sites
obreeding_sites_hull <- st_convex_hull(st_union(lav_o_dff))

# Intersect the convex hull with the shapefile to limit the polygon to a specific area
obreeding_sites_polygon <- st_intersection(obreeding_sites_hull, df_ib_o)

library(ggplot2)

ggplot() +
  geom_sf(data = df_ib_o, fill = "aliceblue", color = "black") +  # Background shapefile
  geom_sf(data = lav_o_df_y, color = "red", size = 2) +  # Breeding sites with Larval
  #geom_sf(data = lav_o_df_n, color = "lightgreen", size = 2, alpha = 0.7) +  # Breeding sites with no larval
  geom_sf(data = obreeding_sites_polygon, fill = NA, color = "brown", size = 2) +  # Convex hull polygon
  labs(title = "Olopomewa Capturing Breeding Sites") +
  theme_minimal()



library(ggplot2)
library(sf)

# Assuming your map_theme() and theme_manuscript() functions are already defined

# Dummy data to create a manual legend
olegend_data <- data.frame(
  label = c("PSC HH", "Breeding sites with no Larval", "Breeding sites with Larval"),
  color = c("brown", "blue", "coral"),
  shape = c(16, 16, 17)
)

# Adding the manual legend
buf_olop_dry <- ggplot() +
  geom_sf(data = df_ib_o_utm, fill = "aliceblue", color = "black")+  # Shapefile
  geom_sf(data = psc_o_df_y, color = "brown", size = 2) +  # PSC HHs
  geom_sf(data = obuffers, fill = NA,  color = "red") +  # Buffers
  geom_sf(data = lav_sites_o_utm, color = "blue", size = 2, alpha = 0.7) +  # Original Points
  geom_sf(data = ol_lav_sites_an_utm, color = "coral", size = 2.5, shape = 17) +  # Other Points
  geom_point(data = olegend_data, aes(x = Inf, y = Inf, color = label), size = 3) +  # Invisible points for legend
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


ggsave(paste0(LuDir, '/plots/', Sys.Date(), 'Geo-location and buffer of breeding sites in Olopomewa.pdf'), buf_olop_dry, width = 10, height = 11)





# Extract coordinates
coordss_lav_sites <- st_coordinates(lav_a_dff)

coordss_lav_sites <- data.frame(coordss_lav_sites)

ag_lav_sites_vi <- sf::st_as_sf(coordss_lav_sites, coords=c('X', 'Y'), crs=4326)

write.csv(coords_lav_sites, file.path(LuDir, "lav_coords.csv"))

subset_lavw <- lav_a_dfw[lav_a_dfw$`Number of Anopheles` == 0,]

wcoords_lav_sites <- st_coordinates(subset_lavw)

write.csv(wcoords_lav_sites, file.path(LuDir, "wlav_coords.csv"))

