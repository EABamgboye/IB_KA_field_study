user <- Sys.getenv("USERNAME")
Drive <- file.path(gsub("[\\]", "/", gsub("Documents", "", Sys.getenv("HOME"))))
LuDir <- file.path(Drive, "Documents")
LuPDir <- file.path(Drive, "Downloads")

source("functions.R")

## read ibadan ward shape files
df_ib <- st_read(file.path(LuDir, "kano_ibadan_shape_files", "ibadan_metro_ward_fiveLGAs", "Ibadan_metro_fiveLGAs.shp")) %>%
  mutate(WardName = ifelse(WardName == 'Oranyan' & LGACode == '31007', 'Oranyan_7', WardName))

##Split Ibadan shapefile into working wards
df_ib_c <- df_ib %>%
  dplyr::filter(WardName == 'Challenge')

df_ib_a <- df_ib %>%
  dplyr::filter(WardName == 'Agugu')

df_ib_o <- df_ib %>%
  dplyr::filter(WardName == 'Olopomewa')


# Reproject to a CRS that uses meters
agugu_shp_m <- st_transform(df_ib_a, crs = 32630)  # Replace 32633 with the appropriate EPSG code for your area

olop_shp_m <- st_transform(df_ib_o, crs = 32630)  # Replace 32633 with the appropriate EPSG code for your area

chal_shp_m <- st_transform(df_ib_c, crs = 32630)  # Replace 32633 with the appropriate EPSG code for your area


##Compute area of each ward and covert to square kilometers
agugu_areas_m <- st_area(agugu_shp_m)

agugu_areas_sqkm <- agugu_areas_m/ 1e6

olop_areas_m <- st_area(olop_shp_m) 

olop_areas_sqkm <- olop_areas_m/ 1e6

chal_areas_m <- st_area(chal_shp_m) 

chal_areas_sqkm <- chal_areas_m / 1e6

library(readxl)

##Read the larval data with breeding sites
lav_df_jf <- read_excel(file.path(LuDir ,"Osun-excel", "Larva prospection January and Feb updated April 2023.xlsx"))

lav_df_m <- read_excel(file.path(LuDir ,"Osun-excel", "MARCH LARVA IBADAN AND KANO.xlsx"))

lav_df_dry <- rbind(lav_df_jf, lav_df_m)

#Agugu
#Dry Season
lav_df_jf <- read_excel(file.path(LuDir ,"Osun-excel", "Larva prospection January and Feb updated April 2023.xlsx"))

lav_df_m <- read_excel(file.path(LuDir ,"Osun-excel", "MARCH LARVA IBADAN AND KANO.xlsx"))

lav_df_dry <- rbind(lav_df_jf, lav_df_m) %>% 
  dplyr::filter(State == "Oyo")

lav_df_dry[44, 27] <- "No"

lav_a <- lav_df_dry %>% 
  dplyr::filter(State=="Oyo", Locality == "Agugu")

lav_a_df <- sf::st_as_sf(lav_a, coords=c('Longitude', 'Latitude'), crs=4326)

lav_a_dff <- st_intersection(lav_a_df, df_ib_a)

##Olopomewa
lav_o <- lav_df_dry %>% 
  dplyr::filter(State=="Oyo", Locality == "Olopomewa")

lav_o_df <- sf::st_as_sf(lav_o, coords=c('Longitude', 'Latitude'), crs=4326)

lav_o_dff <- st_intersection(lav_o_df, df_ib_o)


#Wet Season
lav_df_wet <- read_excel(file.path(LuPDir , "WET_SEASON_ENTO_COLLECTION_LARVAL_PROSPECTION_-_all_versions_-_labels_-_2024-08-12-21-21-06.xlsx"))

lav_df_wet  <- lav_df_wet  %>% 
  mutate(`Household Code/Number` = 1:272)

lav_df_wet  <- slice(lav_df_wet , -(1:2))

lav_df_wet  <- slice(lav_df_wet , -(6))

lav_df_wet  <- lav_df_wet  %>% 
  mutate(Anopheles_Caught = ifelse(`Number of Anopheles` > 0, "Yes", "No"))


#Agugu
lav_aw <- lav_df_wet %>% 
  dplyr::filter(`Ward Name` == "Agugu")

lav_a_dfw <- sf::st_as_sf(lav_aw, coords=c("_Breeding site coordinates_longitude",
                                           "_Breeding site coordinates_latitude"), crs=4326)

lav_a_dffw <- st_intersection(lav_a_dfw, df_ib_a)

#Challenge
lav_cw <- lav_df_wet %>% 
  dplyr::filter(`Ward Name` == "Challenge")

lav_c_dfw <- sf::st_as_sf(lav_cw, coords=c("_Breeding site coordinates_longitude",
                                           "_Breeding site coordinates_latitude"), crs=4326)

lav_c_dffw <- st_intersection(lav_c_dfw, df_ib_c)


##Calculating area covered by breeding sites
##Dry Season
#Agugu
# Convert to a projected coordinate system (e.g., UTM) to compute areas in meters
breeding_sites_a_utm <- st_transform(lav_a_dff, crs = 32631) # Use appropriate UTM zone for your area
##Distance around larval habitats(1m)
buffer_distance <- 1

#Create buffer around each geo-coordinate
a_buffers <- st_buffer(breeding_sites_a_utm, dist = buffer_distance)

#Calculate the area of each buffer in square meters
a_buffer_areas <- st_area(a_buffers)

# Convert areas to square kilometers
a_areas_sqkm <- as.numeric(a_buffer_areas) / 1e6

# # Calculate the average area covered per breeding site
# a_average_area_sqkm <- mean(a_areas_sqkm)

# Estimate the total area covered by summing all buffer areas
a_total_area_sqkm <- sum(a_areas_sqkm)

##Density per 1000 sqkm
#Area covered by anopheles larva based on number of sites positive
Agugu_dry <- 3.140157e-06 *2

a_density0 <- a_total_area_sqkm/agugu_areas_sqkm

a_density <- Agugu_dry/a_total_area_sqkm 

a_density1 <- Agugu_dry/ agugu_areas_sqkm

# Print the results
cat("Average area per breeding site (square kilometers):", a_density, "\n")


##Olopomewa
# Convert to a projected coordinate system (e.g., UTM) to compute areas in meters
breeding_sites_o_utm <- st_transform(lav_o_dff, crs = 32631) # Use appropriate UTM zone for your area
##Distance around larval habitats(1m)
buffer_distance <- 1

#Create buffer around each geo-coordinate
o_buffers <- st_buffer(breeding_sites_o_utm, dist = buffer_distance)

#Calculate the area of each buffer in square meters
o_buffer_areas <- st_area(o_buffers)

# Convert areas to square kilometers
o_areas_sqkm <- as.numeric(o_buffer_areas) / 1e6

# # Calculate the average area covered per breeding site
# o_average_area_sqkm <- mean(o_areas_sqkm)

# Estimate the total area covered by summing all buffer areas
o_total_area_sqkm <- sum(o_areas_sqkm)

##Density per 100 sqkm
#Area covered by anopheles larva based on number of sites positive
Olop_dry <- 3.140157e-06 *3

o_density0 <- o_total_area_sqkm/olop_areas_sqkm

o_density <- Olop_dry/o_total_area_sqkm

o_density1 <- Olop_dry/olop_areas_sqkm

# Print the results
cat("Average area per breeding site (square kilometers):", o_density, "\n")



##Calculating area covered by breeding sites
##Wet Season
#Agugu
# Convert to a projected coordinate system (e.g., UTM) to compute areas in meters
breeding_sites_aw_utm <- st_transform(lav_a_dffw, crs = 32631) # Use appropriate UTM zone for your area
##Distance around larval habitats(1m)
buffer_distance <- 1

#Create buffer around each geo-coordinate
aw_buffers <- st_buffer(breeding_sites_aw_utm, dist = buffer_distance)

#Calculate the area of each buffer in square meters
aw_buffer_areas <- st_area(aw_buffers)

# Convert areas to square kilometers
aw_areas_sqkm <- as.numeric(aw_buffer_areas) / 1e6

# # Calculate the average area covered per breeding site
# aw_average_area_sqkm <- mean(aw_areas_sqkm)

# Estimate the total area covered by summing all buffer areas
aw_total_area_sqkm <- sum(aw_areas_sqkm)

##Density per 1000 sqkm
#Area covered by anopheles larva based on number of sites positive
Agugu_wet <- 3.140157e-06 * 15

aw_density0 <- aw_total_area_sqkm/agugu_areas_sqkm

aw_density <- Agugu_wet/aw_total_area_sqkm

aw_density1 <- Agugu_wet/agugu_areas_sqkm

# Print the results
cat("Average area per breeding site (square kilometers):", aw_density1, "\n")


##Challenge
# Convert to a projected coordinate system (e.g., UTM) to compute areas in meters
breeding_sites_c_utm <- st_transform(lav_c_dffw, crs = 32631) # Use appropriate UTM zone for your area

##Distance around larval habitats(1m)
buffer_distance <- 1

#Create buffer around each geo-coordinate
c_buffers <- st_buffer(breeding_sites_c_utm, dist = buffer_distance)

#Calculate the area of each buffer in square meters
c_buffer_areas <- st_area(c_buffers)

# Convert areas to square kilometers
c_areas_sqkm <- as.numeric(c_buffer_areas) / 1e6

# # Calculate the average area covered per breeding site
# c_average_area_sqkm <- mean(c_areas_sqkm)

# Estimate the total area covered by summing all buffer areas
c_total_area_sqkm <- sum(c_areas_sqkm)

#Compute total area of challenge in sqkm
chal_areas_sqkm <- chal_areas_m/1e6


# #Compute the density: number of breeding sites per total area in square kilometers
# #cnumber_of_breeding_sites <- length(c_buffers)  # Total number of breeding sites
# cbreeding_site_density <- chal_wet / chal_areas_sqkm # Density per square kilometer
# 
# 
# # Identify the number of breeding sites positive for larvae
# c_positive_sites <- sum(lav_c_dffw$Anopheles_Caught == "Yes")  # Assuming 'Positive_Larvae' is 1 for positive sites
# 
# # Total number of breeding sites
# c_total_breeding_sites <- nrow(lav_c_dffw)
# 
# # Calculate the proportion of breeding sites yielding positive larvae
# c_proportion_positive_sites <- c_positive_sites / c_total_breeding_sites
# 
# # Calculate the proportion of positive larva sites per square kilometer
# cproportion_positive_sites_per_sqkm <- c_positive_sites / c_total_area_sqkm
# 
# # Print the results
# cat("Total area covered by buffers (square kilometers):", c_total_area_sqkm, "\n")
# cat("Number of positive breeding sites:", c_positive_sites, "\n")
# cat("Proportion of positive breeding sites:", c_proportion_positive_sites, "\n")
# cat("Proportion of positive breeding sites per square kilometer:", cproportion_positive_sites_per_sqkm, "\n")



##Density per 1000 sqkm
#Area covered by anopheles larva based on number of sites positive
chal_wet <- 3.140157e-06 * 9

c_density0 <- c_total_area_sqkm/chal_areas_sqkm

c_density <- chal_wet/c_total_area_sqkm

c_density1 <- chal_wet/c_total_area_sqkm * chal_areas_sqkm

# Print the results
cat("Average area per breeding site (square kilometers):", c_density, "\n")

library(units)

bs_density <- data.frame( ward <- c("Formal", "Slum", "Slum", "Formal"),
    area_pos_bs <- c(Olop_dry, Agugu_dry, Agugu_wet, chal_wet),
    area_bs <- c(o_total_area_sqkm, a_total_area_sqkm, aw_total_area_sqkm, c_total_area_sqkm),
    density <- c(o_density, a_density, aw_density, c_density),
    denisty1 <- c(o_density1, a_density1, aw_density1, c_density1),
    area_w <- c(olop_areas_m, agugu_areas_m, agugu_areas_m, chal_areas_m),
  seas <- c("Dry", "Dry", "Wet", "Wet"),
  density0 <- c(o_density0, a_density0, aw_density0, c_density0))

colnames(bs_density)[1] <- "Ward"
colnames(bs_density)[2] <- "Positive Breeding site Area"
colnames(bs_density)[3] <- "Area Covered by Breeding site"
colnames(bs_density)[4] <- "density(pbs/bs_area)"
colnames(bs_density)[5] <- "density2(pbs/bs_area*overall)"
colnames(bs_density)[6] <- "Ward Area"
colnames(bs_density)[7] <- "seas"
colnames(bs_density)[8] <- "denisty0(bs_area/overall)"



##Plot density
bs_den_p <- ggplot(data= bs_density, aes(x= `seas`, y=`density(pbs/bs_area)`, group = `ward`,
                              colour = `ward`))+
  geom_point(size = 6.0)+
scale_color_manual(values = c(Formal = "#f57362", Slum = "#f9caa7"))+
  labs(y= "breeding site density/sqkm", x = "Settlement Type")+
  #geom_line()+
  ggtitle("Breeding site density per square kilometer visited")+
    theme(plot.title = element_text(size = 12))+
 theme_manuscript() 
  # theme(
  #   legend.position = c(.9, .8),
  #   legend.justification = c("right", "top"),
  #   legend.box.just = "right",
  #   legend.margin = margin(6, 6, 6, 6)
  # )

ggsave(paste0(LuDir, '/plots/', Sys.Date(), 'Breeding sites denisty per square kilometer.pdf'), bs_den_p, width = 10, height = 8)
