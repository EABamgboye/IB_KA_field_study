user <- Sys.getenv("USERNAME")
LucDir <- file.path("C:/Users/ebamgboye/Urban Malaria Proj Dropbox/urban_malaria")
Datadir <- file.path(LucDir, "data")
ReqDir <- file.path(Datadir, "nigeria/nigeria_hmis", "HF Data 2019-2023")

library(readxl)

##Read in HMIS data
hmis_2019 <- read_excel(file.path(Datadir, "nigeria/nigeria_hmis", "HF Data 2019-2023", "HF Data 2019-2023", "NMEP Malaria 2019 Data.xlsx"), sheet = 1)
hmis_2020 <- read_excel(file.path(Datadir, "nigeria/nigeria_hmis", "HF Data 2019-2023", "HF Data 2019-2023", "NMEP Malaria 2020 Data.xlsx"), sheet = 1)
hmis_2021 <- read_excel(file.path(Datadir, "nigeria/nigeria_hmis", "HF Data 2019-2023", "HF Data 2019-2023", "NMEP Malaria 2021 HF Data.xlsx"), sheet = 1)
hmis_2022 <- read.csv(file.path(Datadir, "nigeria/nigeria_hmis", "HF Data 2019-2023", "HF Data 2019-2023", "2022 HF Data", "2022 HF Data",
                                "Kano 2022 2.csv"))
hmis_2023 <- read.csv(file.path(Datadir, "nigeria/nigeria_hmis", "HF Data 2019-2023", "HF Data 2019-2023", "2023 HF Data", "2023 HF Data",
                                "Kano 2023 (2).csv"))

#Extracting Primary health care facilities alone, Kano and other relevant variables
kn_hmis_2019_p <- hmis_2019 %>% 
  dplyr::filter(state == "kn Kano State") %>% 
  dplyr::select(state, lga, ward, facilityname, opd, conf_u5) %>% 
  dplyr::filter(str_detect(facilityname, "Primary Health Center"))%>% 
  rename(HF_type = `facilityname`,
         outpatient = `opd`,
         rdt_u5 = `conf_u5`)

kn_hmis_2020_p <- hmis_2020 %>% 
  dplyr::filter(state == "kn Kano State") %>% 
  dplyr::select(state, lga, ward, level_of_care, opd, conf_u5) %>% 
  dplyr::filter(level_of_care == "Primary") %>% 
  rename(HF_type = `level_of_care`,
       outpatient = `opd`,
       rdt_u5 = `conf_u5`)

kn_hmis_2021_p <- hmis_2021 %>% 
  dplyr::filter(State == "kn Kano State") %>% 
  dplyr::select(State, LGA, Ward, Level, `Out-patient Attendance`, `Persons tested positive for malaria by RDT <5yrs`) %>% 
  dplyr::filter(Level == "Primary Health Facility")%>% 
  rename( state = State,
          lga = LGA,
          ward = Ward,
          HF_type = `Level`,
         outpatient = `Out-patient Attendance`,
         rdt_u5 = `Persons tested positive for malaria by RDT <5yrs`)

kn_hmis_2022_p <- hmis_2022 %>% 
  dplyr::select(organisationunitname.1, orgunitlevel2, orgunitlevel3, orgunitlevel4, `Out.patient.Attendance`, 
                `Persons.tested.positive.for.malaria.by.RDT..5yrs`) %>% 
  dplyr::filter(organisationunitname.1 == "Primary Health Facility")%>% 
  rename(state = orgunitlevel2,
          lga = orgunitlevel3,
          ward = orgunitlevel4,
          HF_type = `organisationunitname.1`,
          outpatient = `Out.patient.Attendance`,
          rdt_u5 = `Persons.tested.positive.for.malaria.by.RDT..5yrs`)


kn_hmis_2023_p <- hmis_2023 %>% 
  dplyr::select(organisationunitname.1, orgunitlevel2, orgunitlevel3, orgunitlevel4, `Out.patient.Attendance`, 
                `Persons.tested.positive.for.malaria.by.RDT..5yrs`) %>% 
  dplyr::filter(organisationunitname.1 == "Primary Health Facility")%>% 
  rename(state = orgunitlevel2,
         lga = orgunitlevel3,
         ward = orgunitlevel4,
         HF_type = `organisationunitname.1`,
         outpatient = `Out.patient.Attendance`,
         rdt_u5 = `Persons.tested.positive.for.malaria.by.RDT..5yrs`)

##Combine all datasets
merged_kn_hmis_2019_2023 <- rbind(kn_hmis_2019_p, kn_hmis_2020_p, kn_hmis_2021_p, kn_hmis_2022_p, kn_hmis_2023_p)

Kanotpr_201923 <- merged_kn_hmis_2019_2023 %>% 
  group_by(ward) %>% #HF and level added
  summarise(
    u5_tpr_rdt = sum(rdt_u5, na.rm = TRUE) / sum(outpatient, na.rm = TRUE) * 100,  
    ) %>% 
  ungroup()

#Clean up names for ease of merging with shapefile
Kanotpr_201923 <- Kanotpr_201923 %>%
  mutate(ward = gsub("^kn ", "", ward), 
         ward = gsub("^Kn ", "", ward), # Remove "kn " prefix
         ward = gsub(" Ward$", "", ward),  # Remove " Ward" suffix
         ward = gsub("Fagge D 2", "Fagge D2", ward) # Replace "Faggae D 2" with "Faggae D2"
  )

##Merge kano shapefile to tpr data(extract metro area)
kn_metroshp <- st_read(file.path(Datadir,"nigeria", "kano_shapefile", "k_new.shp"))

Kanotpr_201923 <- Kanotpr_201923 %>%
  rename(WardName = ward)  # Replace 'WardName' with the actual column name in kano_u5_tpr_df

# Perform the merge
kanometro_tpr_data <- kn_metroshp %>%
  left_join(Kanotpr_201923, by = "WardName")

kanometro_tpr_data <- st_set_crs(kanometro_tpr_data, 4326)

##Fill up wards with nearest neighbours
w <- spdep::poly2nb(kanometro_tpr_data, queen = TRUE)
w_listw <- spdep::nb2listw(w)


# Compute the average test positivity rate from neighboring polygons

mean_neighbors <- weights(w_listw, kanometro_tpr_data$u5_tpr_rdt)

missing_indices <- which(is.na(kanometro_tpr_data$u5_tpr_rdt))

neighbors_list <- w_listw$neighbours

neighbors_list <- w_listw$neighbours

# Impute missing 'tpr_u5' values with the mean of neighboring values
for (index in seq_along(missing_indices)) {
  polygon <- missing_indices[index]
  neighbor_tprs <- neighbors_list[[polygon]]
  kanometro_tpr_data$u5_tpr_rdt[polygon] <- mean(kanometro_tpr_data$u5_tpr_rdt[neighbor_tprs], na.rm = TRUE)
}

kano_metro_tpr <- kanometro_tpr_data %>% 
  dplyr::select(WardName, u5_tpr_rdt) %>% 
  st_drop_geometry()

write.csv(kano_metro_tpr, file.path(LuDir, "kanotpr2.csv" ))

ggplot(data = kanometro_tpr_data) +
  geom_sf(aes(fill = u5_tpr_rdt)) +  # Replace with the correct column for prevalence
  scale_fill_gradient(low = "azure", high = "violetred", na.value = "grey50",
                      name = "Malaria u5TPR(Burden)") +
  # geom_text(
  #   aes(x = X, y = Y, label = round(u5_tpr3_rdt, 1)),  # Use the centroid coordinates
  #   vjust = 1.2, color = "black", size = 3.5
  # )
  # geom_sf(data = health_facilities_sf, aes(size = rate), shape = 21, fill = "blue", color = "white") +
  # scale_size_continuous(name = "Health Facility Rate") +
  labs(
    title = "Malaria TPR by Study Wards using HMIS 2019-2023",
    #   subtitle = "Health Facilities with High Rates Highlighted",
    caption = "u5TPR = Confirmed u5/Overall Gen. Attendance"
  ) +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    legend.position = "right",
    axis.text = element_blank(),      # Remove axis text
    axis.ticks = element_blank()      # Remove axis ticks
  )+
  theme_manuscript()

##Merge kano shapefile to tpr data(extract metro area)
kn_metroshp <- st_read(file.path(Datadir,"nigeria", "kano_shapefile", "k_new.shp"))

Kanotpr_201923 <- Kanotpr_201923 %>%
  rename(WardName = ward)  # Replace 'WardName' with the actual column name in kano_u5_tpr_df

# Perform the merge
kanometro_tpr_data <- kn_metroshp %>%
  left_join(Kanotpr_201923, by = "WardName")

kanometro_tpr_data <- st_set_crs(kanometro_tpr_data, 4326)

##Fill up wards with nearest neighbours
w <- spdep::poly2nb(kanometro_tpr_data, queen = TRUE)
w_listw <- spdep::nb2listw(w)


# Compute the average test positivity rate from neighboring polygons

mean_neighbors <- weights(w_listw, kanometro_tpr_data$u5_tpr_rdt)

missing_indices <- which(is.na(kanometro_tpr_data$u5_tpr_rdt))

neighbors_list <- w_listw$neighbours

neighbors_list <- w_listw$neighbours

# Impute missing 'tpr_u5' values with the mean of neighboring values
for (index in seq_along(missing_indices)) {
  polygon <- missing_indices[index]
  neighbor_tprs <- neighbors_list[[polygon]]
  kanometro_tpr_data$u5_tpr_rdt[polygon] <- mean(kanometro_tpr_data$u5_tpr_rdt[neighbor_tprs], na.rm = TRUE)
}

kano_metro_tpr <- kanometro_tpr_data %>% 
  dplyr::select(WardName, u5_tpr_rdt) %>% 
  st_drop_geometry()

write.csv(kano_metro_tpr, file.path(LuDir, "kanotpr2.csv" ))

ggplot(data = kanometro_tpr_data) +
  geom_sf(aes(fill = u5_tpr_rdt)) +  # Replace with the correct column for prevalence
  scale_fill_gradient(low = "azure", high = "violetred", na.value = "grey50",
                      name = "Malaria u5TPR(Burden)") +
  # geom_text(
  #   aes(x = X, y = Y, label = round(u5_tpr3_rdt, 1)),  # Use the centroid coordinates
  #   vjust = 1.2, color = "black", size = 3.5
  # )
  # geom_sf(data = health_facilities_sf, aes(size = rate), shape = 21, fill = "blue", color = "white") +
  # scale_size_continuous(name = "Health Facility Rate") +
  labs(
    title = "Malaria TPR by Study Wards using HMIS 2019-2023",
    #   subtitle = "Health Facilities with High Rates Highlighted",
    caption = "u5TPR = Confirmed u5/Overall Gen. Attendance"
  ) +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    legend.position = "right",
    axis.text = element_blank(),      # Remove axis text
    axis.ticks = element_blank()      # Remove axis ticks
  )+
  theme_manuscript()




##Adding data to other prediction covariates

kn_env_predictions <- read.csv(file.path(Datadir, "nigeria", "kano_ibadan_epi", "new_field_data", "analysis_docs", "kano_environmental_data_predictions.csv"))

kano_metro_tpr_m <- kano_metro_tpr %>% 
  rename(ward = WardName)

kn_env_predictions_updated <- kn_env_predictions %>% 
  left_join(kano_metro_tpr_m, by = "ward")

write.csv(kn_env_predictions_updated, file.path(Datadir, "nigeria", "kano_ibadan_epi", "new_field_data", "analysis_docs", "kano_environmental_data_predictions.csv"))



##Merge kano shapefile to tpr data(extract entire)
kn_wardshp <- st_read(file.path(Datadir,"nigeria", "nigeria_shapefiles", "Nigeria Boundary Files_All", "Boundary_VaccWards_Export", "Boundary_VaccWards_Export.shp")) %>% 
  dplyr::filter(StateCode == "KN")

Kanotpr_201923 <- Kanotpr_201923 %>%
  rename(WardName = ward)  # Replace 'WardName' with the actual column name in kano_u5_tpr_df

# Perform the merge
kano_tpr_data <- kn_wardshp %>%
  left_join(Kanotpr_201923, by = "WardName")

kano_tpr_data <- st_set_crs(kano_tpr_data, 4326)

##Fill up wards with nearest neighbours
w <- spdep::poly2nb(kano_tpr_data, queen = TRUE)
w_listw <- spdep::nb2listw(w)


# Compute the average test positivity rate from neighboring polygons

mean_neighbors <- weights(w_listw, kano_tpr_data$u5_tpr_rdt)

missing_indices <- which(is.na(kano_tpr_data$u5_tpr_rdt))

neighbors_list <- w_listw$neighbours

neighbors_list <- w_listw$neighbours

# Impute missing 'tpr_u5' values with the mean of neighboring values
for (index in seq_along(missing_indices)) {
  polygon <- missing_indices[index]
  neighbor_tprs <- neighbors_list[[polygon]]
  kano_tpr_data$u5_tpr_rdt[polygon] <- mean(kano_tpr_data$u5_tpr_rdt[neighbor_tprs], na.rm = TRUE)
}

kano_city_tpr <- kano_tpr_data %>% 
  dplyr::select(WardCode, WardName, u5_tpr_rdt) %>% 
  st_drop_geometry()

write.csv(kano_city_tpr, file.path(LuDir, "kanotpr3.csv" ))

ggplot(data = kano_tpr_data) +
  geom_sf(aes(fill = u5_tpr_rdt)) +  # Replace with the correct column for prevalence
  scale_fill_gradient(low = "azure", high = "violetred", na.value = "grey50",
                      name = "Malaria u5TPR(Burden)") +
  # geom_text(
  #   aes(x = X, y = Y, label = round(u5_tpr3_rdt, 1)),  # Use the centroid coordinates
  #   vjust = 1.2, color = "black", size = 3.5
  # )
  # geom_sf(data = health_facilities_sf, aes(size = rate), shape = 21, fill = "blue", color = "white") +
  # scale_size_continuous(name = "Health Facility Rate") +
  labs(
    title = "Malaria TPR by Study Wards using HMIS 2019-2023",
    #   subtitle = "Health Facilities with High Rates Highlighted",
    caption = "u5TPR = Confirmed u5/Overall Gen. Attendance"
  ) +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    legend.position = "right",
    axis.text = element_blank(),      # Remove axis text
    axis.ticks = element_blank()      # Remove axis ticks
  )+
  theme_manuscript()


##Adding data to other prediction covariates

kn_ward_variables <- read.csv(file.path(LuPDir, "Kano_wards_variables.csv"))

# kano_tpr_data_m <- kano_tpr_data %>% 
#   rename(ward = WardName)

kn_ward_variables_updated <- kn_ward_variables %>% 
  left_join(kano_city_tpr, by = "WardCode")

write.csv(kn_ward_variables_updated, file.path(LuPDir, "Kano_wards_variables.csv"))




