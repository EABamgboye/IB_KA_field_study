user <- Sys.getenv("USERNAME")
Datadir <- file.path("C:/Users/ebamgboye/Urban Malaria Proj Dropbox/urban_malaria/data/nigeria")
ReqDir <- file.path(Datadir, "nigeria/nigeria_hmis", "HF Data 2019-2023")


##Read in HMIS data
hmis_2019 <- read_excel(file.path(Datadir, "nigeria/nigeria_hmis", "HF Data 2019-2023", "HF Data 2019-2023", "NMEP Malaria 2019 Data.xlsx"), sheet = 1)
hmis_2020 <- read_excel(file.path(Datadir, "nigeria/nigeria_hmis", "HF Data 2019-2023", "HF Data 2019-2023", "NMEP Malaria 2020 Data.xlsx"), sheet = 1)
hmis_2021 <- read_excel(file.path(Datadir, "nigeria/nigeria_hmis", "HF Data 2019-2023", "HF Data 2019-2023", "NMEP Malaria 2021 HF Data.xlsx"), sheet = 1)
hmis_2022 <- read.csv(file.path(Datadir, "nigeria/nigeria_hmis", "HF Data 2019-2023", "HF Data 2019-2023", "2022 HF Data", "2022 HF Data",
                                "Taraba 2022.csv"))
hmis_2023 <- read.csv(file.path(Datadir, "nigeria/nigeria_hmis", "HF Data 2019-2023", "HF Data 2019-2023", "2023 HF Data", "2023 HF Data",
                                "Taraba 2023.csv"))


#Extracting Primary health care facilities alone, Taraba and other relevant variables
ta_hmis_2019_p <- hmis_2019 %>% 
  dplyr::filter(state == "ta Taraba State") %>% 
  dplyr::select(state, lga, ward, facilityname, opd, conf_u5) %>% 
  dplyr::filter(str_detect(facilityname, "Primary Health Center"))%>% 
  rename(HF_type = `facilityname`,
         outpatient = `opd`,
         rdt_u5 = `conf_u5`)

ta_hmis_2020_p <- hmis_2020 %>% 
  dplyr::filter(state == "ta Taraba State") %>% 
  dplyr::select(state, lga, ward, level_of_care, opd, conf_u5) %>% 
  dplyr::filter(level_of_care == "Primary") %>% 
  rename(HF_type = `level_of_care`,
         outpatient = `opd`,
         rdt_u5 = `conf_u5`)

ta_hmis_2021_p <- hmis_2021 %>% 
  dplyr::filter(State == "ta Taraba State") %>% 
  dplyr::select(State, LGA, Ward, Level, `Out-patient Attendance`, `Persons tested positive for malaria by RDT <5yrs`) %>% 
  dplyr::filter(Level == "Primary Health Facility")%>% 
  rename( state = State,
          lga = LGA,
          ward = Ward,
          HF_type = `Level`,
          outpatient = `Out-patient Attendance`,
          rdt_u5 = `Persons tested positive for malaria by RDT <5yrs`)

ta_hmis_2022_p <- hmis_2022 %>% 
  dplyr::select(organisationunitname.1, orgunitlevel2, orgunitlevel3, orgunitlevel4, `Out.patient.Attendance`, 
                `Persons.tested.positive.for.malaria.by.RDT..5yrs`) %>% 
  dplyr::filter(organisationunitname.1 == "Primary Health Facility")%>% 
  rename(state = orgunitlevel2,
         lga = orgunitlevel3,
         ward = orgunitlevel4,
         HF_type = `organisationunitname.1`,
         outpatient = `Out.patient.Attendance`,
         rdt_u5 = `Persons.tested.positive.for.malaria.by.RDT..5yrs`)


ta_hmis_2023_p <- hmis_2023 %>% 
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
merged_ta_hmis_2019_2023 <- rbind(ta_hmis_2019_p, ta_hmis_2020_p, ta_hmis_2021_p, ta_hmis_2022_p, ta_hmis_2023_p)

Tarabatpr_201923 <- merged_ta_hmis_2019_2023 %>% 
  group_by(ward) %>% #HF and level added
  summarise(
    u5_tpr_rdt = sum(rdt_u5, na.rm = TRUE) / sum(outpatient, na.rm = TRUE) * 100,  
  ) %>% 
  ungroup()

#Clean up names for ease of merging with shapefile
Tarabatpr_201923 <- Tarabatpr_201923 %>%
  mutate(ward = gsub("^ta ", "", ward), 
         ward = gsub("^ta ", "", ward), # Remove "ta " prefix
         ward = gsub(" Ward$", "", ward),  # Remove " Ward" suffix
  )

##Merge Taraba shapefile to tpr data
ta_wardshp <- st_read(file.path(Datadir,"nigeria", "nigeria_shapefiles", "Nigeria Boundary Files_All", "Boundary_VaccWards_Export", "Boundary_VaccWards_Export.shp"))%>% 
  dplyr::filter(StateCode == "TA")


Tarabatpr_201923 <- Tarabatpr_201923 %>%
  rename(WardName = ward)  # Replace 'WardName' with the actual column name in kano_u5_tpr_df

# Perform the merge
Taraba_tpr_data <- ta_wardshp %>%
  left_join(Tarabatpr_201923, by = "WardName")

Taraba_tpr_data <- st_set_crs(Taraba_tpr_data, 4326)

##Fill up wards with nearest neighbours
w <- spdep::poly2nb(Taraba_tpr_data, queen = TRUE)
w_listw <- spdep::nb2listw(w)


# Compute the average test positivity rate from neighboring polygons

mean_neighbors <- weights(w_listw, Taraba_tpr_data$u5_tpr_rdt)

missing_indices <- which(is.na(Taraba_tpr_data$u5_tpr_rdt))

neighbors_list <- w_listw$neighbours

neighbors_list <- w_listw$neighbours

# Impute missing 'tpr_u5' values with the mean of neighboring values
for (index in seq_along(missing_indices)) {
  polygon <- missing_indices[index]
  neighbor_tprs <- neighbors_list[[polygon]]
  Taraba_tpr_data$u5_tpr_rdt[polygon] <- mean(Taraba_tpr_data$u5_tpr_rdt[neighbor_tprs], na.rm = TRUE)
}


ggplot(data = Taraba_tpr_data) +
  geom_sf(aes(fill = u5_tpr_rdt)) +  # Replace with the correct column for prevalence
  scale_fill_gradient(low = "cadetblue", high = "deeppink", na.value = "white",
                      name = "Malaria u5TPR(Burden)") +
  # geom_text(
  #   aes(x = X, y = Y, label = round(u5_tpr3_rdt, 1)),  # Use the centroid coordinates
  #   vjust = 1.2, color = "black", size = 3.5
  # )
  # geom_sf(data = health_facilities_sf, aes(size = rate), shape = 21, fill = "blue", color = "white") +
  # scale_size_continuous(name = "Health Facility Rate") +
  labs(
    title = "Malaria TPR by Study Wards using HMIS 2019-2023(Taraba)",
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

Taraba_tpr_data <- Taraba_tpr_data %>% 
  dplyr::select(WardName, u5_tpr_rdt) %>% 
  st_drop_geometry()

write.csv(Taraba_tpr_data, file.path(LuDir, "Tarabatpr.csv" ))

library(readxl)

ta_itn_df <-read_excel(file.path(Datadir, "ITN_distribution","household_member_ward_summaries_Itn_distribution", "ITN_distribution_total_ward_taraba_2022.xlsx")) 

ta_itn_df <- ta_itn_df %>% 
  rename(WardName = Ward)

library(dplyr)

# Example dataframes
df1 <- data.frame(Name = c("Alice", "Bob", "Charlie", "David"))
df2 <- data.frame(Name = c("Alice", "Bob", "Chris", "Daniel"))

# Compare the 'Name' columns and flag mismatches
comparison <- ta_itn_df %>%
  mutate(
    Match = ifelse(WardName %in% ta_wardshp$WardName, "Match", "No Match")
  )

print(comparison)

write.csv(comparison, file.path(LuDir, "taraba_mismatch.csv"))
