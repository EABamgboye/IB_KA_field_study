user <- Sys.getenv("USERNAME")
Drive <- file.path(gsub("[\\]", "/", gsub("Documents", "", Sys.getenv("HOME"))))
LuDir <- file.path(Drive, "Documents")
LuPDir <- file.path(Drive, "Downloads")


library(readxl)

lav_df_jf <- read_excel(file.path(LuDir ,"Osun-excel", "Larva prospection January and Feb updated April 2023.xlsx"))

lav_df_m <- read_excel(file.path(LuDir ,"Osun-excel", "MARCH LARVA IBADAN AND KANO.xlsx"))

lav_df_dry <- rbind(lav_df_jf, lav_df_m)


##Dry Season Larval Habitat - Ibadan
lav_ib <- lav_df_dry %>%  dplyr::filter(State == "Oyo") %>%  group_by(`Settlement Type`, Month) %>% 
  summarise(total_mosquitoes_larva = sum(`Anopheles`, na.rm = T)) %>% ungroup()

table(lav$State, lav$`Breeding site`)

p <- ggplot(data=lav_ib, aes(x= Month, y=total_mosquitoes_larva, group = `Settlement Type`,
                             colour = `Settlement Type`))+
  #scale_x_discrete(limits=c("March")) +
  geom_point() +labs(y= "Total number of anopheles mosquitos larva caught", x = "Month of Collection (Ibadan)")+
  #geom_line()+
  ggtitle("Anopheles mosquito collected through Larva Prospection, Jan- March, 2023")+
  geom_point(size = 3.0) +
  theme(plot.title = element_text(size = 12))+
  theme_manuscript() +
  theme(
    legend.position = c(.95, .95),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6)
  ) +
  ylim(0,25)

ggsave(paste0(ResultDir,"/", Sys.Date(), "/", 'mosquitoes_collected_larva_ibadan.png'), p, width = 8, height = 6)


#locations data collection 
p <- ggplot(df_ib_a) +
  geom_sf(fill = NA) +
  geom_point(data = dplyr::filter(lav_df_dry, State=="Oyo"), 
          mapping = aes(x = Longitude, y = Latitude, col = Anopheles_Caught),
          size = 3.5, alpha = 0.3) +
  scale_color_manual(values = c(Yes = "#00A08A", No = "tomato"))+
  geom_text_repel(
    data = df_ib_a,
    aes(label =  WardName, geometry = geometry),color ='black',
    stat = "sf_coordinates", min.segment.length = 0, size = 3.5, force = 1)+
  map_theme()+ 
  labs(title= "Wards in Ibadan showing location of possible breeding sites of mosquito larvae, Jan- March, 2023")+
  coord_sf()

ggsave(paste0(ResultDir,"/", Sys.Date(), "/", 'locations_larva prospection_ibadan.png'), p, width = 8, height = 6)

##Plot Location 
#Overall
ggplot(df_ib) +
  geom_sf(fill= "NA")+
  geom_point(data= ento_lav_wet_df,  aes(geometry = geometry, size = 0.05, alpha = 0.01, col = `Anopheles_Caught`), stat= "sf_coordinates")+
  scale_color_manual(values = c(Yes = "seagreen", No = "grey"))+
  # scale_shape_manual(values = c(Formal = 16,  Informal= 17, Slum = 14))+
  geom_text_repel(
    data = df_ib,
    aes(label =  `WardName`, geometry = geometry),color ='black',
    stat = "sf_coordinates", min.segment.length = 0, size = 2.5, force = 1, max.overlaps = Inf)+
  guides(alpha = FALSE, size = FALSE) +
  map_theme()+ 
  ylab("")+
  xlab("")+
  labs(title= "Sites for Wet Season Larval Collection")+
  coord_sf()

##Plot location(Agugu)
ento_lav_wet_df_int_a <- st_intersection(ento_lav_wet_df, df_ib_a )

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
ento_lav_wet_df_int_c <- st_intersection(ento_lav_wet_df, df_ib_c )

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

##Extract data for shinny(Agugu)
##Dry Season
lav_a <- lav_df_dry %>% 
  dplyr::filter(State=="Oyo", Locality == "Agugu")

lav_a_df <- sf::st_as_sf(lav_a, coords=c('Longitude', 'Latitude'), crs=4326)


lav_agu <- lav_a_df %>% 
  dplyr::select(SN, Locality, `Settlement Type`, geometry, Anopheles_Caught, geometry)

lav_agu1 <- lav_agu %>% 
  dplyr::filter(Anopheles_Caught == "Yes")

lav_agu1 <- lav_agu1 %>% 
  dplyr::select(-Anopheles_Caught)

lav_agu1 <- lav_agu1 %>% 
  mutate(DataSource = "Lavral Prospection(Yes)")

lav_agu2 <- lav_agu %>% 
  dplyr::filter(Anopheles_Caught == "No")

lav_agu2 <- lav_agu2 %>% 
  dplyr::select(-Anopheles_Caught)

lav_agu2 <- lav_agu2 %>% 
  mutate(DataSource = "Lavral Prospection(No)")

colnames(lav_agu1) [1] <- "Serial.Number"
colnames(lav_agu1) [2] <- "Ward"
colnames(lav_agu1) [3] <- "Settlement.Type"
colnames(lav_agu2) [1] <- "Serial.Number"
colnames(lav_agu2) [2] <- "Ward"
colnames(lav_agu2) [3] <- "Settlement.Type"
  
lav_a_dff <- st_intersection(lav_a_df, df_ib_a)

ggplot(df_ib_a) +
  geom_sf(fill = NA) +
  geom_point(data = dplyr::filter(lav_df_dry, State=="Oyo", Locality == "Agugu", Anopheles_Caught == "Yes"), 
             mapping = aes(x = Longitude, y = Latitude, size = Anopheles),
             , color = "tomato",
             alpha = 0.5) +
  #scale_color_manual(values = c(Yes = "#00A08A", No = "tomato"))+
  geom_text_repel(
    data = dplyr::filter(lav_df_dry, State == "Oyo", Locality == "Agugu", Anopheles_Caught == "Yes"),
    aes(x = Longitude, y = Latitude, label = `Breeding site`),
    color = 'black',
    size = 3.5, force = 1)+
  #scale_size_continuous(range = c(1, 40))+
  map_theme()+ 
  labs(title= "Agugu ward showing location of possible breeding sites of mosquito larvae, Jan- March, 2023")+
  coord_sf()


##Dry Season
lav_o <- lav_df_dry %>% 
  dplyr::filter(State=="Oyo", Locality == "Olopomewa")

lav_o_df <- sf::st_as_sf(lav_o, coords=c('Longitude', 'Latitude'), crs=4326)


lav_olop <- lav_o_df %>% 
  dplyr::select(SN, Locality, `Settlement Type`, geometry, Anopheles_Caught, geometry)

lav_olop1 <- lav_olop %>% 
  dplyr::filter(Anopheles_Caught == "Yes")

lav_olop1 <- lav_olop1 %>% 
  dplyr::select(-Anopheles_Caught)

lav_olop1 <- lav_olop1 %>% 
  mutate(DataSource = "Lavral Prospection(Yes)")

lav_olop2 <- lav_olop %>% 
  dplyr::filter(Anopheles_Caught == "No")

lav_olop2 <- lav_olop2 %>% 
  dplyr::select(-Anopheles_Caught)

lav_olop2 <- lav_olop2 %>% 
  mutate(DataSource = "Lavral Prospection(No)")

colnames(lav_olop1) [1] <- "Serial.Number"
colnames(lav_olop1) [2] <- "Ward"
colnames(lav_olop1) [3] <- "Settlement.Type"
colnames(lav_olop2) [1] <- "Serial.Number"
colnames(lav_olop2) [2] <- "Ward"
colnames(lav_olop2) [3] <- "Settlement.Type"

lav_o_dff <- st_intersection(lav_o_df, df_ib_o)

# ggplot(df_ib_a) +
#   geom_sf(fill = NA) +
#   geom_point(data = dplyr::filter(lav_df_dry, State=="Oyo", Locality == "Agugu", Anopheles_Caught == "Yes"), 
#              mapping = aes(x = Longitude, y = Latitude, size = Anopheles),
#              , color = "tomato",
#              alpha = 0.5) +
#   #scale_color_manual(values = c(Yes = "#00A08A", No = "tomato"))+
#   geom_text_repel(
#     data = dplyr::filter(lav_df_dry, State == "Oyo", Locality == "Agugu", Anopheles_Caught == "Yes"),
#     aes(x = Longitude, y = Latitude, label = `Breeding site`),
#     color = 'black',
#     size = 3.5, force = 1)+
#   #scale_size_continuous(range = c(1, 40))+
#   map_theme()+ 
#   labs(title= "Agugu ward showing location of possible breeding sites of mosquito larvae, Jan- March, 2023")+
#   coord_sf()

##Wet Season
lav_aguw <- ento_lav_wet_df_int_a %>% 
  dplyr::select(Household.Code.Number, Ward.Name, `Settlement.Type`, geometry, Anopheles_Caught)

lav_aguw1 <- lav_aguw %>% 
  dplyr::filter(Anopheles_Caught == "Yes")

lav_aguw1 <- lav_aguw1 %>% 
  dplyr::select(-Anopheles_Caught)

lav_aguw1 <- lav_aguw1 %>% 
  mutate(DataSource = "Lavral Prospection(Yes)")

lav_agu2 <- lav_agu %>% 
  dplyr::filter(Anopheles_Caught == "No")

lav_agu2 <- lav_agu2 %>% 
  dplyr::select(-Anopheles_Caught)

lav_agu2 <- lav_agu2 %>% 
  mutate(DataSource = "Lavral Prospection(No)")

colnames(lav_aguw1) [1] <- "Serial.Number"
colnames(lav_aguw1) [2] <- "Ward"
colnames(lav_aguw1) [3] <- "Settlement.Type"
colnames(lav_agu2) [1] <- "Serial.Number"
colnames(lav_agu2) [2] <- "Ward"
colnames(lav_agu2) [3] <- "Settlement.Type"

#lav_a_dff <- st_intersection(lav_a_df, df_ib_a)

# ggplot(df_ib_a) +
#   geom_sf(fill = NA) +
#   geom_point(data = dplyr::filter(lav_df_dry, State=="Oyo", Locality == "Agugu", Anopheles_Caught == "Yes"), 
#              mapping = aes(x = Longitude, y = Latitude, size = Anopheles),
#              , color = "tomato",
#              alpha = 0.5) +
#   #scale_color_manual(values = c(Yes = "#00A08A", No = "tomato"))+
#   geom_text_repel(
#     data = dplyr::filter(lav_df_dry, State == "Oyo", Locality == "Agugu", Anopheles_Caught == "Yes"),
#     aes(x = Longitude, y = Latitude, label = `Breeding site`),
#     color = 'black',
#     size = 3.5, force = 1)+
#   #scale_size_continuous(range = c(1, 40))+
#   map_theme()+ 
#   labs(title= "Agugu ward showing location of possible breeding sites of mosquito larvae, Jan- March, 2023")+
#   coord_sf()


# ggplot(df_ib_o) +
#   geom_sf(fill = NA) +
#   geom_point(data = dplyr::filter(lav_df_dry, State=="Oyo", Locality == "Olopomewa", Anopheles_Caught == "Yes"), 
#              mapping = aes(x = Longitude, y = Latitude, size = Anopheles), 
#              color = "green",
#              alpha = 0.5) +
#   #scale_color_manual(values = c(Yes = "#00A08A", No = "tomato"))+
#   geom_text_repel(
#     data = dplyr::filter(lav_df_dry, State == "Oyo", Locality == "Olopomewa", Anopheles_Caught == "Yes"),
#     aes(x = Longitude, y = Latitude, label = `Breeding site`),
#     color = 'black',
#     size = 3.5, force = 1)+
#   map_theme()+ 
#   labs(title= "Olopomewa Ward showing location of possible breeding sites of mosquito larvae, Jan- March, 2023")+
#   coord_sf()

##Extract data for shinny(Challenge)
lav_chalw <- ento_lav_wet_df_int_c %>% 
  dplyr::select(Household.Code.Number, Ward.Name, `Settlement.Type`, geometry, Anopheles_Caught)

lav_chalw1 <- lav_chalw %>% 
  dplyr::filter(Anopheles_Caught == "Yes")

lav_chalw1 <- lav_chalw1 %>% 
  dplyr::select(-Anopheles_Caught)

lav_chalw1 <- lav_chalw1 %>% 
  mutate(DataSource = "Lavral Prospection(Yes)")

# lav_chal2 <- lav_chal %>% 
#   dplyr::filter(Anopheles_Caught == "No")
# 
# lav_chal2 <- lav_chal2 %>% 
#   dplyr::select(-Anopheles_Caught)
# 
# lav_chal2 <- lav_chal2 %>% 
#   mutate(DataSource = "Lavral Prospection(No)")

colnames(lav_chalw1) [1] <- "Serial.Number"
colnames(lav_chalw1) [2] <- "Ward"
colnames(lav_chalw1) [3] <- "Settlement.Type"
# colnames(lav_chal2) [1] <- "Serial.Number"
# colnames(lav_chal2) [2] <- "Ward"
# colnames(lav_chal2) [3] <- "Settlement.Type"







####Prepare data for CDC
cdc <- cdc %>% 
  mutate(Anopheles_Caught = ifelse(`Total Anopheles` > 0, "Yes", "No"))

#locations of data collection 
p <- ggplot(df_ib) +
  geom_sf(fill = NA) +
  geom_point(data = dplyr::filter(cdc, State=="Oyo"), 
             mapping = aes(x = Longitude, y = Latitude, col = Anopheles_Caught),
             size = 2.5, alpha = 0.3) +
  scale_color_manual(values = c(Yes = "#00A08A", No = "tomato"))+
  geom_text_repel(
    data = df_ib,
    aes(label =  WardName, geometry = geometry),color ='black',
    stat = "sf_coordinates", min.segment.length = 0, size = 3.5, force = 1)+
  map_theme()+ 
  labs(title= "Wards in Ibadan showing location of possible breeding sites of mosquito larvae, Jan- March, 2023")+
  coord_sf()

ggsave(paste0(ResultDir,"/", Sys.Date(), "/", 'locations_larva prospection_ibadan.png'), p, width = 8, height = 6)

##Extract data for shinny CDC (Agugu)
cdc_a <- cdc %>% 
  dplyr::filter(State=="Oyo", `Ward Name` == "Agugu")

cdc_a_df <- sf::st_as_sf(cdc_a, coords=c('Longitude', 'Latitude'), crs=4326)

cdc_a_df <- cdc_a_df %>% 
  mutate(SN = 1:336)

cdc_agu <- cdc_a_df %>% 
  dplyr::select(SN, `Ward Name`, `Settlement Classification`, geometry, Anopheles_Caught)

cdc_agu1 <- cdc_agu %>% 
  dplyr::filter(Anopheles_Caught == "Yes")

cdc_agu1 <- cdc_agu1 %>% 
  dplyr::select(-Anopheles_Caught)

cdc_agu1 <- cdc_agu1 %>% 
  mutate(DataSource = "cdc(Yes)")

cdc_agu2 <- cdc_agu %>% 
  dplyr::filter(Anopheles_Caught == "No")

cdc_agu2 <- cdc_agu2 %>% 
  dplyr::select(-Anopheles_Caught)

cdc_agu2 <- cdc_agu2 %>% 
  mutate(DataSource = "cdc(No)")

colnames(cdc_agu1) [1] <- "Serial.Number"
colnames(cdc_agu1) [2] <- "Ward"
colnames(cdc_agu1) [3] <- "Settlement.Type"
colnames(cdc_agu2) [1] <- "Serial.Number"
colnames(cdc_agu2) [2] <- "Ward"
colnames(cdc_agu2) [3] <- "Settlement.Type"

lav_a_dff <- st_intersection(lav_a_df, df_ib_a)

ggplot(df_ib_a) +
  geom_sf(fill = NA) +
  geom_point(data = dplyr::filter(lav_df_dry, State=="Oyo", Locality == "Agugu", Anopheles_Caught == "Yes"), 
             mapping = aes(x = Longitude, y = Latitude, size = Anopheles),
             , color = "tomato",
             alpha = 0.5) +
  #scale_color_manual(values = c(Yes = "#00A08A", No = "tomato"))+
  geom_text_repel(
    data = dplyr::filter(lav_df_dry, State == "Oyo", Locality == "Agugu", Anopheles_Caught == "Yes"),
    aes(x = Longitude, y = Latitude, label = `Breeding site`),
    color = 'black',
    size = 3.5, force = 1)+
  #scale_size_continuous(range = c(1, 40))+
  map_theme()+ 
  labs(title= "Agugu ward showing location of possible breeding sites of mosquito larvae, Jan- March, 2023")+
  coord_sf()

##Extract data for shinny CDC (Olopomewa)
cdc_o <- cdc %>% 
  dplyr::filter(State=="Oyo", `Ward Name` == "Olopomewa")

cdc_o_df <- sf::st_as_sf(cdc_o, coords=c('Longitude', 'Latitude'), crs=4326)

cdc_o_df <- cdc_o_df %>% 
  mutate(SN = 1:336)

cdc_olop <- cdc_o_df %>% 
  dplyr::select(SN, `Ward Name`, `Settlement Classification`, geometry, Anopheles_Caught)

cdc_olop1 <- cdc_olop %>% 
  dplyr::filter(Anopheles_Caught == "Yes")

cdc_olop1 <- cdc_olop1 %>% 
  dplyr::select(-Anopheles_Caught)

cdc_olop1 <- cdc_olop1 %>% 
  mutate(DataSource = "cdc(Yes)")

cdc_olop2 <- cdc_olop %>% 
  dplyr::filter(Anopheles_Caught == "No")

cdc_olop2 <- cdc_olop2 %>% 
  dplyr::select(-Anopheles_Caught)

cdc_olop2 <- cdc_olop2 %>% 
  mutate(DataSource = "cdc(No)")

colnames(cdc_olop1) [1] <- "Serial.Number"
colnames(cdc_olop1) [2] <- "Ward"
colnames(cdc_olop1) [3] <- "Settlement.Type"
colnames(cdc_olop2) [1] <- "Serial.Number"
colnames(cdc_olop2) [2] <- "Ward"
colnames(cdc_olop2) [3] <- "Settlement.Type"

lav_o_dff <- st_intersection(lav_o_df, df_ib_o)

ggplot(df_ib_o) +
  geom_sf(fill = NA) +
  geom_point(data = dplyr::filter(lav_df_dry, State=="Oyo", Locality == "Olopomewa", Anopheles_Caught == "Yes"), 
             mapping = aes(x = Longitude, y = Latitude, size = Anopheles),
             , color = "tomato",
             alpha = 0.5) +
  #scale_color_manual(values = c(Yes = "#00A08A", No = "tomato"))+
  geom_text_repel(
    data = dplyr::filter(lav_df_dry, State == "Oyo", Locality == "Olopomewa", Anopheles_Caught == "Yes"),
    aes(x = Longitude, y = Latitude, label = `Breeding site`),
    color = 'black',
    size = 3.5, force = 1)+
  #scale_size_continuous(range = c(1, 40))+
  map_theme()+ 
  labs(title= "Olopomewa ward showing location of possible breeding sites of mosquito larvae, Jan- March, 2023")+
  coord_sf()


####Prepare data for PSC
psc <- psc %>% 
  mutate(Anopheles_Caught = ifelse(`An. Gambiae` > 0, "Yes", "No"))

#locations of data collection 
p <- ggplot(df_ib) +
  geom_sf(fill = NA) +
  geom_point(data = dplyr::filter(psc, State=="Oyo", Anopheles_Caught =="Yes"), 
             mapping = aes(x = Longitude, y = Latitude, ),
             size = 3.5, alpha = 0.3) +
  #scale_color_manual(values = c(Yes = "#00A08A", No = "tomato"))+
  geom_text_repel(
    data = df_ib,
    aes(label =  WardName, geometry = geometry),color ='black',
    stat = "sf_coordinates", min.segment.length = 0, size = 3.5, force = 1)+
  map_theme()+ 
  labs(title= "Wards in Ibadan showing location of possible breeding sites of mosquito larvae, Jan- March, 2023")+
  coord_sf()

ggsave(paste0(ResultDir,"/", Sys.Date(), "/", 'locations_larva prospection_ibadan.png'), p, width = 8, height = 6)

##Extract data for shinny psc (Agugu)
psc_a <- psc %>% 
  dplyr::filter(State=="Oyo", `Ward` == "Agugu")

psc_a_df <- sf::st_as_sf(psc_a, coords=c('Longitude', 'Latitude'), crs=4326)

psc_a_dff <- st_intersection(psc_a_df, df_ib_a)

psc_agu <- psc_a_df %>% 
  dplyr::select(SN, `Ward`, `Settlement Classification`, geometry, Anopheles_Caught)

psc_agu1 <- psc_agu %>% 
  dplyr::filter(Anopheles_Caught == "Yes")

psc_agu1 <- psc_agu1 %>% 
  dplyr::select(-Anopheles_Caught)

psc_agu1 <- psc_agu1 %>% 
  mutate(DataSource = "psc(Yes)")

psc_agu2 <- psc_agu %>% 
  dplyr::filter(Anopheles_Caught == "No")

psc_agu2 <- psc_agu2 %>% 
  dplyr::select(-Anopheles_Caught)

psc_agu2 <- psc_agu2 %>% 
  mutate(DataSource = "psc(No)")

colnames(psc_agu1) [1] <- "Serial.Number"
colnames(psc_agu1) [2] <- "Ward"
colnames(psc_agu1) [3] <- "Settlement.Type"
colnames(psc_agu2) [1] <- "Serial.Number"
colnames(psc_agu2) [2] <- "Ward"
colnames(psc_agu2) [3] <- "Settlement.Type"


##Extract data for shinny psc (Olopomewa)
psc_o <- psc %>% 
  dplyr::filter(State=="Oyo", `Ward` == "Olopomewa")

psc_o_df <- sf::st_as_sf(psc_o, coords=c('Longitude', 'Latitude'), crs=4326)

psc_olop <- psc_o_df %>% 
  dplyr::select(SN, `Ward`, `Settlement Classification`, geometry, Anopheles_Caught)

psc_olop1 <- psc_olop %>% 
  dplyr::filter(Anopheles_Caught == "Yes")

psc_olop1 <- psc_olop1 %>% 
  dplyr::select(-Anopheles_Caught)

psc_olop1 <- psc_olop1 %>% 
  mutate(DataSource = "psc(Yes)")

psc_olop2 <- psc_olop %>% 
  dplyr::filter(Anopheles_Caught == "No")

psc_olop2 <- psc_olop2 %>% 
  dplyr::select(-Anopheles_Caught)

psc_olop2 <- psc_olop2 %>% 
  mutate(DataSource = "psc(No)")

colnames(psc_olop1) [1] <- "Serial.Number"
colnames(psc_olop1) [2] <- "Ward"
colnames(psc_olop1) [3] <- "Settlement.Type"
colnames(psc_olop2) [1] <- "Serial.Number"
colnames(psc_olop2) [2] <- "Ward"
colnames(psc_olop2) [3] <- "Settlement.Type"


##Household level positivity data
ib_all_wetdata_pos <- ib_all_wetdata_pos %>% 
  dplyr::select(sn, bi2, settlement, bi7_long, bi7_lat)

ib_all_wetdata_pos_df <- sf::st_as_sf(ib_all_wetdata_pos, coords=c('bi7_long', 'bi7_lat'), crs=4326)

ib_all_wetdata_pos_df <- ib_all_wetdata_pos_df %>% 
  mutate(DataSource = "Positive Households")


hh_df_pos <- ib_all_wetdata_pos_df %>% 
  mutate(settlement = as.numeric(settlement)) %>%
  mutate(Settlement.Type = recode(settlement, 
                           `1` = "Formal",
                           `2` = "Informal",
                           `3` = "Slum"))
hh_df_pos <- hh_df_pos %>% 
  mutate(bi2 = as.numeric(bi2)) %>%
  mutate(Ward = recode(bi2, 
                                  `1` = "Agugu",
                                  `2` = "Challenge",
                                  `3` = "Bashorun",
                                  `4` = "Olopomewa"))


colnames(hh_df_pos) [1] <- "Serial.Number"

hh_pos_dff <- hh_df_pos %>% 
  dplyr::select(-bi2, -settlement)

hh_pos_o <- hh_pos_dff %>% 
  dplyr::filter(Ward == "Olopomewa")

hh_pos_a <- hh_pos_dff %>% 
  dplyr::filter(Ward == "Agugu")

hh_pos_c <- hh_pos_dff %>% 
  dplyr::filter(Ward == "Challenge")

hh_pos_b <- hh_pos_dff %>% 
  dplyr::filter(Ward == "Bashorun")



##Calculating distance between larval habitats and households with positive cases
library(geosphere)

##Combine both dry and wet season points
lav_sites <- rbind(lav_agu1, lav_aguw1)


# Extract coordinates
coords_lav_sites <- st_coordinates(lav_sites)
coords_hh_pos_a <- st_coordinates(hh_pos_a)

write.csv(coords_lav_sites, file.path(LuDir, "lav_coord.csv"))

# Calculate distance matrix
library(geosphere)
distances <- distm(
  x = coords_lav_sites,  # Coordinates of the 6 points
  y = coords_hh_pos_a,  # Coordinates of the 50 points
  fun = distHaversine  # Haversine formula for distance
)

dist_df <- as.data.frame(distances)
colnames(dist_df) <- paste0("Household_", hh_pos_a$Serial.Number)  # Rename columns based on points_50 ids
rownames(dist_df) <- paste0("Point_", lav_sites$Serial.Number)   # Rename rows based on points_6 ids

###AGUGU-ANALYSIS
##Extract households with a known distance from selected LBS points
# Choose the particular point from points_6 (e.g., the first point)
cdc_a_df_y <- cdc_a_df %>% 
  dplyr::filter(Anopheles_Caught =="Yes")

cdc_a_df_n <- cdc_a_df %>% 
  dplyr::filter(Anopheles_Caught =="No") %>% 
  distinct(geometry, .keep_all = TRUE)

lav_a_df_y <- lav_a_df %>% 
  dplyr::filter(Anopheles_Caught =="Yes")

lav_a_df_n <- lav_a_dff %>% 
  dplyr::filter(Anopheles_Caught =="No")

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


##Map circles around specific LBS points

# Transform to a CRS with meters as units (e.g., UTM)
lav_sites_utm<- st_transform(lav_a_df_n, crs = 32633)  # Replace with appropriate UTM zone for your area
df_ib_a_utm <- st_transform(df_ib_a, crs = 32633)  # Same UTM zone
ag_lav_sites_an_utm <- st_transform(lav_a_df_y, crs = 32633)
cdc_pt_a_utm <- st_transform(cdc_a_df_y, crs = 32633)
psc_pt_a_utm <- st_transform(psc_a_df, crs = 32633)

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
abuffers <- st_buffer(cdc_pt_a_utm, dist = 300)


# Dummy data to create a manual legend
alegend_data <- data.frame(
  label = c("CDC HH", "Breeding sites with no Larval", "Breeding sites with Larval"),
  color = c("seagreen", "blue", "coral"),
  shape = c(16, 16, 17)
)

# Plot using ggplot2
ggplot() +
  geom_sf(data = df_ib_a_utm, fill = "aliceblue", color = "black") +  # Shapefile
  geom_sf(data =cdc_a_df_y, color = "seagreen", size = 3) +  # CDC HHs
  #geom_sf(data =psc_a_dff, color = "brown", size = 3) +  # PSC HHs
  geom_sf(data =abuffers, fill = NA,  color = "red") +  # Buffers
  geom_sf(data = lav_sites_utm, color = "blue", size = 2) +  # Points from original dataframe
  geom_sf(data = ag_lav_sites_an_utm, color = "coral", size = 2.5, shape = 17) +  # Points from the other dataframe
  geom_point(data = alegend_data, aes(x = Inf, y = Inf, color = label), size = 3) +  # Invisible points for legend
  scale_color_manual(
    name = "Legend",
    values = c("CDC HH" = "seagreen", "Breeding sites with no Larval" = "blue", "Breeding sites with Larval" = "coral")
  ) +
  labs(title = "Geo-location of breeding sites covered within 300m of a cdc household") +
  theme_manuscript()



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

particular_o_cdc <-cdc_o_df[180, ]



##Map circles around specific LBS points

# Transform to a CRS with meters as units (e.g., UTM)
lav_sites_o_utm<- st_transform(lav_o_df_n, crs = 32633)  # Replace with appropriate UTM zone for your area
df_ib_o_utm <- st_transform(df_ib_o, crs = 32633)  # Same UTM zone
ol_lav_sites_an_utm <- st_transform(lav_o_df_y, crs = 32633)
cdc_pt_o_utm <- st_transform(psc_o_df_y, crs = 32633)

# Assuming you have another dataframe with points, e.g., `other_points_utm`

# Create 100-meter buffers around each point
obuffers <- st_buffer(cdc_pt_o_utm, dist = 300)

st_area(obuffers)

# # Plot using ggplot2
# ggplot() +
#   geom_sf(data = df_ib_o_utm, fill = "aliceblue", color = "black") +  # Shapefile
#   #geom_sf(data =cdc_o_df, color = "seagreen", size = 3) +  # CDC HHs
#   geom_sf(data = psc_o_df_y, color = "brown", size = 3) +  # PSC HHs
#   geom_sf(data = obuffers, fill = NA,  color = "red") +  # Buffers
#   geom_sf(data = lav_sites_o_utm, color = "blue", size = 2) +  # Points from original dataframe
#   geom_sf(data = ol_lav_sites_an_utm, color = "coral", size = 2.5, shape = 17) +  # Points from the other dataframe
#     labs(title = "Geo-location of breeding sites covered within 300m of a psc household") +
#   map_theme()+
#   theme_manuscript()



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
ggplot() +
  geom_sf(data = df_ib_o_utm, fill = "aliceblue", color = "black")+  # Shapefile
  geom_sf(data = psc_o_df_y, color = "brown", size = 3) +  # PSC HHs
  geom_sf(data = obuffers, fill = NA,  color = "red") +  # Buffers
  geom_sf(data = lav_sites_o_utm, color = "blue", size = 2) +  # Original Points
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





##Larval Habitat - Kano
lav_ko <- lav %>%  dplyr::filter(State == "Kano") %>%  group_by(`Settlement Type`, Month) %>% 
  summarise(total_mosquitoes = sum(`Anopheles`, na.rm = T)) %>% ungroup()

#table(psc_ko$Day, psc_ko$Month)

p <- ggplot(data=lav_ko, aes(x= Month, y=total_mosquitoes, group = `Settlement Type`,
                             colour = `Settlement Type`))+
  scale_x_discrete(limits=c("January", "February")) +
  geom_point() +labs(y= "Total number of anopheles mosquitos caught", x = "Month of Collection (Ibadan)")+
  geom_line()+
  ggtitle("Anopheles mosquito collected through Larva Prospection, January 13 and February 5, 2023")+geom_point(size = 3.0) +
  theme(plot.title = element_text(size = 12))+
  theme_manuscript() +
  theme(
    legend.position = c(.95, .95),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6)
  ) +
  ylim(0,2)

ggsave(paste0(ResultDir,"/", Sys.Date(), "/", 'mosquitoes_collected_larva_kano.png'), p, width = 8, height = 6)


#locations data collection 
p <- ggplot(df_ko) +
  geom_sf(fill = NA) +
  #geom_sf_text(data = df, aes(label = WardName), colour = "black")+
  geom_point(data = dplyr::filter(lav, State=="Kano"), mapping = aes(x = Longitude, y = Latitude), colour = "green", size = 5.0) +
  geom_text_repel(
    data = df_ko,
    aes(label =  WardName, geometry = geometry),color ='black',
    stat = "sf_coordinates", min.segment.length = 0, size = 3.5, force = 1)+
  map_theme()+ 
  labs(title= "Wards in Kano showing location of possible breeding sites of mosquito larvae, Jan-Feb, 2023")+
  coord_sf()

ggsave(paste0(ResultDir,"/", Sys.Date(), "/", 'locations_larva prospection_kano.png'), p, width = 8, height = 6)


##Breeding Site Analysis
colnames(lav)[colnames(lav) == "Breeding site"] <- "Breeding.site"
colnames(lav)[colnames(lav) == "Settlement Type"] <- "Settlement.Type"
names(lav)

# df_bs <- table(lav$Breeding.site)
# df_bs <- as.data.frame(df_bs)

df_bss <- lav %>% group_by(`Settlement.Type`, `Month`, `Breeding.site`) %>% 
  summarise(total_mosquitoes = sum(`Anopheles`, na.rm = T)) %>% ungroup()

# colnames(df_bs) [1] <- "Breeding_Site"
# colnames(df_bs) [2] <- "Frequency"

ggplot(data=df_bss, aes(x=Settlement.Type, y=total_mosquitoes, fill = Breeding.site)) +
  geom_bar(stat="identity")+
  geom_text(aes(label=total_mosquitoes), vjust=1.6, color="black", size=3.5)+
  ylim(0,50)+
  ggtitle("Distribution of breeding sites visited in both Kano and Ibadan, Jan-Feb, 2023")+
  theme_manuscript()


#locations data collection
p <- ggplot(dfk) +
  geom_sf(fill = NA) +
  #geom_sf_text(data = df, aes(label = WardName), colour = "black")+
  geom_point(data = filter(larv, State=="Kano"), mapping = aes(x = Longitude, y = Latitude), colour = "green", size = 3.5) +
  geom_text_repel(
    data = dfk,
    aes(label =  WardName, geometry = geometry),color ='black',
    stat = "sf_coordinates", min.segment.length = 0, size = 3.5, force = 1)+
  map_theme()+
  labs(title= "Wards in Kano showing sites visited for larva prospection, Jan-Feb, 2023")+
  coord_sf()

##Combined Plots
names(cdc)
names(psc)
names(lav)
