rm(list=ls())
#memory.limit(size = 50000)
## -----------------------------------------
### Paths
## -----------------------------------------

user <- Sys.getenv("USERNAME")
Drive <- file.path(gsub("[\\]", "/", gsub("Documents", "", Sys.getenv("HOME"))))
NuDir <- file.path(Drive, "urban_malaria")
NuCDir <- file.path(Drive, "my_stuff")
ProjectDir <- file.path(NuDir, "data", 'nigeria','nigeria_dhs' , 'data_analysis')
DataDir <- file.path(ProjectDir, 'data', 'DHS', 'Downloads')

#=======
user <- Sys.getenv("USER")
if ("ifeomaozodiegwu" %in% user) {
  Drive <- file.path(gsub("[\\]", "/", gsub("Documents", "", Sys.getenv("HOME"))))
  NuDir <- file.path(Drive, "Library", "CloudStorage", "OneDrive-NorthwesternUniversity", "urban_malaria")
  EntoDat <- file.path(NuDir, "data", "nigeria", "kano_ibadan_ento", "Osun-excel")
  ResultDir <-file.path(NuDir, "projects/project_implementation/analysis_output/ento_plots")
  shapepath <- file.path(NuDir,"/data/nigeria/kano_ibadan_shape_files")

} else {
  user <- Sys.getenv("USERNAME")
  Drive <- file.path(gsub("[\\]", "/", gsub("Documents", "", Sys.getenv("HOME"))))
  NuDir <- file.path(Drive, "urban_malaria")
  shapepath <- file.path(NuDir,"/data/nigeria/kano_ibadan_shape_files")
  NuCDir <- file.path(Drive, "my_stuff")
  ProjectDir <- file.path(NuDir, "data", 'nigeria','nigeria_dhs' , 'data_analysis')
  EntoDat <- file.path(LuDir, "Osun-excel")
  ResultDir <-file.path(NuDir, "projects/project_implementation/analysis_output/ento_plots")
  DataDir <- file.path(ProjectDir, 'data', 'DHS', 'Downloads')
}


##----------------------------------------------------------------------------
#Libraries--------------------------------------------------------------------
##----------------------------------------------------------------------------

library(readxl)
library(sf)
library(vcd)
library(ggplot2)
library(tmap)
library(ggrepel)
library(tidyverse)
library(geometry)
library(dplyr)
library(rgdal)
library(fun)
library(patchwork)
library(rgeos)
library(maptools)
library(purrr)
library(DescTools)
library(conflicted)
library(stringr)


map_theme <- function(){
  theme(axis.text.x = ggplot2::element_blank(),
        axis.text.y = ggplot2::element_blank(),
        axis.ticks = ggplot2::element_blank(),
        rect = ggplot2::element_blank(),
        plot.background = ggplot2::element_rect(fill = "white", colour = NA),
        plot.title = element_text(hjust = 0.5),
        legend.title.align=0.5,
        legend.title=element_text(size=8, colour = 'black'),
        legend.text =element_text(size = 8, colour = 'black'),
        legend.key.height = unit(0.65, "cm"))
}

con_gplot <-function(df,fill,label){
  ggplot()+
    geom_sf(data=df, mapping=aes(fill = !!fill)) +
    map_theme() +
    geom_text_repel(
      data = df,
      aes(label = !!label, geometry = geometry),color ='black',
      stat = "sf_coordinates",
      min.segment.length = 0, size = 1.5, force = 1, max.overlaps = Inf)+
    xlab('')+
    ylab('')
}

theme_manuscript <- function(){
  theme_bw() +
    theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5),
          plot.title = element_text(hjust = 0.5),
          axis.text.x = element_text(size = 16, color = "black"),
          axis.text.y = element_text(size = 16, color = "black"),
          axis.title.x = element_text(size = 16),
          axis.title.y = element_text(size =16),
          legend.title=element_text(size=16, colour = 'black'),
          legend.text =element_text(size = 16, colour = 'black'),
          legend.key.height = unit(1, "cm"))
}


##----------------------------------------------------------------------------
#Excel files--------------------------------------------------------------------
##----------------------------------------------------------------------------
files <- list.files(path = EntoDat , pattern = ".xlsx", full.names = TRUE, recursive = F)
dat <- sapply(files, read_xlsx, simplify = F)
names(dat)

cdc <- rbind(dat[[3]], dat[[4]], dat[[5]])


##Indoor Transmission Ibadan###
in_cdc <- cdc %>% dplyr::filter(Location == "Indoor", State == "Oyo") %>%  group_by(`Settlement Classification`, `Time of Collection`) %>% 
  summarise(total_mosquitoes = sum(`Total Anopheles`, na.rm = T)) %>% ungroup()

p <- ggplot(data= in_cdc, aes(x= `Time of Collection`, y=total_mosquitoes, group = `Settlement Classification`,
                                           colour = `Settlement Classification`))+
  scale_x_discrete(limits = c("6-7pm", "7-8pm", "8-9pm", "9-10pm", "10-11pm", "11-12am", "12-1am", "1-2am", "2-3am", "3-4am", "5-6am"))+
  geom_point() +labs(y= "Total number of anopheles mosquitos caught/hr.", x = "Time of collection (Ibadan)")+
  geom_line()+
   ggtitle("Hourly indoor biting of anopheles mosquito aggregated \n over fourteen days, January - March, 2023")+geom_point(size = 3.0) +
  theme(plot.title = element_text(size = 12))+
  theme_manuscript() +
  theme(
    legend.position = c(.3, .8),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6)
  )

ggsave(paste0(ResultDir,"/", Sys.Date(), "/", 'indoor_cdc_ibadan.png'), p, width = 8, height = 6)

# min_cdc <- cdc %>% dplyr::filter(Location == "Indoor", State == "Oyo") %>%  group_by(`Month`, `Time of Collection`) %>% 
#   summarise(total_mosquitoes = sum(`Total Anopheles`, na.rm = T)) %>% ungroup()
# 
# p <- ggplot(data= min_cdc, aes(x= `Time of Collection`, y=total_mosquitoes, group = `Month`,
#                               colour = `Month`))+
#   scale_x_discrete(limits = c("6-7pm", "7-8pm", "8-9pm", "9-10pm", "10-11pm", "11-12am", "12-1am", "1-2am", "2-3am", "3-4am", "5-6am"))+
#   geom_point() +labs(y= "Total number of anopheles mosquitos caught/hr.", x = "Time of collection (Ibadan)")+
#   geom_line()+
#   ggtitle("Hourly indoor biting of anopheles mosquito aggregated \n over fourteen days, January - March, 2023")+geom_point(size = 3.0) +
#   theme(plot.title = element_text(size = 12))+
#   theme_manuscript() +
#   theme(
#     legend.position = c(.3, .8),
#     legend.justification = c("right", "top"),
#     legend.box.just = "right",
#     legend.margin = margin(6, 6, 6, 6)
#   )


##Outdoor Transmission- Ibadan
out_cdc <- cdc %>%  dplyr::filter(Location == "Outdoor", State == "Oyo") %>%  group_by(`Settlement Classification`, `Time of Collection`) %>% 
  summarise(total_mosquitoes = sum(`Total Anopheles`)) %>% ungroup()

p <- ggplot(data= out_cdc, aes(x= `Time of Collection`, y=total_mosquitoes, group = `Settlement Classification`,
                              colour = `Settlement Classification`))+
  scale_x_discrete(limits = c("6-7pm", "7-8pm", "8-9pm", "9-10pm", "10-11pm", "11-12am", "12-1am", "1-2am", "2-3am", "3-4am", "5-6am"))+
  geom_point() +labs(y= "Total number of anopheles mosquitos caught/hr.", x = "Time of collection (Ibadan)")+
  geom_line()+
  ggtitle("Hourly indoor biting of anopheles mosquito aggregated \n over fourteen days, January - March, 2023")+geom_point(size = 3.0) +
  theme(plot.title = element_text(size = 12))+
  theme_manuscript() +
  theme(
    legend.position = c(.3, .8),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6)
  )

ggsave(paste0(ResultDir,"/", Sys.Date(), "/", 'outdoor_cdc_ibadan.png'), p, width = 8, height = 6)

## read ibadan ward shape files
df_ib = st_read(file.path(shapepath, "ibadan_metro_ward_fiveLGAs", "Ibadan_metro_fiveLGAs.shp")) %>%
  mutate(WardName = ifelse(WardName == 'Oranyan' & LGACode == '31007', 'Oranyan_7', WardName))

df_ib$ward_color <- ifelse(df_ib$WardName %in% c("Agugu", "Olopomewa", "Challenge", "Bashorun"), 
                           "Sampled", "Unsampled")
ib_w <- df_ib$WardName

write.csv(ib_w, file.path("NuDPDir", "ib_wards.csv"), row.names = FALSE)
          
p <- ggplot(df_ib) +
  geom_sf(aes(fill = ward_color)) +
  scale_fill_manual(
    values = c("Sampled" = "light green", "Unsampled" = "grey"), 
    na.value = "transparent"  # Set NA values to transparent
  )+
  #geom_sf_text(data = df, aes(label = WardName), colour = "black")+
  #geom_point(data = dplyr::filter(cdc, State=="Oyo"), mapping = aes(x = Longitude, y = Latitude), colour = "red", size = 2.5) +
  geom_text_repel(
    data = df_ib,
    aes(label =  WardName, geometry = geometry),color ='black',
    stat = "sf_coordinates", min.segment.length = 0, size = 3.5, force = 1)+
  map_theme()+ 
  labs(title= "Wards in Ibadan showing households visited for indoor \n and outdoor mosquito collection using CDC light trap, Jan-March, 2023")+
  coord_sf()

ggsave(paste0(ResultDir,"/", Sys.Date(), "/", 'locations_cdc_ibadan3.png'), p, width = 8, height = 6)


p <- ggplot(df_ib) +
  geom_sf(aes(fill = ward_color)) +
  scale_fill_manual(
    values = c("Sampled" = "light green", "Unsampled" = "#d2d2d2"), 
    na.value = "transparent"  # Set NA values to transparent
  )+
  #geom_sf_text(data = df, aes(label = WardName), colour = "black")+
  #geom_point(data = dplyr::filter(cdc, State=="Oyo"), mapping = aes(x = Longitude, y = Latitude), colour = "red", size = 2.5) +
  geom_text_repel(
    data = df_ib,
    aes(label =  WardName, geometry = geometry),color ='black',
    stat = "sf_coordinates", min.segment.length = 0, size = 3.5, force = 1)+
  map_theme()+ 
  labs(title= "Ibadan Metro Area showing wards selected for entomological survey, Jan-March, 2023")+
  coord_sf()

ggsave(paste0(ResultDir,"/", Sys.Date(), "/", 'Ibadan Metro showing Ento Wards.pdf'), p, width = 8, height = 6)


##----------------------------------------------------------------------------
#Kano--------------------------------------------------------------------
##----------------------------------------------------------------------------

##Indoor Transmission Kano###
in_cdc <- cdc %>%  dplyr::filter(Location == "Indoor", State == "Kano") %>%  group_by(`Settlement Classification`, `Time of Collection`) %>% 
  summarise(total_mosquitoes = sum(`Total Anopheles`, na.rm = T)) %>% ungroup()

p <- ggplot(data= in_cdc, aes(x= `Time of Collection`, y=total_mosquitoes, group = `Settlement Classification`,
                              colour = `Settlement Classification`))+
  scale_x_discrete(limits = c("6-7pm", "7-8pm", "8-9pm", "9-10pm", "10-11pm", "11-12am", "12-1am", "1-2am", "2-3am", "3-4am", "5-6am"))+
  geom_point() +labs(y= "Total number of anopheles mosquitos caught/hr.", x = "Time of collection (Kano)")+
  geom_line()+
  ggtitle("Hourly indoor biting of anopheles mosquito aggregated \n over fourteen days, January - March, 2023")+geom_point(size = 3.0) +
  theme(plot.title = element_text(size = 12))+
  theme_manuscript() +
  theme(
    legend.position = c(.95, .95),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6)
  ) +
  ylim(0, 2)

ggsave(paste0(ResultDir,"/", Sys.Date(), "/", 'indoor_cdc_kano.png'), p, width = 8, height = 6)


##Outdoor Transmission- Kano###
table(out_cdc$Day, out_cdc$Month)

out_cdc <- cdc %>%  dplyr::filter(Location == "Outdoor", State == "Kano") %>%  group_by(`Settlement Classification`, `Time of Collection`) %>% 
  summarise(total_mosquitoes = sum(`Total Anopheles`)) %>% ungroup()

p <- ggplot(data= out_cdc, aes(x= `Time of Collection`, y=total_mosquitoes, group = `Settlement Classification`,
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

ggsave(paste0(ResultDir,"/", Sys.Date(), "/", 'outdoor_cdc_kano.png'), p, width = 8, height = 6)



## read kano ward shape files
df_ko = st_read(file.path(shapepath, "Kano_metro_ward_fiveLGAs", "Kano_metro_ward_fiveLGAs.shp")) 

p <- ggplot(df_ko) +
  geom_sf(fill = NA) +
  #geom_sf_text(data = df, aes(label = WardName), colour = "black")+
  geom_point(data = dplyr::filter(cdc, State=="Kano"), mapping = aes(x = Longitude, y = Latitude), colour = "red", size = 2.5) +
  geom_text_repel(
    data = df_ko,
    aes(label =  WardName, geometry = geometry),color ='black',
    stat = "sf_coordinates", min.segment.length = 0, size = 3.5, force = 1)+
  map_theme()+ 
  labs(title= "Wards in Kano showing households visited for indoor \n and outdoor mosquito collection using CDC light trap, Jan-March, 2023")+
  coord_sf()

ggsave(paste0(ResultDir,"/", Sys.Date(), "/", 'locations_cdc_kano2.png'), p, width = 8, height = 6)


df_ko$ward_color <- ifelse(df_ko$WardName %in% c("Zango", "Dorayi", "Tudun Wazurchi"), 
                           "Sampled", "Unsampled")

p <- ggplot(df_ko) +
  geom_sf(aes(fill = ward_color)) +
  scale_fill_manual(
    values = c("Sampled" = "light green", "Unsampled" = "#d2d2d2"), 
    na.value = "transparent"  # Set NA values to transparent
  )+
  #geom_sf_text(data = df, aes(label = WardName), colour = "black")+
  #geom_point(data = dplyr::filter(cdc, State=="Kano"), mapping = aes(x = Longitude, y = Latitude), colour = "red", size = 2.5) +
  geom_text_repel(
    data = df_ko,
    aes(label =  WardName, geometry = geometry),color ='black',
    stat = "sf_coordinates", min.segment.length = 0, size = 3.5, force = 1)+
  map_theme()+ 
  labs(title= "Kano Metro showing Wards selected for Entomological Survey, Jan-March, 2023")+
  coord_sf()

ggsave(paste0(ResultDir,"/", Sys.Date(), "/", 'Kano Metro showing Ento Wards.pdf'), p, width = 8, height = 6)


##PSC - Ibadan
names(dat)
psc <- rbind(dat[[6]], dat[[7]], dat[[8]])
names(psc)
table(psc$`An. Gambiae`)
table(psc$An.Funestus)


psc_ib <- psc %>%  dplyr::filter(State == "Oyo") %>%  group_by(`Settlement Classification`, Month) %>% 
  summarise(total_mosquitoes = sum(`An. Gambiae`, na.rm = T)) %>% ungroup()


p <- ggplot(data=psc_ib , aes(x= Month, y=total_mosquitoes, group = `Settlement Classification`,
                               colour = `Settlement Classification`))+
  scale_x_discrete(limits=c("January", "February", "March")) +
  geom_point() +labs(y= "Total number of anopheles mosquitos caught", x = "Month of Collection (Ibadan)")+
  geom_line()+
  ggtitle("Anopheles mosquito collected through Pyrethrum Spray Catches, \n January - March 2023")+geom_point(size = 3.0) +
  theme(plot.title = element_text(size = 12))+
  theme_manuscript() +
  theme(
    legend.position = c(.95, .95),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6)
  ) +
  ylim(0,4)

ggsave(paste0(ResultDir,"/", Sys.Date(), "/", 'mosquitoes_collected_psc_ibadan.png'), p, width = 8, height = 6)

#Locations data collection 
p <- ggplot(df_ib) +
  geom_sf(fill = NA) +
  #geom_sf_text(data = df, aes(label = WardName), colour = "black")+
  geom_point(data = dplyr::filter(psc, State=="Oyo"), mapping = aes(x = Longitude, y = Latitude), colour = "blue", size = 2.5) +
  geom_text_repel(
    data = df_ib,
    aes(label =  WardName, geometry = geometry),color ='black',
    stat = "sf_coordinates", min.segment.length = 0, size = 3.5, force = 1)+
  map_theme()+ 
  labs(title= "Wards in Ibadan showing households visited for indoor \n and outdoor mosquito collection using Pyrethrum Spray Catch, Jan-March, 2023")+
  coord_sf()

ggsave(paste0(ResultDir,"/", Sys.Date(), "/", 'locations_psc_ibadan2.png'), p, width = 8, height = 6)



##PSC - Kano
psc_ko <- psc %>%  dplyr::filter(State == "Kano") %>%  group_by(`Settlement Classification`, Month) %>% 
  summarise(total_mosquitoes = sum(`An. Gambiae`, na.rm = T)) %>% ungroup()

table(psc_ko$Day, psc_ko$Month)

p <- ggplot(data=psc_ko, aes(x= Month, y=total_mosquitoes, group = `Settlement Classification`,
                              colour = `Settlement Classification`))+
  scale_x_discrete(limits=c("January", "February", "March")) +
  geom_point() +labs(y= "Total number of anopheles mosquitos caught", x = "Month of Collection (Kano)")+
  geom_line()+
  ggtitle("Anopheles mosquito collected through Pyrethrum Spray Catches, January - March, 2023")+geom_point(size = 3.0) +
  theme(plot.title = element_text(size = 12))+
  theme_manuscript() +
  theme(
    legend.position = c(.95, .95),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6)
  ) +
  ylim(0,2)

ggsave(paste0(ResultDir,"/", Sys.Date(), "/", 'mosquitoes_collected_psc_kano.png'), p, width = 8, height = 6)

#locations data collection 
p <- ggplot(df_ko) +
  geom_sf(fill = NA) +
  #geom_sf_text(data = df, aes(label = WardName), colour = "black")+
  geom_point(data = dplyr::filter(psc, State=="Kano"), mapping = aes(x = Longitude, y = Latitude), colour = "blue", size = 2.5) +
  geom_text_repel(
    data = df_ko,
    aes(label =  WardName, geometry = geometry),color ='black',
    stat = "sf_coordinates", min.segment.length = 0, size = 3.5, force = 1)+
  map_theme()+ 
  labs(title= "Wards in Kano showing households visited for indoor \n and outdoor mosquito collection using Pyrethrum Spray Catch, Jan-March, 2023")+
  coord_sf()

ggsave(paste0(ResultDir,"/", Sys.Date(), "/", 'locations_psc_kano.png'), p, width = 8, height = 6)



##NEXT STEPS###

##Larva Prospection
names(dat)
lav <- rbind(dat[[1]], dat[[2]])
names(lav)

##Larval Habitat - Ibadan
lav_ib <- lav %>%  dplyr::filter(State == "Oyo") %>%  group_by(`Settlement Type`, Month) %>% 
  summarise(total_mosquitoes = sum(`Anopheles`, na.rm = T)) %>% ungroup()

table(lav$State, lav$`Breeding site`)

p <- ggplot(data=lav_ib, aes(x= Month, y=total_mosquitoes, group = `Settlement Type`,
                             colour = `Settlement Type`))+
  #scale_x_discrete(limits=c("March")) +
  geom_point() +labs(y= "Total number of anopheles mosquitos caught", x = "Month of Collection (Ibadan)")+
  #geom_line()+
  ggtitle("Anopheles mosquito collected through Larva Prospection, March, 2023")+geom_point(size = 3.0) +
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
p <- ggplot(df_ib) +
  geom_sf(fill = NA) +
  #geom_sf_text(data = df, aes(label = WardName), colour = "black")+
  geom_point(data = dplyr::filter(lav, State=="Oyo"), mapping = aes(x = Longitude, y = Latitude), colour = "green", size = 3.5) +
  geom_text_repel(
    data = df_ib,
    aes(label =  WardName, geometry = geometry),color ='black',
    stat = "sf_coordinates", min.segment.length = 0, size = 3.5, force = 1)+
  map_theme()+ 
  labs(title= "Wards in Ibadan showing location of possible breeding sites of mosquito larvae, March, 2023")+
  coord_sf()

ggsave(paste0(ResultDir,"/", Sys.Date(), "/", 'locations_larva prospection_ibadan.png'), p, width = 8, height = 6)


view(lav)

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




##Further Analysis
specie <- c("Culicne", "Anopheles", "Culicne", "Anopheles" )
loc <- c("Ibadan", "Ibadan", "Kano", "Kano")
per <- c(1000, 23, 1661, 0)

sp1 <- data.frame(specie, per, loc)

sp1_1 <- ggplot(data=sp1, aes(x=as.factor(specie), y=per, fill = loc)) +
  geom_bar(stat="identity", position = "dodge") +
  #ggtitle("Proportion ever smoked")+
  geom_text(aes(label=per), color="black",
            position = position_dodge(width = 0.5), vjust= 1.0, size=4.0)+
  scale_fill_brewer(palette="Paired")+
  xlab("Type of Specie")+
  ylab("Number Caught")+
  theme_manuscript_2()
ggsave(paste0(CDataDir, '/maps/', Sys.Date(), 'Distribution of mosquitoes caught by Specie.pdf'), sp1_1, width = 10, height =8)

method <- c("CDC", "PSC")
per <- c(17, 6)
meth <-data.frame(method, per)

met_1 <- ggplot(data=meth, aes(x=as.factor(method), y=per, fill = method)) +
  geom_bar(stat="identity") +
  #ggtitle("Proportion ever smoked")+
  geom_text(aes(label=per), color="black",
            position = position_dodge(width = 0.5), vjust= 1.0, size=4.0)+
  scale_fill_brewer(palette="Paired")+
  xlab("Method of Mosquito Collection")+
  ylab("Number Caught")+
  theme_manuscript_2()

ggsave(paste0(CDataDir, '/maps/', Sys.Date(), 'Distribution of Anophelse caught by type of method.pdf'), met_1, width = 10, height =8)


##Household Analysis-----------------------------------------------------------##

Hh_n <- c(6, 6, 48, 60, 60 , 120)
Month <- c("January", "February", "March", "January", "February", "March")
method <- c("CDC", "CDC", "CDC", "PSC", "PSC", "PSC")

hh_visited_df <- data.frame(Hh_n, Month, method)

ggplot(hh_visited_df, aes(x = as.factor(Month), y = Hh_n))+
  geom_bar(stat = "identity", fill = "plum", width = 0.5, position = "stack") +
  facet_wrap(~ method)+
  labs(title = "Number of Households visited per Month by method of mosquito collection",
       x = "Month of Study",
       y = "Number of Household visited")+
    theme_manuscript1()+
theme(strip.background = element_rect(fill = "yellow", color = "black"))


Species <- 	c("An. gambiae s.s", "An. coluzzii","An. gambiae s.s", "An. coluzzii",
              "An. gambiae s.s", "An. coluzzii")
Location <- c("Challenge", "Challenge", "Olopomewa", "Olopomewa",
              "Agugu", "Agugu")
N <- 	c(3,2,1,0,0,0)

df_e <- data.frame(Species, Location, N)


ggplot(df_e, aes(x = as.factor(Location), y = N))+
  geom_bar(stat = "identity", fill = "plum", width = 0.5, position = "stack") +
  facet_wrap(~ Species)+
  labs(title = "Number of Households visited per Month by method of mosquito collection",
       x = "Month of Study",
       y = "Number of Household visited")+
  theme_manuscript1()+
  theme(strip.background = element_rect(fill = "yellow", color = "black"))

# Species	Number of fed mosquitoes collected 	Location
# 
# PSC                No positive for Human host	Human Blood Index (%)
# An. gambiae s.s	3	Challenge	3	1 (100)
# An. coluzzii	2	Challenge	2	1 (100)
# 
# CDC


n_c <- c(2, 14, 1, 2, 4 , 0)
bf <- c(1,2,0,2,3,0)
Specie <- c("An. coluzzii", "An. gambiae s.s", "An. funestus", "An. coluzzii", "An. gambiae s.s", "An. funestus")
method <- c("CDC", "CDC", "CDC", "PSC", "PSC", "PSC")

n_specie_df <- data.frame(n_c, bf, Specie, method)

ggplot(n_specie_df, aes(x = as.factor(Specie), y = n_c))+
  geom_bar(position = "stack", stat = "identity", fill = "#ffdbac", width = 0.5) +
  facet_wrap(~ method)+
  labs(title = "Specie Distribution of Anopheles mosquitoes caught by collection methods ",
       x = "Specie",
       y = "Number Caught")+
  theme_manuscript()+
  theme(strip.background = element_rect(fill = "#c68642", color = "black"))

##All mosquitoes
n_c <- c(4, 18, 1)
Specie <- c("An. coluzzii", "An. gambiae s.s", "An. funestus")

nc_specie_df <- data.frame(n_c, Specie)

ggplot(nc_specie_df, aes(x = as.factor(Specie), y = n_c, fill = Specie))+
  geom_bar(stat = "identity", width = 0.5) +
  scale_fill_manual(values = c(`An. coluzzii` = "#F7C815", `An. funestus`= "#EC9704", 
                               `An. gambiae s.s`="#9C4A1A"))+
    geom_text(aes(label=n_c), vjust=1.6, color="black", size=3.5)+
    labs(title = "Distribuition of Anopheles mosquitoes by Specie",
       x = "Specie",
       y = "Number Caught")+
  theme_manuscript()+
  theme(strip.background = element_rect(fill = "#c68642", color = "black"))

library(ggplot2)

Ward <- c("Olopomewa", "Olopomewa", "Olopomewa", "Olopomewa", "Olopomewa", "Olopomewa",
          "Olopomewa", "Olopomewa", "Olopomewa", "Challenge", "Challenge", "Challenge",
          "Challenge", "Challenge", "Challenge", "Challenge", "Challenge", "Challenge",
          "Agugu","Agugu","Agugu","Agugu","Agugu","Agugu","Agugu","Agugu","Agugu")
Specie <- c("An gambiae s.s", "An gambiae s.s", "An gambiae s.s", "An. coluzzii", 
            "An. coluzzii", "An. coluzzii", "An. funestus","An. funestus", "An. funestus",
            "An gambiae s.s", "An gambiae s.s", "An gambiae s.s", "An. coluzzii", 
            "An. coluzzii", "An. coluzzii", "An. funestus","An. funestus", "An. funestus",
            "An gambiae s.s", "An gambiae s.s", "An gambiae s.s", "An. coluzzii", 
            "An. coluzzii", "An. coluzzii", "An. funestus","An. funestus", "An. funestus") 
Method <- c("Indoor CDC", "Outdoor CDC", "PSC", "Indoor CDC", "Outdoor CDC", "PSC",
            "Indoor CDC", "Outdoor CDC", "PSC", "Indoor CDC", "Outdoor CDC", "PSC", 
            "Indoor CDC", "Outdoor CDC", "PSC","Indoor CDC", "Outdoor CDC", "PSC",
            "Indoor CDC", "Outdoor CDC", "PSC", "Indoor CDC", "Outdoor CDC", "PSC",
            "Indoor CDC", "Outdoor CDC","PSC")

Count <- c(3,0,1,0,0,0,0,0,0,6,4,3,0,2,2,1,0,0,1,0,0,0,0,0,0,0,0)

plot_df <- data.frame(Ward, Specie, Method, Count)

# Ward <- c("Olopomewa", "Olopomewa", "Olopomewa", "Challenge", "Challenge", "Challenge",
#           "Agugu","Agugu","Agugu")
# Specie <- c("An gambiae s.s", "An. coluzzii", "An. funestus", "An gambiae s.s", "An. coluzzii", 
#             "An. funestus", "An gambiae s.s", "An. coluzzii", "An. funestus") 
# Method <- c("Indoor CDC", "Outdoor CDC", "PSC", "Indoor CDC", "Outdoor CDC", "PSC",
#             "Indoor CDC", "Outdoor CDC", "PSC")
# 
# Count <- c(3,0,1,0,0,0,0,0,0,6,0,3,4,2,0,1,0,0,1,0,0,0,0,0,0,0,0)
# 
# plot_df <- data.frame(Ward, Specie, Method, Count)

p <- ggplot(plot_df, aes(fill=Method, y=Count, x=Specie)) + 
  geom_bar(position="stack", stat="identity")+
  scale_fill_manual(values = c(`Indoor CDC` = "#66545e", `Outdoor CDC` = "#aa6f73",
                               `PSC` = "#f6e0b5")) +
  geom_text(aes(x = Specie, y= Count, label = Count), vjust = -0.5) +
  facet_grid(~ Ward)+
  theme_manuscript()+
theme(legend.position = c(0.90, 0.85))

ggsave(paste0(ResultDir,"/", Sys.Date(), "/", 'Distribution of Anopheles caught by Method.pdf'), p, width = 8, height = 6)


#####
Ward <- c("Challenge", "Challenge", "Challenge", "Olopomewa")
Specie <- c("An. Gambiae s.s", "An. Colluzi", "An. Colluzi", "An. Gambiae s.s")
Method <- c("PSC","PSC", "CDC", "CDC")
Count <- c(3,2,1,2)

bm <- data.frame(Ward, Specie, Method, Count)

library(ggpubr)

ggballoonplot(bm)+
  facet_wrap(~ Ward)


balloon_plot <- ggballoonplot(
  data = bm,
  x = "Method",
  y = "Specie",
  size = "Count",
  color = "Method",
  legend.title = "Number",
  legend.values = "Count",
  main.title = "Balloon Plot"
)

# Display the plot
print(balloon_plot)

# Data Wrangling for CDC Indoor and Outdoor specie by Ward
data_i <- matrix(c(
  3, 0, 0,
  6, 0, 1,
  1, 0, 0
), nrow = 3, byrow = TRUE, dimnames = list(
  c("Olopomewa", "Challenge", "Agugu"),
  c("An gambiae s.s", "An. colluzii", "An funestus")))

# Convert the matrix to a data frame
data_df <- as.data.frame(data_i)
data_df$Location <- rownames(data_df)
colnames(data_df) <- c("An gambiae s.s", "An. colluzii", "An funestus", "Location")

# Convert data to long format
idata_long <- pivot_longer(data_df, cols = -Location, names_to = "Species_Location", values_to = "Count")
idata_long$Method <- "Indoor CDC"

data_o <- matrix(c(
  0, 0,0,
  4, 2,0,
  0, 0,0
), nrow = 3, byrow = TRUE, dimnames = list(
  c("Olopomewa", "Challenge", "Agugu"),
  c("An gambiae s.s", "An. colluzii", "An funestus")))

# Convert the matrix to a data frame
data_df <- as.data.frame(data_o)
data_df$Location <- rownames(data_df)
colnames(data_df) <- c("An gambiae s.s", "An. colluzii", "An funestus", "Location")

# Convert data to long format
odata_long <- pivot_longer(data_df, cols = -Location, names_to = "Species_Location", values_to = "Count")
odata_long$Method <- "Outdoor CDC"

adata_long <- rbind(idata_long, odata_long)

# Create the facet grid plot
plot <- ggplot(adata_long, aes(x = Species_Location, y = Count, fill = Species_Location)) +
  geom_bar(position = "stack", stat = "identity") +
  scale_fill_manual(values = c(`An gambiae s.s` = "#8d5524", `An. colluzii` = "#e0ac69" , `An funestus` = "#ffdbac"))+
  labs(title = "Mosquito Caught by Species,Ward and Location",
       x = "Species",
       y = "Number Caught",
       fill = "Specie")+ 
  theme_manuscript2() +
  facet_grid(~ Location)

plot <- ggplot(plot_df, aes(x = Specie, y = Count, fill = Method)) +
  geom_bar(position = "stack", stat = "identity") +
  scale_fill_manual(values = c(`Indoor` = "#8d5524", `Outdoor` = "#ffdbac"))+
  labs(title = "Mosquito Caught by Species,Ward and Location",
       x = "Species",
       y = "Number Caught",
       fill = "Specie")+ 
  theme_manuscript2() +
  facet_grid(~ Location)

print(plot)

ggplot(plot_df, aes(fill=Method, y=Count, x=Specie)) + 
  geom_bar(position="stack", stat="identity")


##Plot of the 15 larvae to adulthood by specie type

library(ggplot2)
library(tidyr)

# Create a dataframe
dt <- data.frame(
  Location = c("Gutter/Drainages", "Gutter/Drainages", "Gutter/Drainages", "Tyre tracks", "Tyre tracks", "Tyre tracks"),
  Ward = c("Agugu", "Olopomeji", "Challenge", "Agugu", "Olopomeji", "Challenge"),
  An_gambiae_ss = c(8, 3, 0, 2, 0, 0),
  An_coluzzii = c(2, 0, 0, 0, 0, 0)
  )

# Convert data to long format
data_long <- pivot_longer(dt, cols = c(An_gambiae_ss, An_coluzzii), names_to = "Species", values_to = "Value")

# Create a stacked bar plot
ggplot(data_long, aes(x = Ward, y = Value, fill = Species)) +
  geom_bar(stat = "identity") + 
  scale_fill_manual(values = c(An_gambiae_ss = "#ffdbac", An_coluzzii = "#fcbf49"))+
  facet_wrap(~Location)+
  labs(title = "Distribution of adult larvae(mosquito) by Species Type", x = "Ward", y = "Number of Adult Mosquito from Larva") +
  theme_manuscript2() +
  theme(legend.position = "right")

##Blood Meal per Anopheles collected
dtm <- data.frame(
  specie = c("An_gambiae_ss", "An_gambiae_ss", "An_coluzzii", "An_gambiae_ss", "An_coluzzii"), 
  Ward = c("Challenge", "Olopomewa", "Challenge", "Agugu", "Agugu"),
  Unfed = c(10, 2, 1, 0, 0),
  fed = c(3, 2, 3, 0,0)
)

# Convert data to long format
data_long_m <- pivot_longer(dtm, cols = c(Unfed, fed), names_to = "Blood_Meal", values_to = "Value")

# Create a stacked bar plot
ggplot(data_long_m, aes(x = specie, y = Value, fill = Blood_Meal)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c(fed = "#ef233c", Unfed = "#06d6a0"))+
  facet_wrap(~Ward)+
  labs(title = "Distribution of adult larvae(mosquito) by Blood Meal Status", x = "Specie", y = "Number of Adult Mosquito with Blood") +
  theme_manuscript1() +
  theme(legend.position = "right")

##Note: Add Funestus and Agugu for completeness




