rm(list=ls())
#memory.limit(size = 50000)
## -----------------------------------------
### Paths
## -----------------------------------------

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
  geopath <- file.path(NuDir,"data/nigeria/kano_ibadan")
  shapepath <- file.path(geopath,"kano_ibadan_shape_files")
  NuCDir <- file.path(Drive, "my_stuff")
  ProjectDir <- file.path(NuDir, "data", 'nigeria','nigeria_dhs' , 'data_analysis')
  EntoDat <- file.path(geopath, "kano_ibadan_ento", "Osun-excel")
  ResultDir <-file.path(NuDir, "projects/project_implementation/analysis_output/ento_plots")
  DataDir <- file.path(ProjectDir, 'data', 'DHS', 'Downloads')
  PreDir <- file.path(NuDir, "presentations/team member archive_Ifeoma/2023/230704_WHO/pictures")
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
#Read in files and plot sampling locations------------------------------------
##----------------------------------------------------------------------------
files <- list.files(path = EntoDat , pattern = ".xlsx", full.names = TRUE, recursive = F)
dat <- sapply(files, read_xlsx, simplify = F)
names(dat)

#locations 
cdc <- rbind(dat[[3]], dat[[4]], dat[[5]]) %>% select(State, Ward=`Ward Name`, Latitude, Longitude) %>%  mutate(name = "CDC light trap") %>%  distinct(Latitude, Longitude, .keep_all = TRUE)
psc <- rbind(dat[[6]], dat[[7]], dat[[8]]) %>% select(State, Ward, Latitude, Longitude)  %>%  mutate(name = "Pyrethrum Spray Catch") %>%  distinct(Latitude, Longitude, .keep_all = TRUE)
lar <- rbind(dat[[1]], dat[[2]]) %>%select(State, Ward = Locality, Latitude, Longitude)%>%  mutate(name = "Larval prospection")%>%  distinct(Latitude, Longitude, .keep_all = TRUE)

all_dat <- rbind(cdc, psc, lar)   

#shapefiles
df_ib = st_read(file.path(shapepath, "ibadan_metro_ward_fiveLGAs", "Ibadan_metro_fiveLGAs.shp")) %>%
  mutate(WardName = ifelse(WardName == 'Oranyan' & LGACode == '31007', 'Oranyan_7', WardName))
df_ko = st_read(file.path(shapepath, "Kano_metro_ward_sixLGAs", "Kano_metro_ward_sixLGAs.shp"))

st_crs(df_ko)<- 4326

df_sh <- list(df_ib, df_ko)
state <- c("Oyo", "Kano")

for (i in 1:length(df_sh)){

p<- ggplot(df_sh[[i]]) +
  geom_sf(fill = NA) +
  geom_point(data = dplyr::filter(all_dat, State==state[1]), aes(x = Longitude, y = Latitude, color = name), alpha=0.7, size = 2.5) +
  scale_color_manual(name = "", values = c("#e07a5f","#4f772d", "#ee9b00" )) + 
  labs(title= paste0("Wards in ", state[i], " showing household locations visited, Jan-March, 2023"))+
  theme_void()

ggsave(paste0(PreDir,"/", Sys.Date(), '_locations_ento_', state[i], '.pdf'), p, width = 8, height = 6)
}

Wards = c("Olopomewa", "Agugu", "Challenge", "Molete")

for (i in 1:length(Wards)){
  
p<- ggplot(data =dplyr::filter(df_sh[[1]], WardName == Wards[i])) +
  geom_sf(fill = NA) +
  geom_point(data = dplyr::filter(all_dat, State==state[1],  Ward ==Wards[i]), aes(x = Longitude, y = Latitude, color = name), alpha=0.7, size = 2.5) +
  scale_color_manual(name = "", values = c("#e07a5f","#4f772d", "#ee9b00" )) +
  labs(title=  Wards[i])+
  theme_void()
ggsave(paste0(PreDir,"/", Sys.Date(), '_locations_ento_', Wards[i], '.pdf'), p, width = 5, height = 4)
}



##----------------------------------------------------------------------------
#CDC--------------------------------------------------------------------
##----------------------------------------------------------------------------

#How many unique households were visited in Ibadan for CDC (cleaned data for error in household night)
#Jan
cdc_ib_jan1 <- rbind(dat[[3]], dat[[4]], dat[[5]])  %>%  dplyr::filter(State == "Oyo", Location == "Indoor", Month == "January", Night == 1) %>% distinct(Latitude, Longitude, .keep_all = TRUE)
cdc_ib_jan2 <- rbind(dat[[3]], dat[[4]], dat[[5]])  %>%  dplyr::filter(State == "Oyo", Location == "Indoor", Month == "January", Night == 2) %>% distinct(Latitude, Longitude, .keep_all = TRUE)
cdc_ib_jan3 <- rbind(dat[[3]], dat[[4]], dat[[5]])  %>%  dplyr::filter(State == "Oyo", Location == "Indoor", Month == "January", Night == 3) %>% distinct(Latitude, Longitude, .keep_all = TRUE)

#answer 3 households sampled in January
nrow(cdc_ib_jan1)
nrow(cdc_ib_jan2)
nrow(cdc_ib_jan3)

#Feb
cdc_ib_feb1 <- rbind(dat[[3]], dat[[4]], dat[[5]])  %>%  dplyr::filter(State == "Oyo", Location == "Indoor", Month == "February", Night == 1) %>% distinct(Latitude, Longitude, .keep_all = TRUE)
cdc_ib_feb2 <- rbind(dat[[3]], dat[[4]], dat[[5]])  %>%  dplyr::filter(State == "Oyo", Location == "Indoor", Month == "February", Night == 2) %>% distinct(Latitude, Longitude, .keep_all = TRUE)
cdc_ib_feb3 <- rbind(dat[[3]], dat[[4]], dat[[5]])  %>%  dplyr::filter(State == "Oyo", Location == "Indoor", Month == "February", Night == 3) %>% distinct(Latitude, Longitude, .keep_all = TRUE)

#answer 3 households sampled in February
nrow(cdc_ib_feb1)
nrow(cdc_ib_feb2)
nrow(cdc_ib_feb3)


#mar
cdc_ib_mar1 <- rbind(dat[[3]], dat[[4]], dat[[5]])  %>%  dplyr::filter(State == "Oyo", Location == "Indoor", Month == "March", `Time of Collection`=="10-11pm") %>% distinct(Latitude, Longitude, .keep_all = TRUE)
cdc_ib_mar2 <- rbind(dat[[3]], dat[[4]], dat[[5]])  %>%  dplyr::filter(State == "Oyo", Location == "Indoor", Month == "March", `Time of Collection`=="10-11pm") %>% distinct(Latitude, Longitude, .keep_all = TRUE)
cdc_ib_mar3 <- rbind(dat[[3]], dat[[4]], dat[[5]])  %>%  dplyr::filter(State == "Oyo", Location == "Indoor", Month == "March", `Time of Collection`=="10-11pm") %>% distinct(Latitude, Longitude, .keep_all = TRUE)

#answer 24 households sampled in March
nrow(cdc_ib_mar1)
nrow(cdc_ib_mar2)
nrow(cdc_ib_mar3)


# how many mosquitoes were caught and where
cdc <- rbind(dat[[3]], dat[[4]], dat[[5]]) 
sum(cdc$`Total Anopheles`) #17 mosquitoes

table(cdc$`Time of Collection`)



cdc$`Time of Collection`<- factor(cdc$`Time of Collection`, 
                                        levels = c("6-7pm","7-8pm", "8-9pm", "9-10pm", "10-11pm", "11-12am", "12-1am", "1-2am", "2-3am", "3-4am", "4-5am", "5-6am"))


p<- ggplot(cdc, aes(x=`Time of Collection`, y = `Total Anopheles`, fill = Location))+
  geom_point(size = 4,alpha = 0.7, shape=21, stroke =NA)+
  scale_fill_manual(name= "", values =c("#ff3374", "#4b7665")) +
  theme_manuscript()+
  labs(y = " Number of Anopheles Caught")+
  ylim(0, 3.2)+
  theme(legend.position = "bottom")

ggsave(paste0(PreDir,"/", Sys.Date(), '_indoor_outdoor_collection', '.pdf'), p, width = 8, height = 7)

cdc_in <- cdc%>%  dplyr::filter(Location =="Indoor") %>%  summarise(TA = sum(`Total Anopheles`))
cdc_out <- cdc%>%  dplyr::filter(Location =="Outdoor") %>%  summarise(TA = sum(`Total Anopheles`))

p_cdc<- ggplot(cdc, aes(x=`Time of Collection`, y = `Total Anopheles`, fill=`Settlement Classification`))+
  geom_point(size = 3,alpha = 0.7, shape=21, stroke =NA)+
  scale_fill_manual(name= "", values =c("#662d91", "#f1a6a2", "#93325a")) +
  theme_manuscript()+
  labs(y = " Number of Anopheles Caught\n using the CDC Light Trap")+
  ylim(0, 3.2)+
  theme(legend.position = "bottom")+
  ylim(0, 5.2)


ggsave(paste0(PreDir,"/", Sys.Date(), '_settlement_type_collection', '.pdf'), p_cdc, width = 8, height = 7)

#total by settlement 
cdc_for <- cdc%>%  dplyr::filter(`Settlement Classification` == "Formal") %>%  summarise(TA = sum(`Total Anopheles`))
cdc_for
cdc_inf <- cdc%>%  dplyr::filter(`Settlement Classification` =="Informal") %>%  summarise(TA = sum(`Total Anopheles`))
cdc_inf
cdc_sl <- cdc%>%  dplyr::filter(`Settlement Classification` =="Slum") %>%  summarise(TA = sum(`Total Anopheles`))
cdc_sl

#what month did we have the most collections?
cdc$Month<- factor(cdc$Month, levels = c("January", "February", "March"))
table(cdc$`Type of Anopheles_1`)
cdc <- cdc %>%  mutate(Anopheles_code = case_when(`Type of Anopheles_1` == "An. gambiense" ~ "An. gambiae",
                                                  `Type of Anopheles_1` == "Gambiens" ~ "An. gambiae",
                                                   .default = as.character(`Type of Anopheles_1`)))
pdat <- cdc %>% group_by(Month,Anopheles_code) %>%  summarise(num_mos = sum(`Total Anopheles`))
ggplot(pdat, aes(x =Month, y= num_mos, fill = Anopheles_code))+
  geom_bar(stat = "identity", position = "stack")+
  scale_fill_manual(name = "", values = c("#e366f0", "#8840c8", "gray"), label = c("An. funestus", "An gambiae", "No data")) + 
  labs(x = "Month of Collection", y="Number of Anopheles caught using \n the CDC light trap")+
  theme_manuscript()+
  ylim(0, 13)
ggsave(paste0(PreDir,"/", Sys.Date(), '_collection_times_specie', '.pdf'), width = 8, height = 7)



##----------------------------------------------------------------------------
#PSC--------------------------------------------------------------------
##----------------------------------------------------------------------------



#How many unique households were visited in Ibadan for CDC (cleaned data for error in household night)
#Jan
psc_ib_jan1 <- rbind(dat[[6]], dat[[7]], dat[[8]]) %>%  dplyr::filter(State == "Oyo",Month == "January") %>% distinct(Latitude, Longitude, .keep_all = TRUE)
psc_ib_jan2 <- rbind(dat[[6]], dat[[7]], dat[[8]]) %>%  dplyr::filter(State == "Oyo", Month == "February") %>% distinct(Latitude, Longitude, .keep_all = TRUE)
psc_ib_jan3 <- rbind(dat[[6]], dat[[7]], dat[[8]])  %>%  dplyr::filter(State == "Oyo", Month == "March") %>% distinct(Latitude, Longitude, .keep_all = TRUE)

nrow(psc_ib_jan1)
nrow(psc_ib_jan2)
nrow(psc_ib_jan3)

# how many mosquitoes were caught and where
psc <- rbind(dat[[6]], dat[[7]], dat[[8]])
sum(psc$`An. Gambiae`, na.rm = T) #17 mosquitoes
sum(psc$An.Funestus, na.rm = T)


#what month did we have the most collections?
psc$Month<- factor(psc$Month, levels = c("January", "February", "March"))
pdat <- psc %>% group_by(Month) %>%  summarise(num_mos = sum(`An. Gambiae`, na.rm = T))
ggplot(pdat, aes(x =Month, y= num_mos))+
  geom_bar(stat = "identity", fill="#8840c8")+
  labs(x = "Month of Collection", y="Number of Anopheles gambiae caught using \n Pyrethrum Spray Catch")+
  theme_manuscript()+
  ylim(0, 13)


ggsave(paste0(PreDir,"/", Sys.Date(), '_collection_times_PSC', '.pdf'), width = 8, height = 7)

#What settlement did we catch the most mosquitoes 
pdat <- psc %>% group_by(`Settlement Classification`) %>%  summarise(num_mos = sum(`An. Gambiae`, na.rm = T))
p_spray<- ggplot(pdat, aes(x=`Settlement Classification`, y = num_mos, fill = `Settlement Classification`))+
  geom_bar(stat = "identity")+
  scale_fill_manual(name= "", values =c("#662d91", "#f1a6a2", "#93325a"))+
  labs(x = "Settlement Classification", y="Number of Anopheles gambiae caught using \n Pyrethrum Spray Catch")+
  theme_manuscript()+
  ylim(0, 5.2)+
  theme(legend.position = "bottom")

all<- p_cdc + p_spray
ggsave(paste0(PreDir,"/", Sys.Date(), '_settlement_type_collection_PSC_cdc', '.pdf'), all, width = 8, height = 7.5)


#Larval habitat sampling 
lar <- rbind(dat[[1]], dat[[2]])
lar$Month <- factor(lar$Month, levels = c("January", "February", "March"))
pdat <- lar %>%  group_by( Month, `Breeding site`) %>%  summarise(num_breed = n()) %>%  ungroup() %>% 
  group_by(Month) %>%  mutate(percent = round(num_breed/sum(num_breed) *100,0 ))


p1 <- ggplot(pdat, aes(x =Month, y=num_breed , fill = `Breeding site`))+
  geom_bar(stat = "identity", position = "stack")+
  scale_fill_manual(name = "", values = RColorBrewer::brewer.pal(10,  "RdYlBu")) +
  geom_text(aes(label = paste0(percent, "%")),
            position = position_stack(vjust=0.5),
            color = "black")+
  labs(x = "Month of Collection", y="Number of Breeding sites")+
  theme_manuscript()+
  ylim(0, 130)

pdat <- lar %>%  group_by(`Settlement Type`, `Breeding site`) %>%  summarise(num_breed = n()) %>%  ungroup() %>% 
  group_by(`Settlement Type`) %>%  mutate(percent = round(num_breed/sum(num_breed) *100,0 ))

p2<- ggplot(pdat, aes(x =`Settlement Type`, y=num_breed , fill = `Breeding site`))+
  geom_bar(stat = "identity", position = "stack")+
  scale_fill_manual(name = "", values = RColorBrewer::brewer.pal(10,  "RdYlBu")) +
  geom_text(aes(label = paste0(percent, "%")),
            position = position_stack(vjust=0.5),
            color = "black")+
  labs(x = "Settlement Classification", y="Number of Breeding sites")+
  theme_manuscript()+
  ylim(0, 130)

all<-p1 + p2 + plot_layout(guides = "collect") & theme(legend.position = 'bottom')

ggsave(paste0(PreDir,"/", Sys.Date(), '_settlement_type_month_larval', '.pdf'), all, width = 8, height = 7)

pdat <- lar %>%  mutate(WN = case_when(`Water nature` == "clean" ~ "Clean",
                                       `Water nature` == "polluted" ~ "Polluted",
                                       .default = as.character(`Water nature`))) %>%
  dplyr::filter(`No of dips` !=0) %>% 
  group_by(WN) %>%  summarise(num_polluted = n())  

#What habitats did we find anopheles 
pdat <- lar %>%  dplyr::filter(Anopheles !=0) %>%  group_by(`Settlement Type`, `Breeding site`) %>%  summarise(num_ano = sum(Anopheles))

p1<- ggplot(pdat, aes(x =`Settlement Type`, y=num_ano , fill = `Breeding site`))+
  geom_bar(stat = "identity", position = "stack")+
  scale_fill_manual(name= "", values =c("#FFB3B3", "#FFDBA4"))+
  labs(x = "Settlement Classification", y="Number of Anopheles larva prospected")+
  theme_manuscript()+
  theme(legend.position = "bottom")


names(lar)
pdat <- lar %>%  dplyr::filter(Anopheles !=0)%>% mutate(location = paste0(Latitude, Longitude)) %>%  
  group_by(`location`, `Breeding site`) %>%  summarise(num_ano = sum(Anopheles))

pdat <- lar %>% mutate(location = paste0(Latitude, Longitude)) %>%  
  group_by(Anopheles) %>%  summarise(mean_temp = mean(Temp, na.rm = T))
#what is the distance between larval prospection site?

p1<- ggplot(pdat, aes(x =`Settlement Type`, y=num_ano , fill = `Breeding site`))+
  geom_bar(stat = "identity", position = "stack")+
  scale_fill_manual(name= "", values =c("#FFB3B3", "#FFDBA4"))+
  labs(x = "Settlement Classification", y="Number of Anopheles larva prospected")+
  theme_manuscript()+
  theme(legend.position = "bottom")


#clear or polluted in settlement 
pdat <- lar %>%  dplyr::filter(Anopheles !=0) %>%  mutate(WN = case_when(`Water nature` == "clean" ~ "Clean",
                                                                                 `Water nature` == "polluted" ~ "Polluted",
                                                                                 .default = as.character(`Water nature`))) %>%
  group_by(`Settlement Type`, `WN`) %>%  summarise(num_ano = sum(Anopheles))

p2<- ggplot(pdat, aes(x =`Settlement Type`, y=num_ano , fill = `WN`))+
  geom_bar(stat = "identity", position = "stack")+
  scale_fill_manual(name= "", values =c("#8FE3CF", "#002B5B"))+
  labs(x = "Settlement Classification", y="Number of Anopheles larva prospected")+
  theme_manuscript()+
  theme(legend.position = "bottom")


p1 + p2
ggsave(paste0(PreDir,"/", Sys.Date(), '_settlement_type_Anopheles_caught', '.pdf'),  width = 8, height = 7)

pdat <- lar %>%  dplyr::filter(Anopheles !=0) %>%  group_by(`Settlement Type`) %>%  summarise(num_ano = sum(Anopheles),
                                                                                              total_dips = sum(`No of dips`),
                                                                                              num_dip = num_ano/total_dips)


ggplot(pdat, aes(x =`Settlement Type`, y=num_dip))+
  geom_bar(stat = "identity", position = "stack",  fill = "#FF7777")+
  labs(x = "Settlement Classification", y="Number of Anopheles larva caught per dip")+
  theme_manuscript()+
  theme(legend.position = "bottom")
ggsave(paste0(PreDir,"/", Sys.Date(), '_settlement_type_number_dip', '.pdf'),  width = 6, height = 5)

pdat <- lar %>%  group_by(Month, Anopheles_Caught, `Breeding site`) %>%  summarise(num_anopheles = n()) %>%  dplyr::filter(Anopheles_Caught == "Yes")

pdat <- lar %>%  group_by(`Settlement Type`, Anopheles_Caught, `Breeding site`) %>%  summarise(num_anopheles = n()) %>%  dplyr::filter(Anopheles_Caught == "Yes")






















## read ibadan ward shape files
df_ib = st_read(file.path(shapepath, "ibadan_metro_ward_fiveLGAs", "Ibadan_metro_fiveLGAs.shp")) %>%
  mutate(WardName = ifelse(WardName == 'Oranyan' & LGACode == '31007', 'Oranyan_7', WardName))

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



p <- ggplot(df_ib) +
  geom_sf(fill = NA) +
  #geom_sf_text(data = df, aes(label = WardName), colour = "black")+
  geom_point(data = dplyr::filter(cdc, State=="Oyo"), mapping = aes(x = Longitude, y = Latitude), colour = "red", size = 2.5) +
  geom_text_repel(
    data = df_ib,
    aes(label =  WardName, geometry = geometry),color ='black',
    stat = "sf_coordinates", min.segment.length = 0, size = 3.5, force = 1)+
  map_theme()+ 
  labs(title= "Wards in Ibadan showing households visited for indoor \n and outdoor mosquito collection using CDC light trap, Jan-March, 2023")+
  coord_sf()

ggsave(paste0(ResultDir,"/", Sys.Date(), "/", 'locations_cdc_ibadan3.png'), p, width = 8, height = 6)



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

#table(psc_ko$Day, psc_ko$Month)

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
lav_ib <- lav %>% dplyr::filter(State == "Oyo")%>%
  group_by(`Settlement Type`, Month) %>% 
  summarise(total_mosquitoes = sum(`Anopheles`, na.rm = T)) %>% ungroup()

#table(lav$State, lav$Breeding.site)

p <- ggplot(data=lav_ib, aes(x= Month, y=total_mosquitoes, group = `Settlement Type`,
                             colour = `Settlement Type`))+
  scale_x_discrete(limits=c("March")) +
  geom_point() +labs(y= "Total number of anopheles mosquitos caught", x = "Month of Collection (Ibadan)")+
  geom_line()+
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


# #locations data collection
# p <- ggplot(dfk) +
#   geom_sf(fill = NA) +
#   #geom_sf_text(data = df, aes(label = WardName), colour = "black")+
#   geom_point(data = filter(larv, State=="Kano"), mapping = aes(x = Longitude, y = Latitude), colour = "green", size = 3.5) +
#   geom_text_repel(
#     data = dfk,
#     aes(label =  WardName, geometry = geometry),color ='black',
#     stat = "sf_coordinates", min.segment.length = 0, size = 3.5, force = 1)+
#   map_theme()+
#   labs(title= "Wards in Kano showing sites visited for larva prospection, Jan-Feb, 2023")+
#   coord_sf()

##Combined Plots
names(cdc)
cdc$Method <- ("CDC")
cdc_p <- cdc %>% dplyr::select(`State`, `Month`, `Settlement Classification`, 
                               `Latitude`, `Longitude`, `Method`)

cdc_p1 <- unique(cdc_p$Latitude, cdc_p$Longitude)

names(psc)
psc$Method <- ("PSC")
psc_p <- psc %>% dplyr::select(`State`, `Month`, `Settlement Classification`, `Latitude`, `Longitude`, `Method`)

psc_p1 <- unique(psc_p$Latitude, psc_p$Longitude)

names(lav)
colnames(lav)[colnames(lav) == "Settlement.Type"] <- "Settlement Classification"
lav$Method <- ("LARVA PROSPECTION")
lav_p <- lav %>% dplyr::select(`State`, `Month`, `Settlement Classification`, `Latitude`, `Longitude`, `Method`)

lav_p1 <- unique(lav_p$Latitude, lav_p$Longitude)

all_df <- rbind(cdc_p, psc_p, lav_p)

ggplot(df_ib) +
  geom_sf(fill = NA) +
  #geom_sf_text(data = df, aes(label = WardName), colour = "black")+
  geom_point(data = dplyr::filter(all_df, State=="Oyo"), mapping = aes(x = Longitude, y = Latitude, fill = "Method",), size = 3.5) +
  geom_text_repel(
    data = df_ib,
    aes(label =  WardName, geometry = geometry),color ='black',
    stat = "sf_coordinates", min.segment.length = 0, size = 3.5, force = 1)+
  map_theme()+
  labs(title= "Wards in Kano showing sites visited for larva prospection, Jan-Feb, 2023")+
  coord_sf()



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
  theme_manuscript()
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
  theme_manuscript()

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
    theme_manuscript()+
theme(strip.background = element_rect(fill = "yellow", color = "black"))


##Calculations
view(lav)

subset_lav <- lav[lav$Anopheles > 0, ]

lav_dips_sb <- subset_lav %>%  dplyr::filter(State == "Oyo") %>%  group_by(`Settlement Classification`, `Breeding.site`) %>% 
  summarise(total_dips = sum(`No of dips`)) %>% ungroup()

lav_dips_s <- subset_lav %>%  dplyr::filter(State == "Oyo") %>%  group_by(`Settlement Classification`) %>% 
  summarise(total_dips = sum(`No of dips`)) %>% ungroup()

Ano_catch_s <- subset_lav %>%  dplyr::filter(State == "Oyo") %>%  group_by(`Settlement Classification`) %>% 
  summarise(Anopheles_caught = sum(`Anopheles`)) %>% ungroup()

Ano_catch_sb <- subset_lav %>%  dplyr::filter(State == "Oyo") %>%  group_by(`Settlement Classification`, `Breeding.site`) %>% 
  summarise(Anopheles_caught = sum(`Anopheles`)) %>% ungroup()



divide_columns <- function(Ano_catch_sb, Anopheles_caught, lav_dips_sb, total_dips) {
  result <- Ano_catch_sb[[Anopheles_caught]] / lav_dips_sb[[total_dips]]
  return(result)
}

result <- divide_columns(Ano_catch_sb, "Anopheles_caught", lav_dips_sb, "total_dips")


##Larval Density
sett <- c("Formal", "Informal", "Slum", "Formal", "Informal", "Slum")
meth <- c("Drainage", "Drainage", "Drainage", "Tyre Tracks", "Tyre Tracks", "Tyre Tracks")
ld <- c(1.93, 0, 7.6, 0, 0, 2.7)
freq <- c(3, 0, 2, 0, 0, 1)

ld_df <- data.frame(sett, meth, ld, freq)

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


ggplot(ld_df, aes(x = sett, y = freq, fill = sett)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ meth) +
  geom_line(aes(y = ld * max(freq) / max(ld), group = 1), color = "blue") +
  scale_y_continuous(
    name = "Frequency",
    sec.axis = sec_axis(~ . * max(ld) / max(freq), name = "LD")
  ) +
  theme_minimal() +
  labs(x = "Settlement Type", title = "Settlement Type Frequency and LD by Method") +
  theme(legend.position = "none")


##HBR
view(cdc)

subset_cdc <- cdc[cdc$`Total Anopheles` > 0, ]

Ano_caught_cdc <- subset_cdc %>%  dplyr::filter(State == "Oyo") %>%  group_by(`Settlement Classification`, `Location`) %>% 
  summarise(Anopheles_caught = sum(`Total Anopheles`)) %>% ungroup()

Ano_caught_cdc$No_night_bate <- 14 ##6 days in Jan and Feb while 8 days in March. Only 1 person slept as bait

divide_columns <- function(Ano_caught_cdc, Anopheles_caught, No_night_bate, result_col) {
  Ano_caught_cdc[[result_col]] <- Ano_caught_cdc[[Anopheles_caught]] / Ano_caught_cdc[[No_night_bate]]
  return(Ano_caught_cdc)
}

hbr <- divide_columns(Ano_caught_cdc, "Anopheles_caught", "No_night_bate", "HBR")

hbr_indoor <- hbr %>% dplyr::filter (Location == "Indoor")

p <- ggplot(data=hbr_indoor, aes(x= `Settlement Classification`, y=HBR, group = `Settlement Classification`,
                          colour = `Settlement Classification`))+
  scale_x_discrete(limits=c("Formal", "Informal", "Slum")) +
  geom_point() +labs(y= "Human Biting Rate", x = "Settlement Type")+
  geom_line()+
  ggtitle("Indoor Human Biting Rate by settlement type, 2023")+geom_point(size = 3.0) +
  theme(plot.title = element_text(size = 12))+
  theme_manuscript() +
  theme(
    legend.position = c(.95, .95),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6)
  )

hbr_outdoor <- hbr %>% dplyr::filter (Location == "Outdoor")

p <- ggplot(data=hbr_outdoor, aes(x= `Settlement Classification`, y=HBR, group = `Settlement Classification`,
                                 colour = `Settlement Classification`))+
  scale_x_discrete(limits=c("Formal", "Informal", "Slum")) +
  geom_point() +labs(y= "Human Biting Rate", x = "Settlement Type")+
  geom_line()+
  ggtitle("Outdoor Human Biting Rate by settlement type, 2023")+geom_point(size = 3.0) +
  theme(plot.title = element_text(size = 12))+
  theme_manuscript() +
  theme(
    legend.position = c(.95, .95),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6)
  )

##IRD
view(psc)

subset_psc <- psc[psc$`An. Gambiae` > 0, ]

Ano_caught_psc <- subset_psc %>%  dplyr::filter(State == "Oyo") %>%  group_by(`Settlement Classification`) %>% 
  summarise(Anopheles_caught = sum(`An. Gambiae`)) %>% ungroup()

Ano_caught_psc$No_rooms <- 40 ##6 days in Jan and Feb while 8 days in March. Only 1 person slept as bait

divide_columns <- function(Ano_caught_psc, Anopheles_caught, No_rooms, result_col) {
  Ano_caught_psc[[result_col]] <- Ano_caught_psc[[Anopheles_caught]] / Ano_caught_psc[[No_rooms]]
  return(Ano_caught_psc)
}

ird <- divide_columns(Ano_caught_psc, "Anopheles_caught", "No_rooms", "IRD")

p <- ggplot(data=ird, aes(x= `Settlement Classification`, y=IRD, group = `Settlement Classification`,
                             colour = `Settlement Classification`))+
  scale_x_discrete(limits=c("Formal", "Informal", "Slum")) +
  geom_point() +labs(y= "Indoor Residual Density", x = "Settlement Type")+
  geom_line()+
  ggtitle("Indoor Residual Density by settlement type, 2023")+geom_point(size = 3.0) +
  theme(plot.title = element_text(size = 12))+
  theme_manuscript() +
  theme(
    legend.position = c(.95, .95),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6)
  ) 

