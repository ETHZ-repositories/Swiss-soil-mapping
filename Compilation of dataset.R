##NOBODATA
library(tidyr)
library(plyr)
library(rgdal)
library(dplyr)
library(raster)
library(tidyverse)
library(rgeos)
library(googledrive)
library(cowplot)   # for theme_minimal_grid()
library(sf)        # for manipulation of simple features objects
library(rworldmap) # for getMap()
library(ggplot2)
library(uuid)
library(reshape2)
library(stringr)
library(data.table)
library(readxl)
nobdoda<-readxl::read_excel("D:/Swiss_soil_data/CH_SoilDataset_EN/NatBodDS_final_EN.xlsx", sheet = "4_Surv_Prof_Horiz",guess_max = 10000)
colnames(nobdoda)
measu_data<- readxl::read_excel("D:/Swiss_soil_data/CH_SoilDataset_EN/NatBodDS_final_EN.xlsx", sheet = "7_Surv_Sample_Measur",guess_max = 10000)
measur_ST<- nobdoda[!is.na(nobdoda$SAND_pct),]
measur_ST1<- measur_ST[!is.na(measur_ST$CLAY_pct),]
measu_data1_sand <- measu_data[grep("Sand", measu_data$ANALYTICAL_PARAMETER), ]
measu_data1_silt <- measu_data[grep("Grobschluff", measu_data$ANALYTICAL_PARAMETER), ]
measu_data1_P <- measu_data[grep("P-AAE", measu_data$ANALYTICAL_PARAMETER), ]
measu_data1_Corg <- measu_data[grep("Corg", measu_data$ANALYTICAL_PARAMETER),]
measu_data1_Nit <- measu_data[grep("N", measu_data$ANALYTICAL_PARAMETER),]
measu_data1_Nit<- measu_data1_Nit[grep("mg/kg", measu_data1_Nit$UNIT),]
measu_data1_Nit<- measu_data1_Nit[measu_data1_Nit$ANALYTICAL_PARAMETER== "N",]
library(sp)
#test <- read.csv("test.points.csv")
# We create a Spatial points data frame for our data, where X and Y are the fields with coordinates
coordinates(measu_data1_Nit) <- ~ COORDINATE_X_E + COORDINATE_Y_N
# We tell R our data it's on LV95, EPSG=2056
measu_data1_Nit@proj4string <- CRS("+init=epsg:2056")
# We can check the original coords
measu_data1_Nit@coords
# We can plot the data to check
plot(measu_data1_Nit)
# Then we transform the data to WGS84
test2 <- spTransform(measu_data1_Nit, CRS("+init=epsg:4326"))
# We can see the transformed coords
test2@data
jjj<- data.frame(test2)
### WOSIS dataset
#WOSIS-------------------
Table11<- read.table("D:/Downloads_Surya/WoSIS_2019_September/wosis_201909_attributes.tsv", sep = '\t', header = TRUE)
Table12<- read.table("D:/Downloads_Surya/WoSIS_2019_September/wosis_201909_layers_chemical.tsv", sep = '\t', header = TRUE,fill = TRUE)
Table13<- read.table("D:/Downloads_Surya/WoSIS_2019_September/wosis_201909_layers_physical.tsv", sep = '\t', header = TRUE,fill = TRUE,na.strings= c("999", "NA", " ", ""))
Table14<- read.table("D:/Downloads_Surya/WoSIS_2019_September/wosis_201909_profiles.tsv", sep = '\t', header = TRUE,fill = TRUE)
Table14$location_accuracy_id<- Table14$geom_accuracy*10000000
Table14$location_accuracy_min<- 0
Table14$location_accuracy_max<- (Table14$location_accuracy_id)/100
chemical<- Table12[, c("profile_layer_id","orgc_value_avg" )]
colnames(Table12)
colnames(Table14)
pp<-  merge(Table13, Table14, by="profile_id")
WRC_table2<-pp[,c("profile_id","upper_depth","profile_layer_id", "upper_depth", "latitude", "longitude",
                  "lower_depth", "bdfi33_value_avg","bdfiod_value_avg","bdws33_value_avg", "clay_value_avg",
                  "sand_value_avg", "silt_value_avg", "wv0100_value_avg", "wv0010_value_avg",
                  "wv1500_value_avg", "wv0200_value_avg","wv0500_value_avg", "wv0006_value_avg","wv0033_value_avg",
                  "country_id", "country_name", "wg0100_value_avg", "wg0010_value_avg", "wg1500_value_avg"
                  ,"wg0200_value_avg", "wg0033_value_avg"
                  , "wg0500_value_avg", "wg0006_value_avg","location_accuracy_min","location_accuracy_max")]
Swiz_samples<- WRC_table2[WRC_table2$country_name=="Switzerland",]
Swiz_samples<- Swiz_samples[!is.na(Swiz_samples$clay_value_avg),] 
#write.csv(Swiz_samples,"C:/Users/guptasu.D/Documents/Swiss_soil_data/ST_Wosis_data.csv")
## only soil texture data is available for WOSIS
############ Swiz_samples
##SWISS dataset
Swiss_data<-readxl::read_excel("D:/Downloads_Surya/WRC_dataset_2021.xlsx", sheet = "Swiss_database",guess_max = 10000)
colnames(Swiss_data)
#write.csv(Swiss_data,"D:/Users/guptasu.D/Documents/Swiss_soil_data/ST_swiss_data.csv")
#Swiss_data1<-read.csv("D:/Users/guptasu.D/Documents/Swiss_soil_data/ST_swiss_data.csv")
## only soil texture data is avaialble for SWISS dataset
#SPADE----------
spade.PLOT <- read.csv("D:/Downloads_Surya/SPADE/DAT_PLOT.csv")
str(spade.PLOT)
table(spade.PLOT$CNTY_C)
spade.HOR <- read.csv("D:/Downloads_Surya/SPADE/DAT_HOR.csv")
colnames(spade.HOR)
str(spade.HOR)
class(spade.HOR$SILT2_V)
head(spade.HOR)
spade.PLOT = spade.PLOT[!spade.PLOT$LON_COOR_V>180 & spade.PLOT$LAT_COOR_V>20,]
plot(spade.PLOT[,c("LON_COOR_V","LAT_COOR_V")])
spade.PLOT$location_accuracy_min = 100
spade.PLOT$location_accuracy_max = NA
#site.names = c("site_key", "usiteid", "site_obsdate", "longitude_decimal_degrees", "latitude_decimal_degrees")
spade.PLOT$ProfileID = paste(spade.PLOT$CNTY_C, spade.PLOT$PLOT_ID, sep="_")
spade.PLOT$T_Year = 2009
spade.s.lst <- c("PLOT_ID", "ProfileID", "T_Year", "LON_COOR_V", "LAT_COOR_V", "location_accuracy_min", "location_accuracy_max")
## standardize:
spade.HOR$SLTPPT <- rowSums(spade.HOR[,c("SILT1_V", "SILT2_V")], na.rm=TRUE)
spade.HOR$SNDPPT <- rowSums(spade.HOR[,c("SAND1_V", "SAND2_V","SAND3_V")], na.rm=TRUE)
spade.HOR$PHIKCL <- NA
spade.HOR$PHIKCL[which(spade.HOR$PH_M %in% "A14")] <- spade.HOR$PH_V[which(spade.HOR$PH_M %in% "A14")]
spade.HOR$PHIHO5 <- NA
spade.HOR$PHIHO5[which(spade.HOR$PH_M %in% "A12")] <- spade.HOR$PH_V[which(spade.HOR$PH_M %in% "A12")]
#summary(spade.HOR$BD_V)
#for(j in c("site_obsdate", "layer_sequence", "db_13b", "COLEws", "w15bfm", "w6clod", "w10cld", "adod", "wrd_ws13", "w15bfm", "cec7_cly", "w15cly", "tex_psda", "cec_nh4", "ksat_lab", "ksat_field")){  spade.HOR[,j] = NA }
#spade.h.lst = c("HOR_ID","PLOT_ID","layer_sequence","HOR_BEG_V","HOR_END_V","HOR_NAME","db_13b", "BD_V", "COLEws", "w6clod", "w10cld", "WCFC_V", "WC4_V", "w15bfm", "adod", "wrd_ws13", "cec7_cly", "w15cly", "tex_psda", "CLAY_V", "SLTPPT", "SNDPPT", "OC_V", "PHIKCL", "PHIHO5", "CEC_V", "cec_nh4", "GRAV_C", "ksat_lab", "ksat_field")
colnames(spade.HOR)
SPADE.PLOT1<- spade.PLOT[,c( 1:6)]
SPADE_WRC<- merge(SPADE.PLOT1,spade.HOR, by = "PLOT_ID")
SPADE.sel.layer <- SPADE_WRC[,c("PLOT_ID","CNTY_C", "LON_COOR_V", "LAT_COOR_V", "HOR_NAME", "HOR_BEG_V", "HOR_END_V","CLAY_V",
                                "SLTPPT", "SNDPPT", "OC_V","OC_M", "PHIHO5", "WC1_V", "WC1_KPA", "WC2_V", "WC2_KPA", "WC3_V",
                                "WC3_KPA", "WC4_V", "WC4_KPA", "POR_V","BD_V" )]
SPADE.sel.layer_texture<- SPADE.sel.layer[!is.na(SPADE.sel.layer$CLAY_V),]
SPADE.sel.layer_carbon<- SPADE.sel.layer[!is.na(SPADE.sel.layer$OC_V),]
Swiss_data2<- SPADE.sel.layer_carbon[SPADE.sel.layer_carbon$CNTY_C== "CH",]
# write.csv(SPADE.sel.layer_texture,"C:/Users/guptasu.D/Documents/Swiss_soil_data/ST_Spade_data.csv")
# write.csv(SPADE.sel.layer_carbon,"C:/Users/guptasu.D/Documents/Swiss_soil_data/OC_Spade_data.csv")
## LUCAS Swiss_data
LUCAS_data<-readxl::read_excel("D:/Downloads_Surya/LUCAS2015_topsoildata_20200323/Swiss_data_LUCAS.xlsx", sheet = "Data",guess_max = 10000)
colnames(LUCAS_data)
#write.csv(LUCAS_data, "D:/Swiss_soil_data/OC_ST_P_Lucas_data.csv")
##### call the soil texture data
Nobodata_ST<- read.csv("D:/Swiss_soil_data/land_cover_ponits/SWISS_data11_nobodata/measur_ST2.csv")
WOSIS_ST<- read.csv("D:/Swiss_soil_data/land_cover_ponits/SWISS_data11_nobodata/ST_Wosis_data.csv")
SPADE_ST<- read.csv("D:/Swiss_soil_data/land_cover_ponits/SWISS_data11_nobodata/ST_Spade_data.csv")
SWISS_ST<- read.csv("D:/Swiss_soil_data/land_cover_ponits/SWISS_data11_nobodata/ST_swiss_data1.csv")
LUCAS_ST<- read.csv("D:/Swiss_soil_data/OC_ST_P_Lucas_data.csv")
### prepare the soil texture columns
Nobodata_ST<- Nobodata_ST[,c("ID", "DEPTH_FROM_cm", "DEPTH_TO_cm","CLAY_pct","SILT_pct","SAND_pct","COORDINATE_X_E","COORDINATE_Y_N")]
colnames(Nobodata_ST)[which(colnames(Nobodata_ST)%in% c("ID"))]<- "profile_id"
colnames(Nobodata_ST)[which(colnames(Nobodata_ST)%in% c("DEPTH_FROM_cm"))]<- "top"
colnames(Nobodata_ST)[which(colnames(Nobodata_ST)%in% c("DEPTH_TO_cm"))]<- "bottom"
colnames(Nobodata_ST)[which(colnames(Nobodata_ST)%in% c("CLAY_pct"))]<- "clay"
colnames(Nobodata_ST)[which(colnames(Nobodata_ST)%in% c("SILT_pct"))]<- "silt"
colnames(Nobodata_ST)[which(colnames(Nobodata_ST)%in% c("SAND_pct"))]<- "sand"
colnames(Nobodata_ST)[which(colnames(Nobodata_ST)%in% c("COORDINATE_X_E"))]<- "Long"
colnames(Nobodata_ST)[which(colnames(Nobodata_ST)%in% c("COORDINATE_Y_N"))]<- "Lat"
Nobodata_ST$source<- "Nobodata"
colnames(Nobodata_ST)
### Wosis_ST
colnames(WOSIS_ST)
WOSIS_ST<- WOSIS_ST[,c("profile_id", "upper_depth", "lower_depth","clay_value_avg","silt_value_avg","sand_value_avg", "longitude", "latitude" )]
colnames(WOSIS_ST)[which(colnames(WOSIS_ST)%in% c("upper_depth"))]<- "top"
colnames(WOSIS_ST)[which(colnames(WOSIS_ST)%in% c("lower_depth"))]<- "bottom"
colnames(WOSIS_ST)[which(colnames(WOSIS_ST)%in% c("clay_value_avg"))]<- "clay"
colnames(WOSIS_ST)[which(colnames(WOSIS_ST)%in% c("silt_value_avg"))]<- "silt"
colnames(WOSIS_ST)[which(colnames(WOSIS_ST)%in% c("sand_value_avg"))]<- "sand"
colnames(WOSIS_ST)[which(colnames(WOSIS_ST)%in% c("longitude"))]<- "Long"
colnames(WOSIS_ST)[which(colnames(WOSIS_ST)%in% c("latitude"))]<- "Lat"
WOSIS_ST$source<- "WOSIS"
colnames(WOSIS_ST)
### SPADE_ST
colnames(SPADE_ST)
SPADE_ST<- SPADE_ST[,c("PLOT_ID", "HOR_BEG_V", "HOR_END_V","CLAY_V","SLTPPT","SNDPPT","LON_COOR_V","LAT_COOR_V")]
colnames(SPADE_ST)[which(colnames(SPADE_ST)%in% c("PLOT_ID"))]<- "profile_id"
colnames(SPADE_ST)[which(colnames(SPADE_ST)%in% c("HOR_BEG_V"))]<- "top"
colnames(SPADE_ST)[which(colnames(SPADE_ST)%in% c("HOR_END_V"))]<- "bottom"
colnames(SPADE_ST)[which(colnames(SPADE_ST)%in% c("CLAY_V"))]<- "clay"
colnames(SPADE_ST)[which(colnames(SPADE_ST)%in% c("SLTPPT"))]<- "silt"
colnames(SPADE_ST)[which(colnames(SPADE_ST)%in% c("SNDPPT"))]<- "sand"
colnames(SPADE_ST)[which(colnames(SPADE_ST)%in% c("LON_COOR_V"))]<- "Long"
colnames(SPADE_ST)[which(colnames(SPADE_ST)%in% c("LAT_COOR_V"))]<- "Lat"
colnames(SPADE_ST)
SPADE_ST$source<- "SPADE"
### SWISS_ST
colnames(SWISS_ST)
SWISS_ST<- SWISS_ST %>%
  mutate(layer_id = layer_id) %>% 
  group_by(layer_id) %>%
  arrange(desc(layer_id)) %>%
  summarise_all(funs(na.omit(.)[1]))
SWISS_ST<- SWISS_ST[,c("profile_id","hzn_top", "hzn_bot", "clay_tot_psa_percent","silt_tot_psa_percent","sand_tot_psa_percent","longitude_decimal_degrees","latitude_decimal_degrees")]
colnames(SWISS_ST)[which(colnames(SWISS_ST)%in% c("hzn_top"))]<- "top"
colnames(SWISS_ST)[which(colnames(SWISS_ST)%in% c("hzn_bot"))]<- "bottom"
colnames(SWISS_ST)[which(colnames(SWISS_ST)%in% c("clay_tot_psa_percent"))]<- "clay"
colnames(SWISS_ST)[which(colnames(SWISS_ST)%in% c("silt_tot_psa_percent"))]<- "silt"
colnames(SWISS_ST)[which(colnames(SWISS_ST)%in% c("sand_tot_psa_percent"))]<- "sand"
colnames(SWISS_ST)[which(colnames(SWISS_ST)%in% c("longitude_decimal_degrees"))]<- "Long"
colnames(SWISS_ST)[which(colnames(SWISS_ST)%in% c("latitude_decimal_degrees"))]<- "Lat"
colnames(SWISS_ST)
SWISS_ST$source<- "SWISS"
### LUCAS_ST
colnames(LUCAS_ST)
LUCAS_ST$top<- NA
LUCAS_ST$bottom<- NA
LUCAS_ST$profile_id<- NA
LUCAS_ST<- LUCAS_ST[,c("profile_id", "top", "bottom", "Clay..g.kg.1","Silt..g.kg.1","Sand..g.kg.1","GPS_LONG","GPS_.LAT")]
colnames(LUCAS_ST)[which(colnames(LUCAS_ST)%in% c("Clay..g.kg.1"))]<- "clay"
colnames(LUCAS_ST)[which(colnames(LUCAS_ST)%in% c("Silt..g.kg.1"))]<- "silt"
colnames(LUCAS_ST)[which(colnames(LUCAS_ST)%in% c("Sand..g.kg.1"))]<- "sand"
colnames(LUCAS_ST)[which(colnames(LUCAS_ST)%in% c("GPS_LONG"))]<- "Long"
colnames(LUCAS_ST)[which(colnames(LUCAS_ST)%in% c("GPS_.LAT"))]<- "Lat"
colnames(LUCAS_ST)
LUCAS_ST$source<- "LUCAS"
Soil_texture<- rbind(Nobodata_ST, WOSIS_ST, SWISS_ST, SPADE_ST, LUCAS_ST)
#write.csv(Soil_texture, "D:/Swiss_soil_data/land_cover_ponits/SWISS_data11_nobodata/ST_data_2022_13_04.csv")
table(Soil_texture$source)
## organic carbon----------
##### call the OC data
Nobodata_OC<- read.csv("D:/Swiss_soil_data/land_cover_ponits/SWISS_data11_nobodata/measu_data1_Corg2.csv")
SPADE_OC<- read.csv("D:/Swiss_soil_data/OC_Spade_data.csv")
LUCAS_OC<- read.csv("D:/Swiss_soil_data/OC_ST_P_Lucas_data.csv")
### prepare the soil organic carbon columns
colnames(Nobodata_OC)
Nobodata_OC<- Nobodata_OC[,c("ID", "DEPTH_FROM_cm", "DEPTH_TO_cm","MEASURED_VALUE","COORDINATE_X_E","COORDINATE_Y_N")]
colnames(Nobodata_OC)[which(colnames(Nobodata_OC)%in% c("ID"))]<- "profile_id"
colnames(Nobodata_OC)[which(colnames(Nobodata_OC)%in% c("DEPTH_FROM_cm"))]<- "top"
colnames(Nobodata_OC)[which(colnames(Nobodata_OC)%in% c("DEPTH_TO_cm"))]<- "bottom"
colnames(Nobodata_OC)[which(colnames(Nobodata_OC)%in% c("MEASURED_VALUE"))]<- "oc"
colnames(Nobodata_OC)[which(colnames(Nobodata_OC)%in% c("COORDINATE_X_E"))]<- "Long"
colnames(Nobodata_OC)[which(colnames(Nobodata_OC)%in% c("COORDINATE_Y_N"))]<- "Lat"
Nobodata_OC$source<- "Nobodata"
colnames(Nobodata_OC)
### SPADE_ST
colnames(SPADE_OC)
SPADE_OC<- SPADE_OC[!is.na(SPADE_OC$OC_V),]
SPADE_OC<- SPADE_OC[,c("PLOT_ID", "HOR_BEG_V", "HOR_END_V","OC_V","LON_COOR_V","LAT_COOR_V")]
colnames(SPADE_OC)[which(colnames(SPADE_OC)%in% c("PLOT_ID"))]<- "profile_id"
colnames(SPADE_OC)[which(colnames(SPADE_OC)%in% c("HOR_BEG_V"))]<- "top"
colnames(SPADE_OC)[which(colnames(SPADE_OC)%in% c("HOR_END_V"))]<- "bottom"
colnames(SPADE_OC)[which(colnames(SPADE_OC)%in% c("OC_V"))]<- "oc"
colnames(SPADE_OC)[which(colnames(SPADE_OC)%in% c("LON_COOR_V"))]<- "Long"
colnames(SPADE_OC)[which(colnames(SPADE_OC)%in% c("LAT_COOR_V"))]<- "Lat"
colnames(SPADE_OC)
SPADE_OC$source<- "SPADE"
### LUCAS_oc
colnames(LUCAS_OC)
LUCAS_OC$top<- NA
LUCAS_OC$bottom<- NA
LUCAS_OC$profile_id<- NA
LUCAS_OC$Organic.carbon..g.kg.1<- LUCAS_OC$Organic.carbon..g.kg.1/10
LUCAS_OC<- LUCAS_OC[,c("profile_id", "top", "bottom", "Organic.carbon..g.kg.1","GPS_LONG","GPS_.LAT")]
colnames(LUCAS_OC)[which(colnames(LUCAS_OC)%in% c("Organic.carbon..g.kg.1"))]<- "oc"
colnames(LUCAS_OC)[which(colnames(LUCAS_OC)%in% c("GPS_LONG"))]<- "Long"
colnames(LUCAS_OC)[which(colnames(LUCAS_OC)%in% c("GPS_.LAT"))]<- "Lat"
LUCAS_OC$source<- "LUCAS"
colnames(LUCAS_OC)
Soil_OC<- rbind(Nobodata_OC, SPADE_OC, LUCAS_OC)
#write.csv(Soil_OC, "D:/Swiss_soil_data/land_cover_ponits/SWISS_data11_nobodata/OC_data_2022_13_04.csv")
table(Soil_OC$source)
## phosphorus----------
##### call the P datasets
Nobodata_P<- read.csv("D:/Swiss_soil_data/land_cover_ponits/SWISS_data11_nobodata/measu_data2_P.csv")
LUCAS_P<- read.csv("D:/Swiss_soil_data/land_cover_ponits/SWISS_data11_nobodata/OC_ST_P_Lucas_data.csv")
### prepare the phosphorus columns
colnames(Nobodata_P)
Nobodata_P<- Nobodata_P[,c("ID", "DEPTH_FROM_cm", "DEPTH_TO_cm","MEASURED_VALUE","COORDINATE_X_E","COORDINATE_Y_N")]
colnames(Nobodata_P)[which(colnames(Nobodata_P)%in% c("ID"))]<- "profile_id"
colnames(Nobodata_P)[which(colnames(Nobodata_P)%in% c("DEPTH_FROM_cm"))]<- "top"
colnames(Nobodata_P)[which(colnames(Nobodata_P)%in% c("DEPTH_TO_cm"))]<- "bottom"
colnames(Nobodata_P)[which(colnames(Nobodata_P)%in% c("MEASURED_VALUE"))]<- "p"
colnames(Nobodata_P)[which(colnames(Nobodata_P)%in% c("COORDINATE_X_E"))]<- "Long"
colnames(Nobodata_P)[which(colnames(Nobodata_P)%in% c("COORDINATE_Y_N"))]<- "Lat"
Nobodata_P$source<- "Nobodata"
colnames(Nobodata_P)
### LUCAS_P
colnames(LUCAS_P)
LUCAS_P$top<- NA
LUCAS_P$bottom<- NA
LUCAS_P$profile_id<- NA
LUCAS_P<- LUCAS_P[,c("profile_id", "top", "bottom", "Phosphorus..mg.kg.1","GPS_LONG","GPS_.LAT")]
colnames(LUCAS_P)[which(colnames(LUCAS_P)%in% c("Phosphorus..mg.kg.1"))]<- "p"
colnames(LUCAS_P)[which(colnames(LUCAS_P)%in% c("GPS_LONG"))]<- "Long"
colnames(LUCAS_P)[which(colnames(LUCAS_P)%in% c("GPS_.LAT"))]<- "Lat"
LUCAS_P$source<- "LUCAS"
colnames(LUCAS_P)
Soil_P<- rbind(Nobodata_P, LUCAS_P)
#write.csv(Soil_P, "D:/Swiss_soil_data/land_cover_ponits/SWISS_data11_nobodata/P_data_2022_13_04.csv")
##add more datasets
## Combining Mayerhofer dataset
Bacteria<- read.csv("D:/Swiss_soil_data/Bacteria_dataset/Copy of sample_table_organic_carbon_texture_nitrogen.csv")
colnames(Bacteria)
ST<- read.csv("D:/Swiss_soil_data/land_cover_ponits/SWISS_data11_nobodata/ST_data_2022_13_04.csv")
## soil texture
Bacteria_ST<- Bacteria[!is.na(Bacteria$sand),]
Bacteria_ST$top<- 0
Bacteria_ST$bottom<- 20
Bacteria_ST<- Bacteria_ST[,c(3,13,14,12,10,11,8,9)] 
Bacteria_ST$source<- "Mayerhofer_et_al_2021"
colnames(Bacteria_ST)[which(colnames(Bacteria_ST)%in% c("site_ID"))]<- "profile_id"
colnames(Bacteria_ST)[which(colnames(Bacteria_ST)%in% c("long"))]<- "Long"
colnames(Bacteria_ST)[which(colnames(Bacteria_ST)%in% c("lat"))]<- "Lat"
## merge_dataset
colnames(Bacteria_ST)
ST_dataset<- rbind(ST, Bacteria_ST)
## write_data_ST
#write.csv(ST_dataset,"C:/Users/guptasu.D/Documents/Swiss_soil_data/Bacteria_dataset/comb_ST_dataset.csv")
#######Oragnic carbon
OC<- read.csv("D:/Swiss_soil_data/land_cover_ponits/SWISS_data11_nobodata/OC_data_2022_13_04.csv")
##OC = organic carbon
## number of samples in each database
table(OC$source)
#### read the OC of bacteria dataset
Bacteria_OC<- read.csv("D:/Swiss_soil_data/Bacteria_dataset/Copy of sample_table_organic_carbon_texture_nitrogen.csv")
colnames(Bacteria_OC)
## OC
Bacteria_OC<- Bacteria_OC[!is.na(Bacteria_OC$organic_carbon),]
Bacteria_OC$top<- 0
Bacteria_OC$bottom<- 20
Bacteria_OC<- Bacteria_OC[,c(3,13,14,4,8,9)] 
Bacteria_OC$source<- "Mayerhofer_et_al_2021"
colnames(Bacteria_OC)[which(colnames(Bacteria_OC)%in% c("site_ID"))]<- "profile_id"
colnames(Bacteria_OC)[which(colnames(Bacteria_OC)%in% c("long"))]<- "Long"
colnames(Bacteria_OC)[which(colnames(Bacteria_OC)%in% c("lat"))]<- "Lat"
colnames(Bacteria_OC)[which(colnames(Bacteria_OC)%in% c("organic_carbon"))]<- "oc"
## merge_dataset
colnames(Bacteria_OC)
OC_dataset<- rbind(OC, Bacteria_OC)
#write.csv(OC_dataset,"D:/Swiss_soil_data/Bacteria_dataset/comb_oc_dataset.csv")



