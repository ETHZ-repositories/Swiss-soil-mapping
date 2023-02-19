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
library(sp)
library(rgdal)
library(raster)
library(foreign)
library(hydroGOF)
library(dplyr)
library(blockCV)
library(caret)
library(randomForest)
library(ranger)
library(mlr)
library(tibble)
library(raster)
library(sp)
library(rgdal)
library(hexbin)
library(lattice)
library(RColorBrewer)
library(viridis)
library(Metrics)


##maps for N map uncertainty

N_dataset<- read.csv("D:/multispectral_Landsat8/N_Nabo_May.csv")

I.vars = make.names(unique(unlist(sapply(c("B","1_F","hi","Clim","PET", "DEM", "NDVI", "precp","temp3","ID","Lat","Long","depth","lith"), function(i){names(N_dataset)[grep(i, names(N_dataset))]}))))

t.vars = c("N")
sel.n <- c(t.vars,I.vars)
sel.r <- complete.cases(N_dataset[,sel.n])
PTF_temp2 <- N_dataset[sel.r,sel.n]

grid <- list.files("D:/multispectral_Landsat8/maps_18_9/Map_4_11_22/New_layers/Topo_data/Topo_f/20cov_oc/N_20_cov/" , pattern = "*.tif$")
All_cov <- raster::stack(paste0("D:/multispectral_Landsat8/maps_18_9/Map_4_11_22/New_layers/Topo_data/Topo_f/20cov_oc/N_20_cov/", grid))

set.seed(2) 
fm.ksat <- as.formula(paste("N~ ",paste(names(All_cov), collapse = "+")))
fm.ksat
# view(Train1)
# # #
# # #
set.seed(2)
rm.ksat <- PTF_temp2[complete.cases(PTF_temp2[,all.vars(fm.ksat)]),]
m.ksat <- ranger(fm.ksat, rm.ksat,importance="impurity", num.trees=500, mtry=15, quantreg = TRUE)
m.ksat

p2 = predict(All_cov,m.ksat, progress='window',type = "quantiles",fun = function(model, ...) predict(model, ...)$predictions)

writeRaster(p2, "D:/multispectral_Landsat8/N_0cm_error_30m1.tif")

## sand content

sand_dataset<- read.csv("D:/multispectral_Landsat8/ST_for_mapping_2_12_2022.csv")

I.vars = make.names(unique(unlist(sapply(c("B","1_F","hi","Clim","PET", "DEM", "NDVI", "precp","temp3","ID","Lat","Long","depth","lith"), function(i){names(sand_dataset)[grep(i, names(sand_dataset))]}))))

t.vars = c("sand")
sel.n <- c(t.vars,I.vars)
sel.r <- complete.cases(sand_dataset[,sel.n])
PTF_temp2 <- sand_dataset[sel.r,sel.n]

grid <- list.files("D:/multispectral_Landsat8/maps_18_9/Map_4_11_22/New_layers/Topo_data/Topo_f/" , pattern = "*.tif$")
All_cov <- raster::stack(paste0("D:/multispectral_Landsat8/maps_18_9/Map_4_11_22/New_layers/Topo_data/Topo_f/", grid))

set.seed(2) 
fm.ksat <- as.formula(paste("sand~ ",paste(names(All_cov), collapse = "+")))
fm.ksat
# view(Train1)
# # #
# # #
set.seed(2)
rm.ksat <- PTF_temp2[complete.cases(PTF_temp2[,all.vars(fm.ksat)]),]
m.ksat <- ranger(fm.ksat, rm.ksat,importance="impurity", num.trees=500, mtry=45, quantreg = TRUE)
m.ksat

p2 = predict(All_cov,m.ksat, progress='window',type = "quantiles",fun = function(model, ...) predict(model, ...)$predictions)

writeRaster(p2, "D:/multispectral_Landsat8/sand_0cm_error_30m.tif")


##clay content

clay_dataset<- read.csv("D:/multispectral_Landsat8/ST_for_mapping_2_12_2022.csv")

I.vars = make.names(unique(unlist(sapply(c("B","1_F","hi","Clim","PET", "DEM", "NDVI", "precp","temp3","ID","Lat","Long","depth","lith"), function(i){names(clay_dataset)[grep(i, names(clay_dataset))]}))))

t.vars = c("clay")
sel.n <- c(t.vars,I.vars)
sel.r <- complete.cases(clay_dataset[,sel.n])
PTF_temp2 <- clay_dataset[sel.r,sel.n]


##0cm
grid <- list.files("D:/multispectral_Landsat8/maps_18_9/Map_4_11_22/New_layers/Topo_data/Topo_f/" , pattern = "*.tif$")
All_cov <- raster::stack(paste0("D:/multispectral_Landsat8/maps_18_9/Map_4_11_22/New_layers/Topo_data/Topo_f/", grid))

set.seed(2) 
fm.ksat <- as.formula(paste("clay~ ",paste(names(All_cov), collapse = "+")))
fm.ksat
# view(Train1)
# # #
# # #
set.seed(2)
rm.ksat <- PTF_temp2[complete.cases(PTF_temp2[,all.vars(fm.ksat)]),]
m.ksat <- ranger(fm.ksat, rm.ksat,importance="impurity", num.trees=500, mtry=45, quantreg = TRUE)
m.ksat

p2 = predict(All_cov,m.ksat, progress='window',type = "quantiles",fun = function(model, ...) predict(model, ...)$predictions)

writeRaster(p2, "D:/multispectral_Landsat8/clay_0cm_error_30m.tif")

## OC content

OC_dataset<-read.csv("D:/multispectral_Landsat8/OC_Nabodata__F1_all_76_cov11.csv")

I.vars = make.names(unique(unlist(sapply(c("B","1_F","hi","Clim","PET", "DEM", "NDVI", "precp","temp3","ID","Lat","Long","depth","lith"), function(i){names(OC_dataset)[grep(i, names(OC_dataset))]}))))

t.vars = c("oc")
sel.n <- c(t.vars,I.vars)
sel.r <- complete.cases(OC_dataset[,sel.n])
PTF_temp2 <- OC_dataset[sel.r,sel.n]


grid <- list.files("D:/multispectral_Landsat8/maps_18_9/Map_4_11_22/New_layers/Topo_data/Topo_f/" , pattern = "*.tif$")
All_cov <- raster::stack(paste0("D:/multispectral_Landsat8/maps_18_9/Map_4_11_22/New_layers/Topo_data/Topo_f/", grid))

set.seed(2) 
fm.ksat <- as.formula(paste("oc~ ",paste(names(All_cov), collapse = "+")))
fm.ksat
# view(Train1)
# # #
# # #
set.seed(2)
rm.ksat <- PTF_temp2[complete.cases(PTF_temp2[,all.vars(fm.ksat)]),]
m.ksat <- ranger(fm.ksat, rm.ksat,importance="impurity", num.trees=500, mtry=25, quantreg = TRUE)
m.ksat

p2 = predict(All_cov,m.ksat, progress='window',type = "quantiles",fun = function(model, ...) predict(model, ...)$predictions)

writeRaster(p2, "D:/multispectral_Landsat8/OC_0cm_error_30m.tif")

## Phosphorus

p_dataset<- read.csv("D:/multispectral_Landsat8/P_for_mapping.csv")

I.vars = make.names(unique(unlist(sapply(c("B","1_F","hi","Clim","PET", "DEM", "NDVI", "precp","temp3","ID","Lat","Long","depth","lith"), function(i){names(p_dataset)[grep(i, names(p_dataset))]}))))

t.vars = c("p")
sel.n <- c(t.vars,I.vars)
sel.r <- complete.cases(p_dataset[,sel.n])
PTF_temp2 <- p_dataset[sel.r,sel.n]


grid <- list.files("D:/multispectral_Landsat8/maps_18_9/Map_4_11_22/New_layers/Topo_data/Topo_f/20cov_oc/P_20_cov/" , pattern = "*.tif$")
All_cov <- raster::stack(paste0("D:/multispectral_Landsat8/maps_18_9/Map_4_11_22/New_layers/Topo_data/Topo_f/20cov_oc/P_20_cov/", grid))


set.seed(2) 
fm.ksat <- as.formula(paste("p~ ",paste(names(All_cov), collapse = "+")))
fm.ksat
# view(Train1)
# # #
# # #
set.seed(2)
rm.ksat <- PTF_temp2[complete.cases(PTF_temp2[,all.vars(fm.ksat)]),]
m.ksat <- ranger(fm.ksat, rm.ksat,importance="impurity", num.trees=500, mtry=5, quantreg = TRUE)
m.ksat

p2 = predict(All_cov,m.ksat, progress='window',type = "quantiles",fun = function(model, ...) predict(model, ...)$predictions)

writeRaster(p2, "D:/multispectral_Landsat8/P_0cm_error_30m.tif")
