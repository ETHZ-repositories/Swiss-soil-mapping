library (foreign)
OC<- read.dbf("D:/multispectral_Landsat8/OC_W_15.dbf")
## Removing the samples over water bodies and built up areas
OC<- OC[!OC$RASTERVALU==190,]
OC<- OC[!OC$RASTERVALU==210,]
## removing OC values above 15 percent
OC<- OC[!(OC$oc>15),]
OC$depth<- (OC$top+OC$bottom)/2
sp.pnts = OC[,c("Long", "Lat")]
## read the fishnet of 1 km
pol.100km = readOGR("D:/multispectral_Landsat8/fishnet1km.shp")
pol.100km$ID1
ov.ID = sp::over(SpatialPoints(sp.pnts, proj4string = CRS(proj4string(pol.100km))), pol.100km["ID1"])
summary(is.na(ov.ID$ID1))
OC$ID1 = ov.ID$ID1
grid <- list.files("D:/multispectral_Landsat8/maps_18_9/Map_4_11_22/New_layers/Topo_data/Topo_f/" , pattern = "*.tif$")
All_cov <- raster::stack(paste0("D:/multispectral_Landsat8/maps_18_9/Map_4_11_22/New_layers/Topo_data/Topo_f/", grid))
## extract the values from the raster maps
data<- terra::extract(All_cov,sp.pnts)
OC_dataset<- cbind(OC,data)
colnames(OC_dataset)
LUCAS<- OC_dataset[OC_dataset$source=="LUCAS",]
Mayerhofer<- OC_dataset[OC_dataset$source=="Mayerhofer_et_al_2021",]
Nobodata<- OC_dataset[OC_dataset$source=="Nobodata",]
Spade<- OC_dataset[OC_dataset$source=="SPADE",]
others<- OC_dataset[!OC_dataset$source=="Nobodata",]
OC_dataset<-Nobodata
#write.csv(OC_dataset,"D:/multispectral_Landsat8/OC_Nabodata__F1_all_76_cov11.csv")
#OC_dataset<-read.csv("D:/multispectral_Landsat8/OC_Nabodata__F1_all_76_cov11.csv")
I.vars = make.names(unique(unlist(sapply(c("B","1_F","hi","Clim","PET", "DEM", "NDVI", "precp","temp3","ID","Lat","Long","depth","lith"), function(i){names(OC_dataset)[grep(i, names(OC_dataset))]}))))
t.vars = c("oc")
sel.n <- c(t.vars,I.vars)
sel.r <- complete.cases(OC_dataset[,sel.n])
PTF_temp2 <- OC_dataset[sel.r,sel.n]
##Five fold cross validation
##Set1----------
set.seed(5)
chosen <- sample(unique(PTF_temp2$ID1),262)
ff<-subset(PTF_temp2, ID1 %in% chosen)
final<-PTF_temp2[!(PTF_temp2$ID1 %in% ff$ID1),]
library("ggplot2")
theme_set(theme_bw())
library("sf")
library("rnaturalearth")
library("rnaturalearthdata")
world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)
ggplot(data = world) +
  geom_sf() +
  geom_point(data = ff, aes(x = Long, y = Lat), size = 3, 
             shape = 21, fill = "darkred")
set.seed(9)
chosen <- sample(unique(final$ID1),252)
ff1<-subset(final, ID1 %in% chosen)
final1<-final[!(final$ID1 %in% ff1$ID1),]
ggplot(data = world) +
  geom_sf() +
  geom_point(data = ff1, aes(x = Long, y = Lat), size = 3, 
             shape = 21, fill = "darkred")
set.seed(3286)
chosen <- sample(unique(final1$ID1),263)
ff2<-subset(final1, ID1 %in% chosen)
ggplot(data = world) +
  geom_sf() +
  geom_point(data = ff2, aes(x = Long, y = Lat), size = 3, 
             shape = 21, fill = "darkred")
final2<-final1[!(final1$ID1 %in% ff2$ID1),]
set.seed(323)
chosen <- sample(unique(final2$ID1),238)
ff3<-subset(final2, ID1 %in% chosen)
ggplot(data = world) +
  geom_sf() +
  geom_point(data = ff3, aes(x = Long, y = Lat), size = 3, 
             shape = 21, fill = "darkred")
final3<-final2[!(final2$ID1 %in% ff3$ID1),]
df1<-ff
df2<-ff1
df3<-ff2
df4<-ff3
df5<-final3
Train1<- rbind(ff, ff1, ff2, ff3)
Train2<- rbind (ff1, ff2, ff3,final3)
Train3<- rbind(ff2, ff3,final3, ff)
Train4<- rbind(ff3,final3, ff,ff1)
Train5<- rbind(final3, ff,ff1, ff2)
grid <- list.files("D:/multispectral_Landsat8/maps_18_9/Map_4_11_22/New_layers/Topo_data/Topo_f/" , pattern = "*.tif$")
All_cov <- raster::stack(paste0("D:/multispectral_Landsat8/maps_18_9/Map_4_11_22/New_layers/Topo_data/Topo_f/", grid))
set.seed(2) 
fm.ksat <- as.formula(paste("oc~ ",paste(names(All_cov), collapse = "+")))
fm.ksat
set.seed(2) 
rm.ksat <- Train1[complete.cases(Train1[,all.vars(fm.ksat)]),]
m.ksat <- ranger(fm.ksat, rm.ksat, num.trees=500, mtry=15, quantreg = TRUE)
m.ksat
df5$prediction<- predict(m.ksat,df5)$predictions
RMSE(df5$prediction, df5$oc)
## Ist_part is computed
rm.ksat1 <- Train2[complete.cases(Train2[,all.vars(fm.ksat)]),]
m.ksat1 <- ranger(fm.ksat, rm.ksat1, num.trees=500, mtry=15, quantreg = TRUE)
m.ksat1
df1$prediction<- predict(m.ksat1,df1)$predictions
RMSE(df1$prediction, df1$oc)
## 2nd_part is computed
rm.ksat2 <- Train3[complete.cases(Train3[,all.vars(fm.ksat)]),]
m.ksat2 <- ranger(fm.ksat, rm.ksat2, num.trees=500, mtry=15, quantreg = TRUE)
m.ksat2
df2$prediction<- predict(m.ksat2,df2)$predictions
RMSE(df2$prediction, df2$oc)
## 3rd_part is computed
rm.ksat3 <- Train4[complete.cases(Train4[,all.vars(fm.ksat)]),]
m.ksat3 <- ranger(fm.ksat, rm.ksat3, num.trees=500, mtry=15, quantreg = TRUE)
m.ksat3
df3$prediction<- predict(m.ksat3,df3)$predictions
RMSE(df3$prediction, df3$oc)
## 4th_part is computed
rm.ksat4 <- Train5[complete.cases(Train5[,all.vars(fm.ksat)]),]
m.ksat4 <- ranger(fm.ksat, rm.ksat4, num.trees=500, mtry=15, quantreg = TRUE)
m.ksat4
df4$prediction<- predict(m.ksat4,df4)$predictions
RMSE(df4$prediction, df4$oc)
Final_data<- rbind(df1,df2,df3,df4,df5)
rss <- sum((Final_data$prediction - Final_data$oc) ^ 2)  ## residual sum of squares
tss <- sum((Final_data$oc - mean(Final_data$oc)) ^ 2)  ## total sum of squares
rsq <- 1 - rss/tss
ll<- lm(Final_data$prediction~ Final_data$oc)
summary(ll)
plot(Final_data$prediction,Final_data$oc)
RMSE(Final_data$prediction,Final_data$oc)
bias(Final_data$prediction,Final_data$oc)
ccc = DescTools::CCC(Final_data$prediction,Final_data$oc, ci = "z-transform", conf.level = 0.95, na.rm=TRUE)$rho.c
ccc
## Mapping

set.seed(2)
rm.ksat <- PTF_temp2[complete.cases(PTF_temp2[,all.vars(fm.ksat)]),]
m.ksat <- ranger(fm.ksat, rm.ksat,importance="impurity", num.trees=500, mtry=15, quantreg = TRUE)
m.ksat

## Improtant covariates
xl <- as.list(ranger::importance(m.ksat))

par(mfrow=c(1,1),oma=c(0.7,2,0,1), mar=c(4,3.5,1,0))

vv <- t(data.frame(xl[order(unlist(xl), decreasing=TRUE)[10:1]]))
# 
# write.csv(vv, "D:/multispectral_Landsat8/OC_variable_imp_02_12_22_20cov.csv")


p2 = predict(All_cov,m.ksat, progress='window',type = "response",fun = function(model, ...) predict(model, ...)$predictions)

writeRaster(p2, "D:/multispectral_Landsat8/OC_map_0cm.tif")

