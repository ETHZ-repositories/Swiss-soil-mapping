library (foreign)
ST<- read.dbf("D:/multispectral_Landsat8/dataset_Swiz/landsat_pro/ST_sampleslulc.dbf")
ST<- OC1212[!OC1212$RASTERVALU==190,]
ST<- OC1212[!OC1212$RASTERVALU==210,]
ST$depth<- (ST$top+ST$bottom)/2
## Removing samples with negative values of soil depth
ST<- ST[ST$top>=0,]
sp.pnts = ST[,c("Long", "Lat")]
pol.100km = readOGR("D:/multispectral_Landsat8/fishnet1km.shp")
pol.100km$ID1
ov.ID = sp::over(SpatialPoints(sp.pnts, proj4string = CRS(proj4string(pol.100km))), pol.100km["ID1"])
summary(is.na(ov.ID$ID1))
ST$ID1 = ov.ID$ID1
grid <- list.files("D:/multispectral_Landsat8/maps_18_9/Map_4_11_22/New_layers/Topo_data/Topo_f/" , pattern = "*.tif$")
All_cov <- raster::stack(paste0("D:/multispectral_Landsat8/maps_18_9/Map_4_11_22/New_layers/Topo_data/Topo_f/", grid))
## extract points
data<- terra::extract(All_cov,sp.pnts)
ST_dataset<- cbind(ST,data)
LUCAS<- ST_dataset[ST_dataset$source=="LUCAS",]
LUCAS$bottom <- 20
LUCAS$depth<- (LUCAS$top+LUCAS$bottom)/2
Mayerhofer<- ST_dataset[ST_dataset$source=="Mayerhofer_et_al_2021",]
Nobodata<- ST_dataset[ST_dataset$source=="Nobodata",]
Spade<- ST_dataset[ST_dataset$source=="SPADE",]
others<- ST_dataset[!ST_dataset$source=="Nobodata",]
ST_dataset <- Nobodata
I.vars = make.names(unique(unlist(sapply(c("B","1_F","hi","Clim","PET", "DEM", "NDVI", "precp","temp3","ID","Lat","Long","depth","lith"), function(i){names(ST_dataset)[grep(i, names(ST_dataset))]}))))
t.vars = c("clay")
sel.n <- c(t.vars,I.vars)
sel.r <- complete.cases(ST_dataset[,sel.n])
PTF_temp2 <- ST_dataset[sel.r,sel.n]
summary(PTF_temp2)
##Set1----------
set.seed(2)
chosen <- sample(unique(PTF_temp2$ID1),1107)
ff<-subset(PTF_temp2, ID1 %in% chosen)
final<-PTF_temp2[!(PTF_temp2$ID1 %in% ff$ID1),]
set.seed(8)
chosen <- sample(unique(final$ID1),1047)
ff1<-subset(final, ID1 %in% chosen)
final1<-final[!(final$ID1 %in% ff1$ID1),]
set.seed(3286)
chosen <- sample(unique(final1$ID1),1096)
ff2<-subset(final1, ID1 %in% chosen)
final2<-final1[!(final1$ID1 %in% ff2$ID1),]
set.seed(322)
chosen <- sample(unique(final2$ID1),1085)
ff3<-subset(final2, ID1 %in% chosen)
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
set.seed(2) 
fm.ksat <- as.formula(paste("clay~ ",paste(names(All_cov), collapse = "+")))
fm.ksat
set.seed(2) 
rm.ksat <- Train1[complete.cases(Train1[,all.vars(fm.ksat)]),]
m.ksat <- ranger(fm.ksat, rm.ksat, num.trees=500, mtry=45, quantreg = TRUE)
m.ksa
df5$prediction<- predict(m.ksat,df5)$predictions
RMSE(df5$prediction, df5$clay)
## Ist_part is computed
rm.ksat1 <- Train2[complete.cases(Train2[,all.vars(fm.ksat)]),]
m.ksat1 <- ranger(fm.ksat, rm.ksat1, num.trees=500, mtry=45, quantreg = TRUE)
m.ksat1
df1$prediction<- predict(m.ksat1,df1)$predictions
RMSE(df1$prediction, df1$clay)
## 2nd_part is computed
rm.ksat2 <- Train3[complete.cases(Train3[,all.vars(fm.ksat)]),]
m.ksat2 <- ranger(fm.ksat, rm.ksat2, num.trees=500, mtry=45, quantreg = TRUE)
m.ksat2
df2$prediction<- predict(m.ksat2,df2)$predictions
RMSE(df2$prediction, df2$clay)
## 3rd_part is computed
rm.ksat3 <- Train4[complete.cases(Train4[,all.vars(fm.ksat)]),]
m.ksat3 <- ranger(fm.ksat, rm.ksat3, num.trees=500, mtry=45, quantreg = TRUE)
m.ksat3
df3$prediction<- predict(m.ksat3,df3)$predictions
RMSE(df3$prediction, df3$clay)
## 4th_part is computed
rm.ksat4 <- Train5[complete.cases(Train5[,all.vars(fm.ksat)]),]
m.ksat4 <- ranger(fm.ksat, rm.ksat4, num.trees=500, mtry=45, quantreg = TRUE)
m.ksat4
df4$prediction<- predict(m.ksat4,df4)$predictions
RMSE(df4$prediction, df4$clay)
Final_data<- rbind(df1,df2,df3,df4,df5)
## 
rss <- sum((Final_data$prediction - Final_data$clay) ^ 2)  ## residual sum of squares
tss <- sum((Final_data$clay - mean(Final_data$clay)) ^ 2)  ## total sum of squares
rsq <- 1 - rss/tss
plot(Final_data$prediction,Final_data$clay)
RMSE(Final_data$prediction,Final_data$clay)
bias(Final_data$prediction,Final_data$clay)
ccc = DescTools::CCC(Final_data$prediction,Final_data$sand, ci = "z-transform", conf.level = 0.95, na.rm=TRUE)$rho.c
ccc

## Mapping
set.seed(2)
rm.ksat <- PTF_temp2[complete.cases(PTF_temp2[,all.vars(fm.ksat)]),]
m.ksat <- ranger(fm.ksat, rm.ksat,importance="impurity", num.trees=500, mtry=45, quantreg = TRUE)
m.ksat
xl <- as.list(ranger::importance(m.ksat))
vv <- t(data.frame(xl[order(unlist(xl), decreasing=TRUE)[20:1]]))
par(mfrow=c(1,1),oma=c(0.7,2,0,1), mar=c(4,3.5,1,0))
plot(vv <- t(data.frame(xl[order(unlist(xl), decreasing=TRUE)[10:1]])), 1:10,
     type = "n", ylab = "", yaxt = "n", xlab = "Variable Importance (Node Impurity)",
     cex.axis = .7, cex.lab = .7)
p2 = predict(All_cov,m.ksat, progress='window',type = "response",fun = function(model, ...) predict(model, ...)$predictions)
writeRaster(p2, "D:/multispectral_Landsat8/sand_F_all_0cm_76_test1.tif")





