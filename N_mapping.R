N<- read.dbf("D:/Swiss_soil_data/Bacteria_dataset/Nit_dat1_cl1.dbf")
N<- N[!N$RASTERVALU==190,]
N<- N[!N$RASTERVALU==210,]
N$top<- as.numeric(N$top)
N$bottom<- as.numeric(N$bottom)
N$depth<- (N$top+N$bottom)/2
sp.pnts = N[,c("Long", "Lat")]
pol.100km = readOGR("D:/multispectral_Landsat8/fishnet1km.shp")
pol.100km$ID1
ov.ID = sp::over(SpatialPoints(sp.pnts, proj4string = CRS(proj4string(pol.100km))), pol.100km["ID1"])
summary(is.na(ov.ID$ID1))
N$ID1 = ov.ID$ID1
grid <- list.files("D:/multispectral_Landsat8/maps_18_9/Map_4_11_22/New_layers/Topo_data/Topo_f/" , pattern = "*.tif$")
All_cov <- raster::stack(paste0("D:/multispectral_Landsat8/maps_18_9/Map_4_11_22/New_layers/Topo_data/Topo_f/", grid))
data<- terra::extract(All_cov,sp.pnts)
N_dataset<- cbind(N,data)
LUCAS<- N_dataset[N_dataset$source=="LUCAS",]
Mayerhofer<- N_dataset[N_dataset$source=="Mayerhofer_et_al_2021",]
Nobodata<- N_dataset[N_dataset$source=="Nobodata",]
N_dataset<-rbind(Nobodata,Mayerhofer)
I.vars = make.names(unique(unlist(sapply(c("B","1_F","hi","Clim","PET", "DEM", "NDVI", "precp","temp3","ID","Lat","Long","depth","lith"), function(i){names(N_dataset)[grep(i, names(N_dataset))]}))))
t.vars = c("N")
sel.n <- c(t.vars,I.vars)
sel.r <- complete.cases(N_dataset[,sel.n])
PTF_temp2 <- N_dataset[sel.r,sel.n]
###
set.seed(6)
chosen <- sample(unique(PTF_temp2$ID1),145)
ff<-subset(PTF_temp2, ID1 %in% chosen)
final<-PTF_temp2[!(PTF_temp2$ID1 %in% ff$ID1),]
set.seed(8)
chosen <- sample(unique(final$ID1),168)
ff1<-subset(final, ID1 %in% chosen)
final1<-final[!(final$ID1 %in% ff1$ID1),]
set.seed(3283)
chosen <- sample(unique(final1$ID1),129)
ff2<-subset(final1, ID1 %in% chosen)
final2<-final1[!(final1$ID1 %in% ff2$ID1),]
set.seed(322)
chosen <- sample(unique(final2$ID1),120)
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
fm.ksat <- as.formula(paste("N~ ",paste(names(All_cov), collapse = "+")))
fm.ksat
set.seed(2) 
rm.ksat <- Train1[complete.cases(Train1[,all.vars(fm.ksat)]),]
m.ksat <- ranger(fm.ksat, rm.ksat, num.trees=500, mtry=25, quantreg = TRUE)
m.ksat
df5$prediction<- predict(m.ksat,df5)$predictions
RMSE(df5$prediction, df5$N)
## Ist_part is computed
rm.ksat1 <- Train2[complete.cases(Train2[,all.vars(fm.ksat)]),]
m.ksat1 <- ranger(fm.ksat, rm.ksat1, num.trees=500, mtry=25, quantreg = TRUE)
m.ksat1
df1$prediction<- predict(m.ksat1,df1)$predictions
RMSE(df1$prediction, df1$N)
## 2nd_part is computed
rm.ksat2 <- Train3[complete.cases(Train3[,all.vars(fm.ksat)]),]
m.ksat2 <- ranger(fm.ksat, rm.ksat2, num.trees=500, mtry=25, quantreg = TRUE)
m.ksat2
df2$prediction<- predict(m.ksat2,df2)$predictions
RMSE(df2$prediction, df2$N)
## 3rd_part is computed
rm.ksat3 <- Train4[complete.cases(Train4[,all.vars(fm.ksat)]),]
m.ksat3 <- ranger(fm.ksat, rm.ksat3, num.trees=500, mtry=25, quantreg = TRUE)
m.ksat3
df3$prediction<- predict(m.ksat3,df3)$predictions
RMSE(df3$prediction, df3$N)
## 4th_part is computed
rm.ksat4 <- Train5[complete.cases(Train5[,all.vars(fm.ksat)]),]
m.ksat4 <- ranger(fm.ksat, rm.ksat4, num.trees=500, mtry=25, quantreg = TRUE)
m.ksat4
df4$prediction<- predict(m.ksat4,df4)$predictions
RMSE(df4$prediction, df4$N)
Final_data<- rbind(df1,df2,df3,df4,df5)
colnames(Final_data)
rss <- sum((Final_data$prediction - Final_data$N) ^ 2)  ## residual sum of squares
tss <- sum((Final_data$N - mean(Final_data$N)) ^ 2)  ## total sum of squares
rsq <- 1 - rss/tss
plot(Final_data$prediction,Final_data$N)
RMSE(Final_data$prediction,Final_data$N)
bias(Final_data$prediction,Final_data$N)
ccc = DescTools::CCC(Final_data$prediction,Final_data$N, ci = "z-transform", conf.level = 0.95, na.rm=TRUE)$rho.c
ccc
#0cm
set.seed(2) 
fm.ksat <- as.formula(paste("N~ ",paste(names(All_cov), collapse = "+")))
fm.ksat
# # #
set.seed(2)
rm.ksat <- PTF_temp2[complete.cases(PTF_temp2[,all.vars(fm.ksat)]),]
m.ksat <- ranger(fm.ksat, rm.ksat,importance="impurity", num.trees=500, mtry=25, quantreg = TRUE)
m.ksat
p2 = predict(All_cov,m.ksat, progress='window',type = "response",fun = function(model, ...) predict(model, ...)$predictions)
writeRaster(p2, "D:/multispectral_Landsat8/N_map_0cm.tif")
xl <- as.list(ranger::importance(m.ksat))
par(mfrow=c(1,1),oma=c(0.7,2,0,1), mar=c(4,3.5,1,0))
plot(vv <- t(data.frame(xl[order(unlist(xl), decreasing=TRUE)[10:1]])), 1:10,
     type = "n", ylab = "", yaxt = "n", xlab = "Variable Importance (Node Impurity)",
     cex.axis = .7, cex.lab = .7)
#write.csv(vv, "D:/multispectral_Landsat8/N_variable_imp_02_12_22.csv")




