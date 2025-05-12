
### Extraccion de la precipitacion mensual desde un netcdf PISCO para una coordenada
### https://github.com/hydrocodes

rm(list=ls())

library(sp)
library(rgdal)
#library(Rccp) # include for R 4.x versions, also try with package "terra"
library(raster)
library(ncdf4)
library(fields)
library(lubridate)
library(lattice)
library(dplyr)
library(easypackages)
library(xts)
library(ggplot2)

setwd("C:/Users/ASUS/Documents/Cuenca")  
cuenca.shape <- readOGR("subcuencaBajoPiura.shp")
class(cuenca.shape)
plot(cuenca.shape, axes=T,col=c("cyan"))
head(cuenca.shape@data)
#Proyeccion y reproyeccion de coordenadas
cuenca.utm <- spTransform(cuenca.shape, CRS("+proj=utm +zone=17 +ellps=WGS84 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))
plot(cuenca.utm, axes=T, asp=1)
cuenca.wgs <- spTransform(cuenca.utm, CRS("+proj=longlat +ellps=WGS84"))
plot(cuenca.wgs, axes=T, asp=1)
# Visualizando el producto raster PISCO de precipitacion mensual
r <- stack("PISCO Prec v2p1.nc") 
# Visualizando la Precipitacion espacial del 1er mes (Enero) de 1981.
plot(r[[1]])
# Ploteando la cuenca dentro del mapa de precipitaciones
plot(cuenca.wgs, add=T)
# Delimitando el área de estudio al cuadrante que ocupa la cuenca
raster_pp <- crop(r, cuenca.wgs, snap="out")
# Ploteando enero de 1981 en el cuadrante que ocupa la cuenca
plot(raster_pp[[1]])
plot(cuenca.wgs, add=T)

#Ingresando las coordenadas del punto a extraer desde un archivo CSV
long_lat <- read.csv("Estaciones4.csv", header = T)


#Estaciones1
sp::coordinates(long_lat) <- ~XX+YY
raster::projection(long_lat) <- raster::projection(raster_pp)
points_long_lat <- raster::extract(raster_pp[[1]], long_lat, cellnumbers = T)[,1]
data_long_lat <- t(raster_pp[points_long_lat])
colnames(data_long_lat) <- as.character(long_lat$NN)


#Guardar informacion
write.csv(data_long_lat, "Pisco19812016.csv", quote = F)


#Para el caso de cuencas que abarcan mas de una grilla. Estimando el peso de cada grilla
weights <- raster::extract(raster_pp[[1]], cuenca.wgs, cellnumbers=T,  weights=T, small=T) 
weights

#Ordenamos Información

series<- data.frame("Fecha"=as.POSIXlt(seq.Date(from=as.Date("1981-01-01"),to=as.Date("2016-12-31"), by= "day"),format="%Y-%m-$d"),data_long_lat)
write.csv(series, "Pisco19812016_1.csv", quote = F)


#convertimor a una serie temporal

precipitacion.ts<-ts(series,start=1981, frequency = 365)
summary(precipitacion.ts[,-1])
maximo<-max(precipitacion.ts[,-1],na.rm = T)
maximo


#Graficamos
plot(precipitacion.ts[,-1], main="Histograma de precipitacion de la estaciones",ylim = c(0,maximo))

xyplot(precipitacion.ts[,-1], main="Histograma de precipitación", xlab="fecha", ylab="Precipitación [mm/día]",ylim = c(0,maximo))




#Extraccion de datos horarios

# Visualizando el producto raster PISCO horario 2015
r1 <- stack("PISCOp_h_2015.nc") 
# Delimitando el área de estudio al cuadrante que ocupa la cuenca
raster_pp1 <- crop(r1, cuenca.wgs, snap="out")
#Ingresando las coordenadas del punto a extraer desde un archivo CSV
long_lat1 <- read.csv("Estaciones4.csv", header = T)
#Estaciones1
sp::coordinates(long_lat1) <- ~XX+YY
raster::projection(long_lat1) <- raster::projection(raster_pp1)
points_long_lat1 <- raster::extract(raster_pp1[[1]], long_lat1, cellnumbers = T)[,1]
data_long_lat1 <- t(raster_pp1[points_long_lat1])
colnames(data_long_lat1) <- as.character(long_lat1$NN)


# Visualizando el producto raster PISCO horario 2016
r2 <- stack("PISCOp_h_2016.nc") 
# Delimitando el área de estudio al cuadrante que ocupa la cuenca
raster_pp2 <- crop(r2, cuenca.wgs, snap="out")
#Ingresando las coordenadas del punto a extraer desde un archivo CSV
long_lat2 <- read.csv("Estaciones4.csv", header = T)
#Estaciones1
sp::coordinates(long_lat2) <- ~XX+YY
raster::projection(long_lat2) <- raster::projection(raster_pp2)
points_long_lat2 <- raster::extract(raster_pp2[[1]], long_lat2, cellnumbers = T)[,1]
data_long_lat2 <- t(raster_pp2[points_long_lat2])
colnames(data_long_lat2) <- as.character(long_lat2$NN)


# Visualizando el producto raster PISCO horario 2017
r3 <- stack("PISCOp_h_2017.nc") 
# Delimitando el área de estudio al cuadrante que ocupa la cuenca
raster_pp3 <- crop(r3, cuenca.wgs, snap="out")
#Ingresando las coordenadas del punto a extraer desde un archivo CSV
long_lat3 <- read.csv("Estaciones4.csv", header = T)
#Estaciones1
sp::coordinates(long_lat3) <- ~XX+YY
raster::projection(long_lat3) <- raster::projection(raster_pp3)
points_long_lat3 <- raster::extract(raster_pp3[[1]], long_lat3, cellnumbers = T)[,1]
data_long_lat3 <- t(raster_pp3[points_long_lat3])
colnames(data_long_lat3) <- as.character(long_lat3$NN)



# Visualizando el producto raster PISCO horario 2017
r4 <- stack("PISCOp_h_2018.nc") 
# Delimitando el área de estudio al cuadrante que ocupa la cuenca
raster_pp4 <- crop(r4, cuenca.wgs, snap="out")
#Ingresando las coordenadas del punto a extraer desde un archivo CSV
long_lat4 <- read.csv("Estaciones4.csv", header = T)
#Estaciones1
sp::coordinates(long_lat4) <- ~XX+YY
raster::projection(long_lat4) <- raster::projection(raster_pp4)
points_long_lat4 <- raster::extract(raster_pp4[[1]], long_lat4, cellnumbers = T)[,1]
data_long_lat4 <- t(raster_pp4[points_long_lat4])
colnames(data_long_lat4) <- as.character(long_lat4$NN)



# Visualizando el producto raster PISCO horario 2017
r5 <- stack("PISCOp_h_2019.nc") 
# Delimitando el área de estudio al cuadrante que ocupa la cuenca
raster_pp5 <- crop(r5, cuenca.wgs, snap="out")
#Ingresando las coordenadas del punto a extraer desde un archivo CSV
long_lat5 <- read.csv("Estaciones4.csv", header = T)
#Estaciones1
sp::coordinates(long_lat5) <- ~XX+YY
raster::projection(long_lat5) <- raster::projection(raster_pp5)
points_long_lat5 <- raster::extract(raster_pp5[[1]], long_lat5, cellnumbers = T)[,1]
data_long_lat5 <- t(raster_pp5[points_long_lat5])
colnames(data_long_lat5) <- as.character(long_lat5$NN)


# Visualizando el producto raster PISCO horario 2020
r6 <- stack("PISCOp_h_2020.nc") 
# Delimitando el área de estudio al cuadrante que ocupa la cuenca
raster_pp6 <- crop(r6, cuenca.wgs, snap="out")
#Ingresando las coordenadas del punto a extraer desde un archivo CSV
long_lat6 <- read.csv("Estaciones4.csv", header = T)
#Estaciones1
sp::coordinates(long_lat6) <- ~XX+YY
raster::projection(long_lat6) <- raster::projection(raster_pp6)
points_long_lat6 <- raster::extract(raster_pp6[[1]], long_lat6, cellnumbers = T)[,1]
data_long_lat6 <- t(raster_pp6[points_long_lat6])
colnames(data_long_lat6) <- as.character(long_lat6$NN)

#Guardar informacion
write.csv(data_long_lat1, "Pisco2015h.csv", quote = F)
write.csv(data_long_lat2, "Pisco2016h.csv", quote = F)
write.csv(data_long_lat3, "Pisco2017h.csv", quote = F)
write.csv(data_long_lat4, "Pisco2018h.csv", quote = F)
write.csv(data_long_lat5, "Pisco2019h.csv", quote = F)
write.csv(data_long_lat6, "Pisco2020h.csv", quote = F)


##Datos horarios a diarios PISCO
#Pasar datos horarios a diarios 2015
df <- read.csv("Pisco2015h.csv", header = T)
df$X <- as.POSIXct(gsub("X", "", df$X), format = "%Y.%m.%d.%H.%M.%S")
# Pasa los datos horarios a diarios utilizando aggregate()
df_diario <- aggregate(. ~ format(X, "%Y-%m-%d"), data = df, FUN = sum)
df_diario <- df_diario %>% select(-X)
names (df_diario)[1]="Fecha"
head(df_diario)

#Pasar datos horarios a diarios 2016
df1 <- read.csv("Pisco2016h.csv", header = T)
df1$X <- as.POSIXct(gsub("X", "", df1$X), format = "%Y.%m.%d.%H.%M.%S")
# Pasa los datos horarios a diarios utilizando aggregate()
df1_diario <- aggregate(. ~ format(X, "%Y-%m-%d"), data = df1, FUN = sum)
df1_diario <- df1_diario %>% select(-X)
names (df1_diario)[1]="Fecha"
head(df1_diario)

#Pasar datos horarios a diarios 2017
df2 <- read.csv("Pisco2017h.csv", header = T)
df2$X <- as.POSIXct(gsub("X", "", df2$X), format = "%Y.%m.%d.%H.%M.%S")
# Pasa los datos horarios a diarios utilizando aggregate()
df2_diario <- aggregate(. ~ format(X, "%Y-%m-%d"), data = df2, FUN = sum)
df2_diario <- df2_diario %>% select(-X)
names (df2_diario)[1]="Fecha"
head(df2_diario)

#Pasar datos horarios a diarios 2018
df3 <- read.csv("Pisco2018h.csv", header = T)
df3$X <- as.POSIXct(gsub("X", "", df3$X), format = "%Y.%m.%d.%H.%M.%S")
# Pasa los datos horarios a diarios utilizando aggregate()
df3_diario <- aggregate(. ~ format(X, "%Y-%m-%d"), data = df3, FUN = sum)
df3_diario <- df3_diario %>% select(-X)
names (df3_diario)[1]="Fecha"
head(df3_diario)

#Pasar datos horarios a diarios 2019
df4 <- read.csv("Pisco2019h.csv", header = T)
df4$X <- as.POSIXct(gsub("X", "", df4$X), format = "%Y.%m.%d.%H.%M.%S")
# Pasa los datos horarios a diarios utilizando aggregate()
df4_diario <- aggregate(. ~ format(X, "%Y-%m-%d"), data = df4, FUN = sum)
df4_diario <- df4_diario %>% select(-X)
names (df4_diario)[1]="Fecha"
head(df4_diario)

#Pasar datos horarios a diarios 2020
df5 <- read.csv("Pisco2020h.csv", header = T)
df5$X <- as.POSIXct(gsub("X", "", df5$X), format = "%Y.%m.%d.%H.%M.%S")
# Pasa los datos horarios a diarios utilizando aggregate()
df5_diario <- aggregate(. ~ format(X, "%Y-%m-%d"), data = df5, FUN = sum)
df5_diario <- df5_diario %>% select(-X)
names (df5_diario)[1]="Fecha"
head(df5_diario)

#Pisco 1981-2016
df6 <-read.csv("Pisco19812016_1.csv",header =T)
df6 <- df6 %>% select(-X)
head(df6)

#Juntar archivos CSV
df_junto <- rbind(df6,df2_diario,df3_diario,df4_diario,df5_diario)


names (df_junto)[2]="Laguna Ramon"
names (df_junto)[3]="Chusis"
names (df_junto)[4]="Montegrande"
names (df_junto)[5]="Qda. Las Monjas"
names (df_junto)[6]="Miraflores"
names (df_junto)[7]="El tablazo"
names (df_junto)[8]="Hualtaco"
names (df_junto)[9]="Bernal"
names (df_junto)[10]="San Miguel"
write.csv(df_junto, "Pisco19812020.csv", quote = F)

data<-read.csv("Pisco19812020.csv", header=T)
lagunaramon<-data[,3]
Chusis<-data[,4]
Montegrande<-data[,5]
Lasmonjas<-data[,6]
Miraflores<-data[,7]
Tablazo<-data[,8]
Hualtaco<-data[,9]
bernal<-data[,10]
Sanmiguel<-data[,11]
#convertir a series de tiempo
prep1<-ts(lagunaramon,start=1981, frequency = 365)
prep2<-ts(Chusis,start=1981, frequency = 365)
prep3<-ts(Montegrande,start=1981, frequency = 365)
prep4<-ts(Lasmonjas,start=1981, frequency = 365)
prep5<-ts(Miraflores,start=1981, frequency = 365)
prep6<-ts(Tablazo,start=1981, frequency = 365)
prep7<-ts(Hualtaco,start=1981, frequency = 365)
prep8<-ts(bernal,start=1981, frequency = 365)
prep9<-ts(Sanmiguel,start=1981, frequency = 365)

summary(long_lat)
max1<-max(prep1,na.rm = T)
max2<-max(prep2,na.rm = T)
max3<-max(prep3,na.rm = T)
max4<-max(prep4,na.rm = T)
max5<-max(prep5,na.rm = T)
max6<-max(prep6,na.rm = T)
max7<-max(prep7,na.rm = T)
max8<-max(prep8,na.rm = T)
max9<-max(prep9,na.rm = T)



#Graficamos PISCO 1981-2020

xyplot(prep1, main="Precipitación de la estación Laguna Ramón ", xlab="fecha", ylab="Precipitación [mm/día]",ylim = c(0,max1))
xyplot(prep2, main="Precipitación de la estación Chusis ", xlab="fecha", ylab="Precipitación [mm/día]",ylim = c(0,max2))
xyplot(prep3, main="Precipitación de la estación Montegrande ", xlab="fecha", ylab="Precipitación [mm/día]",ylim = c(0,max3))
xyplot(prep4, main="Precipitación de la estación Qba. Las monjas ", xlab="fecha", ylab="Precipitación [mm/día]",ylim = c(0,max4))
xyplot(prep5, main="Precipitación de la estación Miraflores ", xlab="fecha", ylab="Precipitación [mm/día]",ylim = c(0,max5))
xyplot(prep6, main="Precipitación de la estación El tablazo ", xlab="fecha", ylab="Precipitación [mm/día]",ylim = c(0,max6))
xyplot(prep7, main="Precipitación de la estación Hualtaco ", xlab="fecha", ylab="Precipitación [mm/día]",ylim = c(0,max7))
xyplot(prep8, main="Precipitación de la estación Bernal ", xlab="fecha", ylab="Precipitación [mm/día]",ylim = c(0,max8))
xyplot(prep9, main="Precipitación de la estación San Miguel ", xlab="fecha", ylab="Precipitación [mm/día]",ylim = c(0,max9))








str(data)
# Eliminar columna X
data1 <- data[ , !(names(data) %in% c("X"))]
idx <- as.Date(data1[,1])
data.matrix <- data1[,-1]
data.xts <- xts(data.matrix, order.by = idx )
str(data.xts)
plot(data.xts,type="l",main="Precipitación diaria de las estaciones (mm/día)")
plot(data.matrix[,1], type="l")
plot(data.xts[,3])
#Convirtiendo a mensuales la estacion 103
data.monthly <- apply.monthly(data.xts[,3], FUN = sum)
plot(data.monthly)
#Todas las estaciones a mensuales
data.monthly <- apply.monthly(data.xts, FUN=apply, MARGIN = 2, sum)
write.csv(data.monthly, "Pisco19812020mensual.csv", quote = F)
a<-max(data.monthly)
xyplot(data.monthly[,1], ylim = c(0,a), main="Precitación mensual estación Laguna Ramon")
xyplot(data.monthly[,2], ylim = c(0,a), main="Precitación mensual estación Chusis")
xyplot(data.monthly[,3], ylim = c(0,a), main="Precitación mensual estación Montegrande")
xyplot(data.monthly[,4], ylim = c(0,a), main="Precitación mensual estación Qda. Las Monjas")
xyplot(data.monthly[,5], ylim = c(0,a), main="Precitación mensual estación Miraflores")
xyplot(data.monthly[,6], ylim = c(0,a), main="Precitación mensual estación El Tablazo")
xyplot(data.monthly[,7], ylim = c(0,a), main="Precitación mensual estación Hualtaco")
xyplot(data.monthly[,8], ylim = c(0,a), main="Precitación mensual estación Bernal")
xyplot(data.monthly[,9], ylim = c(0,a), main="Precitación mensual estación San Miguel")
xyplot(data.monthly, ylim = c(0,a), main="Precitación total mensual [mm/mes]")
#Convirtiendo a anuales la estacion 103
data.anual <- apply.yearly(data.xts[,3], FUN = sum)
write.csv(data.anual, "Pisco19812020anual.csv", quote = F)
barplot(data.anual)
#Todas las estaciones a anuales
data.anual <- apply.yearly(data.monthly, FUN=apply, 2, sum)
write.csv(data.anual, "Pisco19812020anual.csv", quote = F)
xyplot(data.anual)
boxplot(coredata(data.monthly)) 

#Datos mensuales por estación, para la creación de boxplot mensual
# Etiquetas de los meses en español
month_labels_es <- c("Ene", "Feb", "Mar", "Abr", "May", "Jun", "Jul", "Ago", "Sep", "Oct", "Nov", "Dic")
#BoxplotMeses
# Crear el boxplot con los datos mensuales y etiquetas en español
boxplot(matrix(coredata(data.monthly[,1]), nrow=nrow(data.monthly)/12, ncol=12, byrow=T), col="gray", main=c(paste(names(data.monthly[,1])), "Prec mensual [mm]"), xaxt="n")
axis(1, at=1:12, labels=F)
text(x = 1:12, y = par("usr")[3] - 0.5, labels = month_labels_es, srt = 90, adj = 1.5, xpd = TRUE)
boxplot(matrix(coredata(data.monthly[,2]), nrow=nrow(data.monthly)/12, ncol=12, byrow=T), col="gray", main=c(paste(names(data.monthly[,2])), "Prec mensual [mm]"), xaxt="n")
axis(1, at=1:12, labels=F)
text(x = 1:12, y = par("usr")[3] - 0.5, labels = month_labels_es, srt = 90, adj = 1.5, xpd = TRUE)
boxplot(matrix(coredata(data.monthly[,3]), nrow=nrow(data.monthly)/12, ncol=12, byrow=T), col="gray", main=c(paste(names(data.monthly[,3])), "Prec mensual [mm]"), xaxt="n")
axis(1, at=1:12, labels=F)
text(x = 1:12, y = par("usr")[3] - 0.5, labels = month_labels_es, srt = 90, adj = 1.5, xpd = TRUE)
boxplot(matrix(coredata(data.monthly[,4]), nrow=nrow(data.monthly)/12, ncol=12, byrow=T), col="gray", main=c(paste(names(data.monthly[,4])), "Prec mensual [mm]"), xaxt="n")
axis(1, at=1:12, labels=F)
text(x = 1:12, y = par("usr")[3] - 0.5, labels = month_labels_es, srt = 90, adj = 1.5, xpd = TRUE)
boxplot(matrix(coredata(data.monthly[,5]), nrow=nrow(data.monthly)/12, ncol=12, byrow=T), col="gray", main=c(paste(names(data.monthly[,5])), "Prec mensual [mm]"), xaxt="n")
axis(1, at=1:12, labels=F)
text(x = 1:12, y = par("usr")[3] - 0.5, labels = month_labels_es, srt = 90, adj = 1.5, xpd = TRUE)
boxplot(matrix(coredata(data.monthly[,6]), nrow=nrow(data.monthly)/12, ncol=12, byrow=T), col="gray", main=c(paste(names(data.monthly[,6])), "Prec mensual [mm]"), xaxt="n")
axis(1, at=1:12, labels=F)
text(x = 1:12, y = par("usr")[3] - 0.5, labels = month_labels_es, srt = 90, adj = 1.5, xpd = TRUE)
boxplot(matrix(coredata(data.monthly[,7]), nrow=nrow(data.monthly)/12, ncol=12, byrow=T), col="gray", main=c(paste(names(data.monthly[,7])), "Prec mensual [mm]"), xaxt="n")
axis(1, at=1:12, labels=F)
text(x = 1:12, y = par("usr")[3] - 0.5, labels = month_labels_es, srt = 90, adj = 1.5, xpd = TRUE)
boxplot(matrix(coredata(data.monthly[,8]), nrow=nrow(data.monthly)/12, ncol=12, byrow=T), col="gray", main=c(paste(names(data.monthly[,8])), "Prec mensual [mm]"), xaxt="n")
axis(1, at=1:12, labels=F)
text(x = 1:12, y = par("usr")[3] - 0.5, labels = month_labels_es, srt = 90, adj = 1.5, xpd = TRUE)
boxplot(matrix(coredata(data.monthly[,9]), nrow=nrow(data.monthly)/12, ncol=12, byrow=T), col="gray", main=c(paste(names(data.monthly[,9])), "Prec mensual [mm]"), xaxt="n")
axis(1, at=1:12, labels=F)
text(x = 1:12, y = par("usr")[3] - 0.5, labels = month_labels_es, srt = 90, adj = 1.5, xpd = TRUE)

















