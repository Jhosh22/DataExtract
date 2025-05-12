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
#Visualizacion de cuenca
#Lectura de cuenca
setwd("C:/Users/ASUS/Documents/Octavo Ciclo/Tesis/CHIRPS")  
setwd("E:/Octavo Ciclo/CHIRPS")
cuenca.shape <- readOGR("subcuencaBajoPiura_UTM.shp")
class(cuenca.shape)
plot(cuenca.shape, axes=T,col=c("cyan"))
head(cuenca.shape@data)
#Proyeccion de coordenadas
cuenca.utm <- spTransform(cuenca.shape, CRS("+proj=utm +zone=17 +ellps=WGS84 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))
plot(cuenca.utm, axes=T, asp=1)
cuenca.wgs <- spTransform(cuenca.utm, CRS("+proj=longlat +ellps=WGS84"))
plot(cuenca.wgs, axes=T, asp=1)
plot(a1981[[1]])
plot(cuenca.wgs, add=T)
#Lectura de netfdc
a1981 <- stack("chirps-v2.0.1981.days_p05.nc")
a1982 <- stack("chirps-v2.0.1982.days_p05.nc")
a1983 <- stack("chirps-v2.0.1983.days_p05.nc")
a1984 <- stack("chirps-v2.0.1984.days_p05.nc")
a1985 <- stack("chirps-v2.0.1985.days_p05.nc")
a1986 <- stack("chirps-v2.0.1986.days_p05.nc")
a1987 <- stack("chirps-v2.0.1987.days_p05.nc")
a1988 <- stack("chirps-v2.0.1988.days_p05.nc")
a1989 <- stack("chirps-v2.0.1989.days_p05.nc")
a1990 <- stack("chirps-v2.0.1990.days_p05.nc")
a1991 <- stack("chirps-v2.0.1991.days_p05.nc")
a1992 <- stack("chirps-v2.0.1992.days_p05.nc")
a1993 <- stack("chirps-v2.0.1993.days_p05.nc")
a1994 <- stack("chirps-v2.0.1994.days_p05.nc")
a1995 <- stack("chirps-v2.0.1995.days_p05.nc")
a1996 <- stack("chirps-v2.0.1996.days_p05.nc")
a1997 <- stack("chirps-v2.0.1997.days_p05.nc")
a1998 <- stack("chirps-v2.0.1998.days_p05.nc")
a1999 <- stack("chirps-v2.0.1999.days_p05.nc")
a2000 <- stack("chirps-v2.0.2000.days_p05.nc")
a2001 <- stack("chirps-v2.0.2001.days_p05.nc")
a2002 <- stack("chirps-v2.0.2002.days_p05.nc")
a2003 <- stack("chirps-v2.0.2003.days_p05.nc")
a2004 <- stack("chirps-v2.0.2004.days_p05.nc")
a2005 <- stack("chirps-v2.0.2005.days_p05.nc")
a2006 <- stack("chirps-v2.0.2006.days_p05.nc")
a2007 <- stack("chirps-v2.0.2007.days_p05.nc")
a2008 <- stack("chirps-v2.0.2008.days_p05.nc")
a2009 <- stack("chirps-v2.0.2009.days_p05.nc")
a2010 <- stack("chirps-v2.0.2010.days_p05.nc")
a2011 <- stack("chirps-v2.0.2011.days_p05.nc")
a2012 <- stack("chirps-v2.0.2012.days_p05.nc")
a2013 <- stack("chirps-v2.0.2013.days_p05.nc")
a2014 <- stack("chirps-v2.0.2014.days_p05.nc")
a2015 <- stack("chirps-v2.0.2015.days_p05.nc")
a2016 <- stack("chirps-v2.0.2016.days_p05.nc")
a2017 <- stack("chirps-v2.0.2017.days_p05.nc")
a2018 <- stack("chirps-v2.0.2018.days_p05.nc")
a2019 <- stack("chirps-v2.0.2019.days_p05.nc")
a2020 <- stack("chirps-v2.0.2020.days_p05.nc")
a2021 <- stack("chirps-v2.0.2021.days_p05.nc")
a2022 <- stack("chirps-v2.0.2022.days_p05.nc")
a2023 <- stack("chirps-v2.0.2023.days_p05.nc")

# Delimitando el área de estudio al cuadrante que ocupa la cuenca

raster_1981 <- crop(a1981, cuenca.wgs, snap="out")
plot(raster_1981[[1]])
plot(cuenca.wgs, add=T)
raster_1982 <- crop(a1982, cuenca.wgs, snap="out")
plot(raster_1982[[1]])
plot(cuenca.wgs, add=T)
raster_1983 <- crop(a1983, cuenca.wgs, snap="out") 
plot(raster_1983[[1]])
plot(cuenca.wgs, add=T)
raster_1984 <- crop(a1984, cuenca.wgs, snap="out") 
plot(raster_1984[[1]])
plot(cuenca.wgs, add=T)
raster_1985 <- crop(a1985, cuenca.wgs, snap="out") 
raster_1986 <- crop(a1986, cuenca.wgs, snap="out") 
raster_1987 <- crop(a1987, cuenca.wgs, snap="out") 
raster_1988 <- crop(a1988, cuenca.wgs, snap="out") 
raster_1989 <- crop(a1989, cuenca.wgs, snap="out") 
raster_1990 <- crop(a1990, cuenca.wgs, snap="out") 
raster_1991 <- crop(a1991, cuenca.wgs, snap="out") 
raster_1992 <- crop(a1992, cuenca.wgs, snap="out") 
raster_1993 <- crop(a1993, cuenca.wgs, snap="out")
raster_1994 <- crop(a1994, cuenca.wgs, snap="out")
raster_1995 <- crop(a1995, cuenca.wgs, snap="out") 
raster_1996 <- crop(a1996, cuenca.wgs, snap="out") 
raster_1997 <- crop(a1997, cuenca.wgs, snap="out") 
plot(raster_1997[[1]])
plot(cuenca.wgs, add=T)
raster_1998 <- crop(a1998, cuenca.wgs, snap="out")
plot(raster_1998[[1]])
plot(cuenca.wgs, add=T)
raster_1999 <- crop(a1999, cuenca.wgs, snap="out") 
raster_2000 <- crop(a2000, cuenca.wgs, snap="out") 
raster_2001 <- crop(a2001, cuenca.wgs, snap="out") 
raster_2002 <- crop(a2002, cuenca.wgs, snap="out") 
raster_2003 <- crop(a2003, cuenca.wgs, snap="out") 
raster_2004 <- crop(a2004, cuenca.wgs, snap="out") 
raster_2005 <- crop(a2005, cuenca.wgs, snap="out")
raster_2006 <- crop(a2006, cuenca.wgs, snap="out")
raster_2007 <- crop(a2007, cuenca.wgs, snap="out") 
raster_2008 <- crop(a2008, cuenca.wgs, snap="out") 
raster_2009 <- crop(a2009, cuenca.wgs, snap="out") 
raster_2010 <- crop(a2010, cuenca.wgs, snap="out") 
raster_2011 <- crop(a2011, cuenca.wgs, snap="out") 
raster_2012 <- crop(a2012, cuenca.wgs, snap="out") 
raster_2013 <- crop(a2013, cuenca.wgs, snap="out") 
raster_2014 <- crop(a2014, cuenca.wgs, snap="out") 
raster_2015 <- crop(a2015, cuenca.wgs, snap="out") 
raster_2016 <- crop(a2016, cuenca.wgs, snap="out") 
raster_2017 <- crop(a2017, cuenca.wgs, snap="out") 
raster_2018 <- crop(a2018, cuenca.wgs, snap="out") 
raster_2019 <- crop(a2019, cuenca.wgs, snap="out") 
raster_2020 <- crop(a2020, cuenca.wgs, snap="out") 
raster_2021 <- crop(a2021, cuenca.wgs, snap="out") 
raster_2022 <- crop(a2022, cuenca.wgs, snap="out") 
raster_2023 <- crop(a2023, cuenca.wgs, snap="out") 


long_lat <- read.csv("Estaciones4.csv", header = T)
sp::coordinates(long_lat) <- ~XX+YY
raster::projection(long_lat) <- raster::projection(raster_1981)
points_long_lat <- raster::extract(raster_1981[[1]], long_lat, cellnumbers = T)[,1]
data_long_lat_1981 <- t(raster_1981[points_long_lat])
colnames(data_long_lat_1981) <- as.character(long_lat$NN)

long_lat <- read.csv("Estaciones4.csv", header = T)
sp::coordinates(long_lat) <- ~XX+YY
raster::projection(long_lat) <- raster::projection(raster_1982)
points_long_lat <- raster::extract(raster_1982[[1]], long_lat, cellnumbers = T)[,1]
data_long_lat_1982 <- t(raster_1982[points_long_lat])
colnames(data_long_lat_1982) <- as.character(long_lat$NN)

long_lat <- read.csv("Estaciones4.csv", header = T)
sp::coordinates(long_lat) <- ~XX+YY
raster::projection(long_lat) <- raster::projection(raster_1983)
points_long_lat <- raster::extract(raster_1983[[1]], long_lat, cellnumbers = T)[,1]
data_long_lat_1983 <- t(raster_1983[points_long_lat])
colnames(data_long_lat_1983) <- as.character(long_lat$NN)

long_lat <- read.csv("Estaciones4.csv", header = T)
sp::coordinates(long_lat) <- ~XX+YY
raster::projection(long_lat) <- raster::projection(raster_1984)
points_long_lat <- raster::extract(raster_1984[[1]], long_lat, cellnumbers = T)[,1]
data_long_lat_1984 <- t(raster_1984[points_long_lat])
colnames(data_long_lat_1984) <- as.character(long_lat$NN)

long_lat <- read.csv("Estaciones4.csv", header = T)
sp::coordinates(long_lat) <- ~XX+YY
raster::projection(long_lat) <- raster::projection(raster_1985)
points_long_lat <- raster::extract(raster_1985[[1]], long_lat, cellnumbers = T)[,1]
data_long_lat_1985 <- t(raster_1985[points_long_lat])
colnames(data_long_lat_1985) <- as.character(long_lat$NN)

long_lat <- read.csv("Estaciones4.csv", header = T)
sp::coordinates(long_lat) <- ~XX+YY
raster::projection(long_lat) <- raster::projection(raster_1986)
points_long_lat <- raster::extract(raster_1986[[1]], long_lat, cellnumbers = T)[,1]
data_long_lat_1986 <- t(raster_1986[points_long_lat])
colnames(data_long_lat_1986) <- as.character(long_lat$NN)

long_lat <- read.csv("Estaciones4.csv", header = T)
sp::coordinates(long_lat) <- ~XX+YY
raster::projection(long_lat) <- raster::projection(raster_1987)
points_long_lat <- raster::extract(raster_1987[[1]], long_lat, cellnumbers = T)[,1]
data_long_lat_1987 <- t(raster_1987[points_long_lat])
colnames(data_long_lat_1987) <- as.character(long_lat$NN)

long_lat <- read.csv("Estaciones4.csv", header = T)
sp::coordinates(long_lat) <- ~XX+YY
raster::projection(long_lat) <- raster::projection(raster_1988)
points_long_lat <- raster::extract(raster_1988[[1]], long_lat, cellnumbers = T)[,1]
data_long_lat_1988 <- t(raster_1988[points_long_lat])
colnames(data_long_lat_1988) <- as.character(long_lat$NN)

long_lat <- read.csv("Estaciones4.csv", header = T)
sp::coordinates(long_lat) <- ~XX+YY
raster::projection(long_lat) <- raster::projection(raster_1989)
points_long_lat <- raster::extract(raster_1989[[1]], long_lat, cellnumbers = T)[,1]
data_long_lat_1989 <- t(raster_1989[points_long_lat])
colnames(data_long_lat_1989) <- as.character(long_lat$NN)

long_lat <- read.csv("Estaciones4.csv", header = T)
sp::coordinates(long_lat) <- ~XX+YY
raster::projection(long_lat) <- raster::projection(raster_1990)
points_long_lat <- raster::extract(raster_1990[[1]], long_lat, cellnumbers = T)[,1]
data_long_lat_1990 <- t(raster_1990[points_long_lat])
colnames(data_long_lat_1990) <- as.character(long_lat$NN)

long_lat <- read.csv("Estaciones4.csv", header = T)
sp::coordinates(long_lat) <- ~XX+YY
raster::projection(long_lat) <- raster::projection(raster_1991)
points_long_lat <- raster::extract(raster_1991[[1]], long_lat, cellnumbers = T)[,1]
data_long_lat_1991 <- t(raster_1991[points_long_lat])
colnames(data_long_lat_1991) <- as.character(long_lat$NN)

long_lat <- read.csv("Estaciones4.csv", header = T)
sp::coordinates(long_lat) <- ~XX+YY
raster::projection(long_lat) <- raster::projection(raster_1992)
points_long_lat <- raster::extract(raster_1992[[1]], long_lat, cellnumbers = T)[,1]
data_long_lat_1992 <- t(raster_1992[points_long_lat])
colnames(data_long_lat_1992) <- as.character(long_lat$NN)

long_lat <- read.csv("Estaciones4.csv", header = T)
sp::coordinates(long_lat) <- ~XX+YY
raster::projection(long_lat) <- raster::projection(raster_1993)
points_long_lat <- raster::extract(raster_1993[[1]], long_lat, cellnumbers = T)[,1]
data_long_lat_1993 <- t(raster_1993[points_long_lat])
colnames(data_long_lat_1993) <- as.character(long_lat$NN)

long_lat <- read.csv("Estaciones4.csv", header = T)
sp::coordinates(long_lat) <- ~XX+YY
raster::projection(long_lat) <- raster::projection(raster_1994)
points_long_lat <- raster::extract(raster_1994[[1]], long_lat, cellnumbers = T)[,1]
data_long_lat_1994 <- t(raster_1994[points_long_lat])
colnames(data_long_lat_1994) <- as.character(long_lat$NN)

long_lat <- read.csv("Estaciones4.csv", header = T)
sp::coordinates(long_lat) <- ~XX+YY
raster::projection(long_lat) <- raster::projection(raster_1995)
points_long_lat <- raster::extract(raster_1995[[1]], long_lat, cellnumbers = T)[,1]
data_long_lat_1995 <- t(raster_1995[points_long_lat])
colnames(data_long_lat_1995) <- as.character(long_lat$NN)

long_lat <- read.csv("Estaciones4.csv", header = T)
sp::coordinates(long_lat) <- ~XX+YY
raster::projection(long_lat) <- raster::projection(raster_1996)
points_long_lat <- raster::extract(raster_1996[[1]], long_lat, cellnumbers = T)[,1]
data_long_lat_1996 <- t(raster_1996[points_long_lat])
colnames(data_long_lat_1996) <- as.character(long_lat$NN)

long_lat <- read.csv("Estaciones4.csv", header = T)
sp::coordinates(long_lat) <- ~XX+YY
raster::projection(long_lat) <- raster::projection(raster_1997)
points_long_lat <- raster::extract(raster_1997[[1]], long_lat, cellnumbers = T)[,1]
data_long_lat_1997 <- t(raster_1997[points_long_lat])
colnames(data_long_lat_1997) <- as.character(long_lat$NN)

long_lat <- read.csv("Estaciones4.csv", header = T)
sp::coordinates(long_lat) <- ~XX+YY
raster::projection(long_lat) <- raster::projection(raster_1998)
points_long_lat <- raster::extract(raster_1998[[1]], long_lat, cellnumbers = T)[,1]
data_long_lat_1998 <- t(raster_1998[points_long_lat])
colnames(data_long_lat_1998) <- as.character(long_lat$NN)

long_lat <- read.csv("Estaciones4.csv", header = T)
sp::coordinates(long_lat) <- ~XX+YY
raster::projection(long_lat) <- raster::projection(raster_1999)
points_long_lat <- raster::extract(raster_1999[[1]], long_lat, cellnumbers = T)[,1]
data_long_lat_1999 <- t(raster_1999[points_long_lat])
colnames(data_long_lat_1999) <- as.character(long_lat$NN)

long_lat <- read.csv("Estaciones4.csv", header = T)
sp::coordinates(long_lat) <- ~XX+YY
raster::projection(long_lat) <- raster::projection(raster_2000)
points_long_lat <- raster::extract(raster_2000[[1]], long_lat, cellnumbers = T)[,1]
data_long_lat_2000 <- t(raster_2000[points_long_lat])
colnames(data_long_lat_2000) <- as.character(long_lat$NN)

long_lat <- read.csv("Estaciones4.csv", header = T)
sp::coordinates(long_lat) <- ~XX+YY
raster::projection(long_lat) <- raster::projection(raster_2001)
points_long_lat <- raster::extract(raster_2001[[1]], long_lat, cellnumbers = T)[,1]
data_long_lat_2001 <- t(raster_2001[points_long_lat])
colnames(data_long_lat_2001) <- as.character(long_lat$NN)

long_lat <- read.csv("Estaciones4.csv", header = T)
sp::coordinates(long_lat) <- ~XX+YY
raster::projection(long_lat) <- raster::projection(raster_2002)
points_long_lat <- raster::extract(raster_2002[[1]], long_lat, cellnumbers = T)[,1]
data_long_lat_2002 <- t(raster_2002[points_long_lat])
colnames(data_long_lat_2002) <- as.character(long_lat$NN)

long_lat <- read.csv("Estaciones4.csv", header = T)
sp::coordinates(long_lat) <- ~XX+YY
raster::projection(long_lat) <- raster::projection(raster_2003)
points_long_lat <- raster::extract(raster_2003[[1]], long_lat, cellnumbers = T)[,1]
data_long_lat_2003 <- t(raster_2003[points_long_lat])
colnames(data_long_lat_2003) <- as.character(long_lat$NN)

long_lat <- read.csv("Estaciones4.csv", header = T)
sp::coordinates(long_lat) <- ~XX+YY
raster::projection(long_lat) <- raster::projection(raster_2004)
points_long_lat <- raster::extract(raster_2004[[1]], long_lat, cellnumbers = T)[,1]
data_long_lat_2004 <- t(raster_2004[points_long_lat])
colnames(data_long_lat_2004) <- as.character(long_lat$NN)

long_lat <- read.csv("Estaciones4.csv", header = T)
sp::coordinates(long_lat) <- ~XX+YY
raster::projection(long_lat) <- raster::projection(raster_2005)
points_long_lat <- raster::extract(raster_2005[[1]], long_lat, cellnumbers = T)[,1]
data_long_lat_2005 <- t(raster_2005[points_long_lat])
colnames(data_long_lat_2005) <- as.character(long_lat$NN)

long_lat <- read.csv("Estaciones4.csv", header = T)
sp::coordinates(long_lat) <- ~XX+YY
raster::projection(long_lat) <- raster::projection(raster_2006)
points_long_lat <- raster::extract(raster_2006[[1]], long_lat, cellnumbers = T)[,1]
data_long_lat_2006 <- t(raster_2006[points_long_lat])
colnames(data_long_lat_2006) <- as.character(long_lat$NN)

long_lat <- read.csv("Estaciones4.csv", header = T)
sp::coordinates(long_lat) <- ~XX+YY
raster::projection(long_lat) <- raster::projection(raster_2007)
points_long_lat <- raster::extract(raster_2007[[1]], long_lat, cellnumbers = T)[,1]
data_long_lat_2007 <- t(raster_2007[points_long_lat])
colnames(data_long_lat_2007) <- as.character(long_lat$NN)

long_lat <- read.csv("Estaciones4.csv", header = T)
sp::coordinates(long_lat) <- ~XX+YY
raster::projection(long_lat) <- raster::projection(raster_2008)
points_long_lat <- raster::extract(raster_2008[[1]], long_lat, cellnumbers = T)[,1]
data_long_lat_2008 <- t(raster_2008[points_long_lat])
colnames(data_long_lat_2008) <- as.character(long_lat$NN)

long_lat <- read.csv("Estaciones4.csv", header = T)
sp::coordinates(long_lat) <- ~XX+YY
raster::projection(long_lat) <- raster::projection(raster_2009)
points_long_lat <- raster::extract(raster_2009[[1]], long_lat, cellnumbers = T)[,1]
data_long_lat_2009 <- t(raster_2009[points_long_lat])
colnames(data_long_lat_2009) <- as.character(long_lat$NN)

long_lat <- read.csv("Estaciones4.csv", header = T)
sp::coordinates(long_lat) <- ~XX+YY
raster::projection(long_lat) <- raster::projection(raster_2010)
points_long_lat <- raster::extract(raster_2010[[1]], long_lat, cellnumbers = T)[,1]
data_long_lat_2010 <- t(raster_2010[points_long_lat])
colnames(data_long_lat_2010) <- as.character(long_lat$NN)

long_lat <- read.csv("Estaciones4.csv", header = T)
sp::coordinates(long_lat) <- ~XX+YY
raster::projection(long_lat) <- raster::projection(raster_2011)
points_long_lat <- raster::extract(raster_2011[[1]], long_lat, cellnumbers = T)[,1]
data_long_lat_2011 <- t(raster_2011[points_long_lat])
colnames(data_long_lat_2011) <- as.character(long_lat$NN)

long_lat <- read.csv("Estaciones4.csv", header = T)
sp::coordinates(long_lat) <- ~XX+YY
raster::projection(long_lat) <- raster::projection(raster_2012)
points_long_lat <- raster::extract(raster_2012[[1]], long_lat, cellnumbers = T)[,1]
data_long_lat_2012 <- t(raster_2012[points_long_lat])
colnames(data_long_lat_2012) <- as.character(long_lat$NN)

long_lat <- read.csv("Estaciones4.csv", header = T)
sp::coordinates(long_lat) <- ~XX+YY
raster::projection(long_lat) <- raster::projection(raster_2013)
points_long_lat <- raster::extract(raster_2013[[1]], long_lat, cellnumbers = T)[,1]
data_long_lat_2013 <- t(raster_2013[points_long_lat])
colnames(data_long_lat_2013) <- as.character(long_lat$NN)

long_lat <- read.csv("Estaciones4.csv", header = T)
sp::coordinates(long_lat) <- ~XX+YY
raster::projection(long_lat) <- raster::projection(raster_2014)
points_long_lat <- raster::extract(raster_2014[[1]], long_lat, cellnumbers = T)[,1]
data_long_lat_2014 <- t(raster_2014[points_long_lat])
colnames(data_long_lat_2014) <- as.character(long_lat$NN)

long_lat <- read.csv("Estaciones4.csv", header = T)
sp::coordinates(long_lat) <- ~XX+YY
raster::projection(long_lat) <- raster::projection(raster_2015)
points_long_lat <- raster::extract(raster_2015[[1]], long_lat, cellnumbers = T)[,1]
data_long_lat_2015 <- t(raster_2015[points_long_lat])
colnames(data_long_lat_2015) <- as.character(long_lat$NN)

long_lat <- read.csv("Estaciones4.csv", header = T)
sp::coordinates(long_lat) <- ~XX+YY
raster::projection(long_lat) <- raster::projection(raster_2016)
points_long_lat <- raster::extract(raster_2016[[1]], long_lat, cellnumbers = T)[,1]
data_long_lat_2016 <- t(raster_2016[points_long_lat])
colnames(data_long_lat_2016) <- as.character(long_lat$NN)

long_lat <- read.csv("Estaciones4.csv", header = T)
sp::coordinates(long_lat) <- ~XX+YY
raster::projection(long_lat) <- raster::projection(raster_2017)
points_long_lat <- raster::extract(raster_2017[[1]], long_lat, cellnumbers = T)[,1]
data_long_lat_2017 <- t(raster_2017[points_long_lat])
colnames(data_long_lat_2017) <- as.character(long_lat$NN)

long_lat <- read.csv("Estaciones4.csv", header = T)
sp::coordinates(long_lat) <- ~XX+YY
raster::projection(long_lat) <- raster::projection(raster_2018)
points_long_lat <- raster::extract(raster_2018[[1]], long_lat, cellnumbers = T)[,1]
data_long_lat_2018 <- t(raster_2018[points_long_lat])
colnames(data_long_lat_2018) <- as.character(long_lat$NN)

long_lat <- read.csv("Estaciones4.csv", header = T)
sp::coordinates(long_lat) <- ~XX+YY
raster::projection(long_lat) <- raster::projection(raster_2019)
points_long_lat <- raster::extract(raster_2019[[1]], long_lat, cellnumbers = T)[,1]
data_long_lat_2019 <- t(raster_2019[points_long_lat])
colnames(data_long_lat_2019) <- as.character(long_lat$NN)

long_lat <- read.csv("Estaciones4.csv", header = T)
sp::coordinates(long_lat) <- ~XX+YY
raster::projection(long_lat) <- raster::projection(raster_2020)
points_long_lat <- raster::extract(raster_2020[[1]], long_lat, cellnumbers = T)[,1]
data_long_lat_2020 <- t(raster_2020[points_long_lat])
colnames(data_long_lat_2020) <- as.character(long_lat$NN)

long_lat <- read.csv("Estaciones4.csv", header = T)
sp::coordinates(long_lat) <- ~XX+YY
raster::projection(long_lat) <- raster::projection(raster_2021)
points_long_lat <- raster::extract(raster_2021[[1]], long_lat, cellnumbers = T)[,1]
data_long_lat_2021 <- t(raster_2021[points_long_lat])
colnames(data_long_lat_2021) <- as.character(long_lat$NN)

long_lat <- read.csv("Estaciones4.csv", header = T)
sp::coordinates(long_lat) <- ~XX+YY
raster::projection(long_lat) <- raster::projection(raster_2022)
points_long_lat <- raster::extract(raster_2022[[1]], long_lat, cellnumbers = T)[,1]
data_long_lat_2022 <- t(raster_2022[points_long_lat])
colnames(data_long_lat_2022) <- as.character(long_lat$NN)

long_lat <- read.csv("Estaciones4.csv", header = T)
sp::coordinates(long_lat) <- ~XX+YY
raster::projection(long_lat) <- raster::projection(raster_2023)
points_long_lat <- raster::extract(raster_2023[[1]], long_lat, cellnumbers = T)[,1]
data_long_lat_2023 <- t(raster_2023[points_long_lat])
colnames(data_long_lat_2023) <- as.character(long_lat$NN)

write.csv(data_long_lat_1981, "1981.csv", quote = F)
write.csv(data_long_lat_1982, "1982.csv", quote = F)
write.csv(data_long_lat_1983, "1983.csv", quote = F)
write.csv(data_long_lat_1984, "1984.csv", quote = F)
write.csv(data_long_lat_1985, "1985.csv", quote = F)
write.csv(data_long_lat_1986, "1986.csv", quote = F)
write.csv(data_long_lat_1987, "1987.csv", quote = F)
write.csv(data_long_lat_1988, "1988.csv", quote = F)
write.csv(data_long_lat_1989, "1989.csv", quote = F)
write.csv(data_long_lat_1990, "1990.csv", quote = F)
write.csv(data_long_lat_1991, "1991.csv", quote = F)
write.csv(data_long_lat_1992, "1992.csv", quote = F)
write.csv(data_long_lat_1993, "1993.csv", quote = F)
write.csv(data_long_lat_1994, "1994.csv", quote = F)
write.csv(data_long_lat_1995, "1995.csv", quote = F)
write.csv(data_long_lat_1996, "1996.csv", quote = F)
write.csv(data_long_lat_1997, "1997.csv", quote = F)
write.csv(data_long_lat_1998, "1998.csv", quote = F)
write.csv(data_long_lat_1999, "1999.csv", quote = F)
write.csv(data_long_lat_2000, "2000.csv", quote = F)
write.csv(data_long_lat_2001, "2001.csv", quote = F)
write.csv(data_long_lat_2002, "2002.csv", quote = F)
write.csv(data_long_lat_2003, "2003.csv", quote = F)
write.csv(data_long_lat_2004, "2004.csv", quote = F)
write.csv(data_long_lat_2005, "2005.csv", quote = F)
write.csv(data_long_lat_2006, "2006.csv", quote = F)
write.csv(data_long_lat_2007, "2007.csv", quote = F)
write.csv(data_long_lat_2008, "2008.csv", quote = F)
write.csv(data_long_lat_2009, "2009.csv", quote = F)
write.csv(data_long_lat_2010, "2010.csv", quote = F)
write.csv(data_long_lat_2011, "2011.csv", quote = F)
write.csv(data_long_lat_2012, "2012.csv", quote = F)
write.csv(data_long_lat_2013, "2013.csv", quote = F)
write.csv(data_long_lat_2014, "2014.csv", quote = F)
write.csv(data_long_lat_2015, "2015.csv", quote = F)
write.csv(data_long_lat_2016, "2016.csv", quote = F)
write.csv(data_long_lat_2017, "2017.csv", quote = F)
write.csv(data_long_lat_2018, "2018.csv", quote = F)
write.csv(data_long_lat_2019, "2019.csv", quote = F)
write.csv(data_long_lat_2020, "2020.csv", quote = F)
write.csv(data_long_lat_2021, "2021.csv", quote = F)
write.csv(data_long_lat_2022, "2022.csv", quote = F)
write.csv(data_long_lat_2023, "2023.csv", quote = F)


pp1981 <- read.csv("1981.csv", header = T)
pp1982 <- read.csv("1982.csv", header = T)
pp1983 <- read.csv("1983.csv", header = T)
pp1984 <- read.csv("1984.csv", header = T)
pp1985 <- read.csv("1985.csv", header = T)
pp1986 <- read.csv("1986.csv", header = T)
pp1987 <- read.csv("1987.csv", header = T)
pp1988 <- read.csv("1988.csv", header = T)
pp1989 <- read.csv("1989.csv", header = T)
pp1990 <- read.csv("1990.csv", header = T)
pp1991 <- read.csv("1991.csv", header = T)
pp1992 <- read.csv("1992.csv", header = T)
pp1993 <- read.csv("1993.csv", header = T)
pp1994 <- read.csv("1994.csv", header = T)
pp1995 <- read.csv("1995.csv", header = T)
pp1996 <- read.csv("1996.csv", header = T)
pp1997 <- read.csv("1997.csv", header = T)
pp1998 <- read.csv("1998.csv", header = T)
pp1999 <- read.csv("1999.csv", header = T)
pp2000 <- read.csv("2000.csv", header = T)
pp2001 <- read.csv("2001.csv", header = T)
pp2002 <- read.csv("2002.csv", header = T)
pp2003 <- read.csv("2003.csv", header = T)
pp2004 <- read.csv("2004.csv", header = T)
pp2005 <- read.csv("2005.csv", header = T)
pp2006 <- read.csv("2006.csv", header = T)
pp2007 <- read.csv("2007.csv", header = T)
pp2008 <- read.csv("2008.csv", header = T)
pp2009 <- read.csv("2009.csv", header = T)
pp2010 <- read.csv("2010.csv", header = T)
pp2011 <- read.csv("2011.csv", header = T)
pp2012 <- read.csv("2012.csv", header = T)
pp2013 <- read.csv("2013.csv", header = T)
pp2014 <- read.csv("2014.csv", header = T)
pp2015 <- read.csv("2015.csv", header = T)
pp2016 <- read.csv("2016.csv", header = T)
pp2017 <- read.csv("2017.csv", header = T)
pp2018 <- read.csv("2018.csv", header = T)
pp2019 <- read.csv("2019.csv", header = T)
pp2020 <- read.csv("2020.csv", header = T)
pp2021 <- read.csv("2021.csv", header = T)
pp2022 <- read.csv("2022.csv", header = T)
pp2023 <- read.csv("2023.csv", header = T)

ppdiario81_23<-rbind(pp1981 ,pp1982 ,pp1983 ,pp1984 ,pp1985 ,pp1986 ,pp1987 ,pp1988 ,pp1989 ,pp1990 ,pp1991 ,pp1992 ,pp1993 ,pp1994 ,pp1995 ,pp1996 ,pp1997 ,pp1998 ,pp1999 ,pp2000 ,pp2001 ,pp2002 ,pp2003,pp2004 ,pp2005 ,pp2006 ,pp2007 ,pp2008 ,pp2009 ,pp2010 ,pp2011 ,pp2012 ,pp2013 ,pp2014 ,pp2015 ,pp2016 ,pp2017 ,pp2018 ,pp2019 ,pp2020 ,pp2021 ,pp2022 ,pp2023)

write.csv(ppdiario81_23, "1981_2023.csv", quote = F)

ppdiario81_23 <- read.csv("1981_2023.csv", header = T)

ppdiario81_23$X <- as.POSIXct(gsub("X", "", ppdiario81_23$X), format = "%Y.%m.%d")

names (ppdiario81_23)[1]="Fecha"

write.csv(ppdiario81_23, "1981_2023.csv", quote = F)

pptotal <- read.csv("1981_2023.csv", header = T)

lagunaramon<-pptotal[,3]
Chusis<-pptotal[,4]
Montegrande<-pptotal[,5]
Lasmonjas<-pptotal[,6]
Miraflores<-pptotal[,7]
Tablazo<-pptotal[,8]
Hualtaco<-pptotal[,9]
bernal<-pptotal[,10]
Sanmiguel<-pptotal[,11]
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

xyplot(prep1, main="Precipitación de la estación Laguna Ramón ", xlab="fecha", ylab="Precipitación [mm/día]",ylim = c(0,max1))
xyplot(prep4, main="Precipitación de la estación Qba. Las monjas ", xlab="fecha", ylab="Precipitación [mm/día]",ylim = c(0,max4))
xyplot(prep2, main="Precipitación de la estación Chusis ", xlab="fecha", ylab="Precipitación [mm/día]",ylim = c(0,max2))
xyplot(prep3, main="Precipitación de la estación Montegrande ", xlab="fecha", ylab="Precipitación [mm/día]",ylim = c(0,max3))
xyplot(prep5, main="Precipitación de la estación Miraflores ", xlab="fecha", ylab="Precipitación [mm/día]",ylim = c(0,max5))
xyplot(prep6, main="Precipitación de la estación El tablazo ", xlab="fecha", ylab="Precipitación [mm/día]",ylim = c(0,max6))
xyplot(prep7, main="Precipitación de la estación Hualtaco ", xlab="fecha", ylab="Precipitación [mm/día]",ylim = c(0,max7))
xyplot(prep8, main="Precipitación de la estación Bernal ", xlab="fecha", ylab="Precipitación [mm/día]",ylim = c(0,max8))
xyplot(prep9, main="Precipitación de la estación San Miguel ", xlab="fecha", ylab="Precipitación [mm/día]",ylim = c(0,max9))



