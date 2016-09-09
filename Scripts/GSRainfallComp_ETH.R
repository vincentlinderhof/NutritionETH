# PROJECT: IPOP/CIMMYT/DFID
# ``````````````````````````````````````````````````````````````````````````````````````````````````
# ``````````````````````````````````````````````````````````````````````````````````````````````````
# R code to compare rainfall with crop calendar and growth season data
# ``````````````````````````````````````````````````````````````````````````````````````````````````
# `````````````````````````````````````````````````````````````````````````````````````````````````` 

# INSTALL PACKAGES AND SET WORKING DIRECTORY
BasePackages <- c("foreign", "stringr", "gdata", "car", "reshape2", "RColorBrewer", "plyr", "dplyr", "tidyr")
lapply(BasePackages, library, character.only = TRUE)
SpatialPackages <- c("rgdal", "gdalUtils", "ggmap", "raster", "rasterVis", "rgeos", "sp", "mapproj", "maptools", "proj4")
lapply(SpatialPackages, library, character.only = TRUE)
AdditionalPackages <- c("lubridate", "GSIF", "SPEI")
lapply(AdditionalPackages, library, character.only = TRUE)

# OPTIONS
options(scipen = 999) # surpress scientific notation
options("stringsAsFactors" = FALSE) 
options(digits = 4)

# SET WORKING DIRECTORY
wdPath <- "D:\\Dropbox\\Michiel_research\\2285000066 Africa Maize Yield Gap\\Analysis"
setwd(wdPath)


# SOURCE FUNCTIONS

# SET COUNTRY AND YEAR
iso3c <- "GHA"

# Obtain country coordinates for target country
Get.country.shapefile.f <- function(iso3c, lev=0, proj = "+proj=longlat +datum=WGS84"){
  
  #download country boundary as spatialpolygonDF (and rewrite as .shp for convenience)
  targetfile <- paste(iso3c, paste("_adm", lev, ".Rdata", sep=""), sep="")
  if(file.exists(paste(basemapPath, targetfile, sep="/"))){
    load(paste(basemapPath, targetfile, sep="/"))
  } else {
    gadm=getData('GADM', country=iso3c, level=lev, path=basemapPath)
  }
  
  # change projection 
  projection <- proj
  country.sp <- spTransform(gadm, CRS(projection))
  return(country.sp)
}

# Download Basemap
basemapPath = paste(wdPath, iso3c, "Data", "Basemap", sep="\\") 
if (!file.exists(basemapPath)) dir.create(path = basemapPath)
country.map <- Get.country.shapefile.f(iso3c, 2)

# PREPARE LSMS SPATIAL DATAPOINTS

# SET TARGET REGION COORDINATES
# Ghana, Northern Region, Savelugu-Nanton, around Tamale, 9.41o lat, -0.85o lon;
# Ghana, Brong Ahafo, Nkoranza, around Nkoranza, 7.56o lat, -1.71o lon;
# Ethiopia, Oromia, Baco, around Baco, 9.12o lat, 37.06o lon;
# Ethiopia, SNNPR, Awassa, around Awassa, 7.05o lat, 38.50o lon => now changed to Adami Tullu.

geo.base <- data.frame(Region = c("Tamale", "Nkoranza",  "Baco", "Awasa"), 
                       lon= c(-0.85, -1.71, 37.06, 38.5) , lat = c(9.41, 7.56, 9.12, 7.05))
# Create spatial points 
standardproj<-"+proj=longlat +datum=WGS84"
geo.coord <- geo.base %>% 
  dplyr::select(lon, lat) %>%
  SpatialPoints(., proj4string=CRS(standardproj))

# Check location
# Does NOT work in explorer => set default browser to Firefox
library(plotGoogleMaps)
geo.check.google <- spTransform(geo.coord, CRS('+init=epsg:28992'))
m <- plotGoogleMaps(geo.check.google)


# REGION INFORMATION
geo.region  <- over(geo.coord, country.map) %>%
  cbind(geo.base,.) %>%
  dplyr::select(-PID, -ID_0, -ISO, -NAME_0, -NL_NAME_2, -VARNAME_2, -TYPE_2, -ENGTYPE_2)  


# GROWING SEASON
GrowingSeasonPath <- "D:\\Data\\IPOP\\GrowingSeason\\Processed"
stwka <-raster(paste(GrowingSeasonPath, "stwka", sep="\\"))
endwka <-raster(paste(GrowingSeasonPath, "endwka", sep="\\"))

# Extract data
# Note that the extent is slightly different between stwka and endwka so we extract data separately
geo.stwka <- raster::extract(stwka, geo.coord) %>%
  cbind(geo.base,.) 
names(geo.stwka)[4] <- "stwka"

geo.endwka <- raster::extract(endwka, geo.coord) %>%
  cbind(geo.base,.) 
names(geo.endwka)[4] <- "endwka"

geo.wka <- left_join(geo.stwka, geo.endwka) %>%
  mutate(stday = stwka*7,
         endday = endwka*7)

# check missing information
plot(crop(endwka, country.map))
plot(country.map, add=T)
plot(geo.coord, add=T, pch = 18, col="red")

# CROP CALENDAR
CropCalendarPath <- "D:\\Data\\IPOP\\CropCalendar\\Processed"
cc <-stack(paste(CropCalendarPath, "CropCalendar", sep="\\"))

# Extract data
geo.cc <- raster::extract(cc, geo.coord) %>%
  cbind(geo.base,.) %>%
  rename(start_planting = Maize..Start.of.planting, end_planting = Maize..End.of.planting,
         start_harvest = Maize..Start.of.harvest, end_harvest = Maize..End.of.harvest)
#%>%
# gather(cc, day, -ea_id, -lon, -lat) 

# MONTHLY RAINFALL DATA
rainfallPath2 <- "D:\\Data\\IPOP\\CRU_TS_3.22\\Processed"
pet <-brick(paste(rainfallPath2, "MonthlyEvapotranspiration201101_201312", sep="\\"))
pre <-brick(paste(rainfallPath2, "MonthlyPrecipitation201101_201312", sep="\\"))

# extract data
geo.pet <- raster::extract(pet, geo.coord) %>%
  cbind(geo.base,.) %>% 
  gather(date, pet, X2001.01.16:X2013.12.16)

geo.pre <- raster::extract(pre, geo.coord) %>%
  cbind(geo.base,.) %>% 
  gather(date, pre, X2001.01.16:X2013.12.16) %>%
  mutate(date = gsub("X","", date), 
         date = ymd(date),
         year = year(date),
         month = month(date, label=TRUE),
         day = day(date),
         start = ymd(paste(year, 01, 01, sep="-")),
         span = as.integer(date-start))
         

# DAILY RAINFALL DATA
# Extract rainfall data and link to GIS coordinates of plots/housholds
rainfallPath <- "D:\\Data\\IPOP\\Rainfall\\Processed"
rainfall <-brick(paste(rainfallPath, "DailyRainfall20110301_20110531", sep="\\"))

# extract data
geo.rainfall <- raster::extract(rainfall, geo.coord) %>%
  cbind(geo.base,.) %>%
  gather(date, rain, Date_2011_3_1:Date_2011_5_31) %>%
  mutate(date = gsub("Date_","", date), 
         date = ymd(date),
         year = year(date),
         month = month(date, label=TRUE),
         day = day(date))

# BIND ALL SPATIAL INFORMATION
# Compare CropCalendar and GrowingSeason wih rainfall data
# Plot rainfall and dates for selected regions (one of each type)

geo.comp <- left_join(geo.base, geo.pre) %>% left_join(., geo.cc) %>% left_join(., geo.wka) 

ggplot(geo.comp) + geom_line(aes(x = span, y = pre, group = year, colour=factor(year))) + 
                    facet_wrap(~Region) +
 
                    
                    geom_vline(aes(xintercept=start_planting)) +
                    geom_text(aes(x=start_planting,y=Inf,label = "start_planting"), vjust = 1, size=3) +
                    
                    geom_vline(aes(xintercept=end_planting)) +
                    geom_text(aes(x=end_planting,y=Inf,label = "end_planting"), vjust = 1, size=3) +
                    
                    geom_vline(aes(xintercept=start_harvest)) +
                    geom_text(aes(x=start_harvest,y=Inf,label = "start_harvest"), vjust = 1, size=3) + 
                  
                    geom_vline(aes(xintercept=end_harvest)) +
                    geom_text(aes(x=end_harvest,y=Inf,label = "end_harvest"), vjust = 1, size=3) 
#                     
#                     geom_vline(aes(xintercept=stday), colour="green") +
#                     geom_text(aes(x=stday, y=Inf,label = "start gs"), vjust = 1, size=3) +
#                     
#                     geom_vline(aes(xintercept=endday), colour="red") +
#                     geom_text(aes(x=endday,y=Inf,label = "end gs"), vjust = 1, size=3)
#                   