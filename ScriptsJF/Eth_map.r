#----------------------------------------------------------------------------------------------------------
#http://stackoverflow.com/questions/17723822/administrative-regions-map-of-a-country-with-ggmap-and-ggplot2
#----------------------------------------------------------------------------------------------------------
library(ggplot2)
library(dplyr)
library(rgdal)
library(maptools)
library(rgeos)
library(mapproj)
library(maps)
library(data.table)
library(xlsx)

#-------------------------
#Change in technology
tech2011 <- data.frame(household_id = data2011$household_id, tech2011 = data2011$tech)
technologydifference2013 <- left_join(data2013, tech2011)
technologydifference2013$techdiff <- technologydifference2013$tech - technologydifference2013$tech2011
#-------------------------





emap <- function(mapdata, variable){
#Loading excel file to align the zone names with the spatial polygon data from GADM
regionzones <- read.xlsx("C:/Users/flori007/Desktop/regionzones.xlsx", sheetName = 'Sheet1')
names(regionzones) <- c("region", "zone", "name")
mapdata <- left_join(mapdata, regionzones, by=c("region", "zone"))

load("C:/Users/flori007/Desktop/ETH_adm2.RData")
ethiopia.adm2.spdf <- get('gadm')
ethiopia.adm2.df <- fortify(ethiopia.adm2.spdf, region = "NAME_2")

by_zone <- data.frame(household_id = mapdata$household_id,
                      region       = mapdata$region, 
                      zone         = mapdata$zone, 
                      name         = mapdata$name,
                      var          = mapdata[,variable] )
by_zone <- group_by(by_zone, id=name)    

mrge <- data.frame(summarise(by_zone,
                                var = mean(var, na.rm=T)))

var.df <- data.frame(id= unique(ethiopia.adm2.df[,'id']))
var.df <- left_join(var.df, mrge)

ethiopia.adm2.df <- merge(ethiopia.adm2.df, var.df, by.y = 'id', all.x = TRUE)

ethiopia.adm2.centroids.df <- data.frame(long = coordinates(ethiopia.adm2.spdf)[, 1], 
                                         lat = coordinates(ethiopia.adm2.spdf)[, 2])

ethiopia.adm2.centroids.df[, 'ID_1'] <- ethiopia.adm2.spdf@data[,'ID_1']
ethiopia.adm2.centroids.df[, 'NAME_2'] <- ethiopia.adm2.spdf@data[,'NAME_2']

p <- ggplot(ethiopia.adm2.df, aes(x = long, y = lat, group = group)) + geom_polygon(aes(fill = cut(var,9,)), colour="black") +
  #geom_text(data = ethiopia.adm2.centroids.df, aes(label = NAME_2, x = long, y = lat, group = NAME_2), size = 2) + 
  labs(x=" ", y=" ") + 
  theme_bw() + scale_fill_brewer(variable, palette  = 'RdYlGn', na.value="grey") + 
  coord_map() + 
  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank()) #+ 
  theme(axis.ticks = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank()) + 
  theme(panel.border = element_blank())

rm()    
p
}







