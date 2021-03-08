library(tidyverse)
library(tmap)
library(tmaptools)
library(osmdata)
library(osmplotr)
library(sf)
kcmoshp <- sf::read_sf("KCMOZIPCODE/KCMOZIPCODE.shp")


full_data <- kcmoshp %>% 
  full_join(covid, by = c("ZCTA5CE10" = "ZipCode"))

tmap_mode("view")

tm_shape(full_data) +
  tm_polygons("Cases")

tm_basemap(server = "OpenStreetMap.HOT")+
  tm_shape(full_data) +
  tm_polygons(c("Cases"), style = "jenks", n = 3,
              popup.vars = c("Cases",
                             "Crude Rate per 100,000",
                             "Total Residents Tested",
                             "Two-Week Total Tested" ,
                             "Two-Week Total Cases"),
              title = "Total Cases",
              palette = "YlGnBu", contrast = 1)


