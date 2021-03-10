library(tidyverse)
library(tmap)
library(tmaptools)
library(osmdata)
library(osmplotr)
library(sf)
kcmoshp <- sf::read_sf("KCMOZIPCODE/KCMOZIPCODE.shp")


full_data <- kcmoshp %>% 
  full_join(LifeExp, by = c("ZCTA5CE10" = "ZIP-Code")) %>% 
  mutate(LifeExp = ifelse(LifeExp == 0, NA, LifeExp))

tmap_mode("view")

tm_shape(full_data) +
  tm_polygons("LifeExp")

tm_basemap(server = "OpenStreetMap.HOT")+
  tm_shape(full_data)+
  tm_polygons(c("LifeExp"), style = "jenks", n = 5,
              title = "Total Cases",
              palette = "Greys", main = "Title")+
  tm_text("ZCTA5CE10", size = 0.5)+
  tm_layout(legend.outside = F,  legend.just = "center")





