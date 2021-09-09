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

#====================== Leaflet================================#
library(leaflet)
library(RColorBrewer)
library(htmltools)

pal <- colorNumeric("YlOrRd", domain = shp$CrudeRate)
labels <- 
  sprintf(
    "<strong>%s</strong><br/> Case Rate: %s per 100k",
    shp$ZCTA5CE10,shp$CrudeRate) %>% 
  lapply(htmltools::HTML)


leaflet(shp) %>%
 # setView(-96, 37.8, 4) %>%
  addProviderTiles("CartoDB.PositronNoLabels") %>%
  addPolygons(
    fillColor = ~pal(CrudeRate), 
    weight = 1,
    opacity = 1,
    color = "white",
    fillOpacity = 0.7,
    highlight = highlightOptions(
      weight = 2,
      color = "#666",
      fillOpacity = 0.7,
      bringToFront = TRUE),
    label = labels,
    labelOptions = labelOptions(
      style = list("font-weight" = "normal"),
      textsize = "15px",
      direction = "auto")) %>% 
  addLegend(pal = pal,
            values = shp$CrudeRate,
            position = "bottomright",
            title = "COVID-19 Case Rate",
            labFormat = labelFormat(suffix = ""),
            opacity = 0.8,
            na.label = "NA")




