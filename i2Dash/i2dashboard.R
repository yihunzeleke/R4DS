library(i2dash)
library(leaflet)
library(rgdal)


dashboard <- i2dashboard(
  title = "My Dashboar",
  author = "Yihun Zeleke",
  interactive = TRUE,
  theme = "spacelab")

dashboard %<>%
  add_page(
    page = "page1",
    title = "First page",
    layout = "focal_left",
    menu = NULL
  ) %>% 
  add_page(
    page = "page2",
    title = "About me",
    layout = "focal_left",
    menu = "Menu",
  ) %>% 
  add_page(
    page = "page3",
    title = "Contact me",
    layout = "focal_left",
    menu = "Menu"
  )


address_point <- leaflet(data = Pothole_Tracker_2021) %>% 
  addTiles() %>% 
  addMarkers(lng = ~LONGITUDE, lat = ~LATITUDE, popup = ~as.character(`ZIP CODE`), label =  ~as.character(`ZIP CODE`) )
