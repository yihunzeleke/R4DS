library(plotly)
library(sf)
library(dplyr)

COVID19_ZIP <- read_csv("COVID19_ZIP.csv") %>% janitor::clean_names() 

COVID19_ZIP <- COVID19_ZIP %>% 
  mutate(zip_code = as.character(zip_code)) %>% 
  mutate(hover = paste0(zip_code, "\n$", cases, total_residents_tested,
                        crude_rate_per_100_000,two_week_total_cases,two_week_total_tested))

kcmo <- sf::st_read("KCMOZIPCODE/KCMOZIPCODE.shp", quiet = TRUE)

kcmo_joined <- kcmo %>% 
  full_join(COVID19_ZIP, by = c("ZCTA5CE10"="zip_code"))

fig <- plot_geo(nc)

fig


"https://api.mapbox.com/{endpoint}?access_token={pk.eyJ1IjoieWFuYXNoZWciLCJhIjoiY2tqejcyYXFoMDJhcTJ2cWp3MTNkeTE4ZyJ9.EEy4P2uLWMZ6UbVNn9K2Tg}
"
mapboxToken <- paste(readLines("../.https://api.mapbox.com/{endpoint}?access_token={pk.eyJ1IjoieWFuYXNoZWciLCJhIjoiY2tqejcyYXFoMDJhcTJ2cWp3MTNkeTE4ZyJ9.EEy4P2uLWMZ6UbVNn9K2Tg}
"), collapse="")    # You need your own token
Sys.setenv("MAPBOX_TOKEN" = mapboxToken) # for Orca

mapboxToken <- paste("pk.eyJ1IjoieWFuYXNoZWciLCJhIjoiY2tqejcyYXFoMDJhcTJ2cWp3MTNkeTE4ZyJ9.EEy4P2uLWMZ6UbVNn9K2Tg", collapse="")    # You need your own token

Sys.setenv("MAPBOX_TOKEN" = mapboxToken) # for Orca

plot_ly(kcmo_joined, split = ~GEOID10)

plot_ly(
  kcmo_joined, 
  split = ~GEOID10,
  color = ~cases,
  alpha = 1,
  showlegend = FALSE
)


plot_ly(
  kcmo_joined,
  split = ~GEOID10,
  color = ~ZCTA5CE10,
  stroke = I("black"),
  text = ~paste(ZCTA5CE10, "\n is in", ZCTA5CE10), 
  hoverinfo = "text",
  hoveron = "fill"
)

styles <- schema()$layout$layoutAttributes$mapbox$style$values
styles
# generate plot.js buttons, one for every style 
style_buttons <- lapply(styles, function(s) {
  list(label = s, method = "relayout", args = list("mapbox.style", s))
})

plot_mapbox(kcmo_joined, color = I("black")) %>%
  layout(
    title = "KCMO COVID-19 Cases",
    mapbox = list(style = "satellite-streets"),
    updatemenus = list(list(y = 0.8, buttons = rev(style_buttons)))
  )








