
#==# Installing mapsf package 
# install.packages("mapsf")

#==# Dependencies

# remotes::install_github("riatelab/mapsf")
# devtools::install_github("r-spatial/sf")
# install.packages("rgdal")

# Loading libraries

library(tidyverse)
library(mapsf)

mtq <- mf_get_mtq()
str(mtq)

shp <- sf::read_sf('MO-shp/tl_2019_29_cousub.shp')

shp_kcmo <- sf::read_sf("KCMO-NH/KCMOZIPCODE.shp") %>% 
  mutate(ZCTA5CE10 = as.character(ZCTA5CE10))

data <- readr::read_csv("https://data.kcmo.org/api/views/374j-h7xt/rows.csv?accessType=DOWNLOAD")

kcmo_data <- data %>% 
  janitor::clean_names()

kcmo_data <- kcmo_data %>% 
  mutate(zip_code = as.character(zip_code)) %>% 
  mutate(Rate = ifelse(str_detect(crude_rate_per_100_000, "^Not"), NA, crude_rate_per_100_000),
         TwoWeekTotalCases = ifelse(str_detect(two_week_total_cases, "SUPP"), NA, two_week_total_cases),
         TwoWeekCaseRate = ifelse(str_detect(two_week_total_tested, "SUPP"), NA, two_week_total_tested))

shp_kcmo_data <- shp_kcmo %>% 
  full_join(kcmo_data, by = c('ZCTA5CE10' = 'zip_code')) %>% 
  select(-c(two_week_total_cases, two_week_total_tested, crude_rate_per_100_000)) %>% 
  relocate(c(cases, total_residents_tested,Rate,TwoWeekCaseRate,TwoWeekTotalCases), .after=ZCTA5CE10) %>% 
  mutate(Rate = as.numeric(Rate),
         TwoWeekTotalCases = as.numeric(TwoWeekTotalCases),
         TwoWeekCaseRate = as.numeric(TwoWeekCaseRate))

nmf_init(x = mtq, theme = 'iceberg')
mf_shadow(mtq, add = TRUE)

mf_map(mtq, type = "base", add = TRUE)
mf_layout(title = "Martinique", credits = paste0("Sources: ING, 2018\n","mapsf",
                                                 packageVersion("mapsf")))
# Missouri Counties 

mf_init(x = shp, theme ="iceberg")
mf_shadow(shp, add = T)
# mf_map(shp, type = "base", add = TRUE)
mf_map(
  x = shp, 
  var = "AWATER",
  type = "prop",
  inches = 0.25,
  col = "brown4",
  leg_pos = 'bottomleft2',
  leg_title = "Census Population")
mf_layout(title = "Missouri")

# KCMO are Counties 

mf_theme("darkula")

mf_map(
  x = shp_kcmo_data, 
  var = "cases",
  type = "choro",
  col = "brown4",
  leg_pos = 'topright2',
  leg_title = "COVID-19 Cases")
mf_layout(title = "KCMO Area Neighborhood", credits = paste0("Source: KCMO OpenData"))


#==# Corophlete

mf_theme("darkula")

mf_map(
  x = shp_kcmo_data, 
  var = "TwoWeekCaseRate",
  type = "choro",
  breaks = "geom",
  nbreaks = 4,
  pal = "Oranges",
  border = "white", 
  lwd = 0.75,
  leg_pos = "topright2", 
  leg_title = "Census Population")
mf_layout(title = "KCMO Area Neighborhood", credits = paste0("Source: KCMO OpenData"))

#==# Proportional 


# import the sample data set
# set theme
mf_init(x = shp_kcmo_data, theme = "candy", expandBB = c(0,0,0,.15))
# Plot a shadow
mf_shadow(shp_kcmo_data, add = TRUE)
# Plot the municipalities
mf_map(shp_kcmo_data, add = TRUE)
# Plot symbols with choropleth coloration
mf_map(
  x = shp_kcmo_data, 
  var = c("total_residents_tested", "Rate"), 
  type = "prop_choro",
  border = "grey50",
  lwd = 1,
  leg_pos = c("topright", "right"), 
  leg_title= c("Total Resident Tested","Case Rate \nper(100K)"),
  breaks = "equal", 
  nbreaks = 3, 
  pal = "Greens",
  leg_val_rnd = c(0, -2), 
  leg_frame = c(TRUE, TRUE)) 
# layout
mf_layout(title = "COVDI-19 Cases, 2021", 
          credits = paste0("Sources: KCHD, 2021\n",
                           "mapsf ", 
                           packageVersion("mapsf")), 
          frame = TRUE)


#==# Typology Map






