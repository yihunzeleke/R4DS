# install.packages("remotes")
# remotes::install_github("JohnCoene/echarts4r")
# remotes::install_github('JohnCoene/echarts4r.maps')
# remotes::install_github('JohnCoene/echarts4r.assets')

library(dplyr)
library(readr)
library(ggplot2)
library(lubridate)
library(echarts4r)

covid_cases <- read_csv("https://data.kcmo.org/api/views/nfta-sjx6/rows.csv?accessType=DOWNLOAD")
house_price <- read_csv('https://files.zillowstatic.com/research/public_v2/zhvi/Metro_zhvi_uc_sfrcondo_tier_0.33_0.67_sm_sa_mon.csv')

covid_cases <- covid_cases %>% 
  mutate(new_date = mdy_hms(Date))

covid_cases %>% 
  e_chart(x = new_date) %>% 
  e_line(serie = `New Cases`) %>% 
  
#==# adding area plot
  e_area(`Total Tested`) %>% 
  e_axis_labels(x = "Date") %>% 
  e_title("COVID-19 Cases in KCMO as of Feb 02-2021") %>% 
  e_legend(right = 0) %>% 
  e_tooltip(trigger = "axis")

#==# customize theme
p <- covid_cases %>% 
  e_chart(x =new_date ) %>% 
  e_line(serie = `New Cases`) %>% 
  e_line(`Total Tested`) %>% 
  e_axis_labels(x = "Date") %>% 
  e_title("COVID-19 Cases in KCMO as of Feb 02-2021") %>% 
  e_legend(right = 0) %>% 
  e_tooltip(trigger = "axis")

p %>% 
  e_theme("bee-inspired")

#==# customize theme from https://echarts.apache.org/en/theme-builder.html website 
p %>% 
  e_theme_custom("fav-theme.json") %>% 
  # customize background color
  e_color(background = "#ffffff")


#==# customize line color
line_color <-  c("darkblue", "#03925e")

p %>% 
  e_color(line_color)


#==# working with RColorbrewer

library(RColorBrewer)

mycolors <- brewer.pal(3,"Accent")
mycolors

p %>% 
  e_color(mycolors)

#==# color palettes
library(paletteer)

paletteer_d("ggthemes::Color_Blind")

#==# save one of theme
pa_color <- paletteer_d("ggthemes::Color_Blind")[c(1,10)]

p %>% 
  e_color(pa_color)


covid_cases <- covid_cases %>% 
  mutate(Mon = month(new_date, label = T))%>% 
  group_by(Mon) %>% ungroup()
  
covid_cases %>% 
  e_charts(x = Mon) %>% 
  e_line(serie = `New Cases`) %>% 
  e_tooltip()










