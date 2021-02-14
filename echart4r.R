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

covid_cases <- covid_cases %>% 
  mutate(new_date = mdy_hms(Date))

covid_cases %>% 
  e_chart(x = Date) %>% 
  e_line(serie = `New Cases`) %>% 
  
#==# adding area plot
  e_area(`Total Tested`) %>% 
  e_axis_labels(x = "Date") %>% 
  e_title("COVID-19 Cases in KCMO as of Feb 02-2021") %>% 
  e_legend(right = 0) %>% 
  e_tooltip(trigger = "axis")








