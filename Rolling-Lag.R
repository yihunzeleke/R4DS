
library(tidyverse)
library(lubridate)
library(plotly)
theme_set( hrbrthemes::theme_modern_rc())

covid <- readr::read_csv("https://data.kcmo.org/api/views/nfta-sjx6/rows.csv?accessType=DOWNLOAD")

df_clean <- covid %>% janitor::clean_names() %>% 
  mutate(date = mdy_hms(date)) %>% 
  mutate(YearMonth = format(date, "%Y-%m"),
         Week = floor_date(date, unit = 'weeks'),
         Day = day(date))

#==# Lag month

monthdata <-  df_clean %>% 
  group_by(YearMonth) %>% 
  summarise(Cases = sum(new_cases)) %>% 
  arrange(YearMonth) %>% 
  mutate(
    M_over_M = (Cases -lag(Cases)) / lag(Cases)
  ) 


monthdata %>% 
  ggplot(aes(YearMonth, M_over_M, fill = M_over_M > 0))+
  geom_col(show.legend = F, width = 0.75)

#==# covert % to comparison
monthdata <- monthdata %>% 
  mutate(M_over_M = round(M_over_M*100,1))

  
weekdata <-  df_clean%>% 
  group_by(Week) %>% 
  summarise(week_cases = sum(new_cases)) %>% 
  arrange(desc(Week)) %>%
  mutate(WeekCaseChange = (week_cases - lag(week_cases))/lag(week_cases)) %>% ungroup()

weekdata %>% 
  ggplot(aes(Week, WeekCaseChange, fill = WeekCaseChange > 0))+
  geom_col(show.legend = F)
 

#==# two week lag

two_weekdata <-df_clean%>% 
  group_by(Week) %>% 
  summarise(week_cases = sum(new_cases)) %>% 
  arrange(desc(Week)) %>%
  mutate(WeekCaseChange = (week_cases - lag(week_cases,2))/lag(week_cases,2)) %>% ungroup()

two_weekdata %>% 
  ggplot(aes(Week, WeekCaseChange, fill = WeekCaseChange > 0))+
  geom_col(show.legend = F)

#==# for month

df_clean%>% 
  group_by(Week) %>% 
  summarise(week_cases = sum(new_cases)) %>% 
  arrange(desc(Week)) %>%
  mutate(WeekCaseChange = (week_cases - lag(week_cases,4))/lag(week_cases,4)) %>% ungroup() %>% 
  ggplot(aes(Week, WeekCaseChange, fill = WeekCaseChange > 0))+
  geom_col(show.legend = F)






