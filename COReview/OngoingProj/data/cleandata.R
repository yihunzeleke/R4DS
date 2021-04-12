
library(tidyverse)
library(lubridate)

cases <- readr::read_csv("C:/Users/yihun/Documents/COReview/OngoingProj/data/DailyCOVID19.csv") %>% 
  janitor::clean_names()

cases_zipcode <- readr::read_csv("C:/Users/yihun/Documents/COReview/OngoingProj/data/CasesByZipCode.csv") %>% 
  janitor::clean_names()



zip_code <- sf::read_sf("C:/Users/yihun/Documents/COReview/OngoingProj/data/zipcode_pop/KCMOZIPCODE.shp")


# data cleaning 

cases <- cases %>% 
  mutate(date = ymd(as.Date(date, "%m/%d/%Y")))

cases_zipcode <- cases_zipcode %>% 
  mutate(zip_code = as.character(zip_code))

KCMO <- zip_code %>% 
  full_join(cases_zipcode, by = c("ZCTA5CE10" = "zip_code"))


KCMO <- KCMO %>% 
  mutate(`Positivity Rate` = round(cases/total_residents_tested*100, digits = 1)) %>% 
  rename(`Case Rates` = crude_rate_per_100_000) %>% 
  mutate(id_case = glue::glue("ZIP Code: {ZCTA5CE10} Cases: {cases} Cases: {cases}"),
         id_case_rate =  glue::glue("ZIP Code: {ZCTA5CE10} Cases: {cases} Case Rate: {`Case Rates`}"),
         id_positive_rate = glue::glue("ZIP Code: {ZCTA5CE10} Cases: {cases} Total Resident Tested: {`Positivity Rate`}")
         )
  
KCMO$lable <-  with(KCMO, paste(
  "<p> <b>","ZIP Code:", ZCTA5CE10 , "</b> </br>",
  "Cases:",cases , "</br>",
  "Total Resident Tested:", total_residents_tested,
  "</p>"))
