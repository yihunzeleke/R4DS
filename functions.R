
library(dplyr)
library(readr)
library(lubridate)
cvid_cases <- read_csv("https://data.kcmo.org/api/views/nfta-sjx6/rows.csv?accessType=DOWNLOAD")

cvid_cases <- cvid_cases %>% 
  mutate(new_date = mdy_hms(date))


cvid_cases %>% 
  mutate(week_of = floor_date(new_date, unit = "1 week")) %>% 
  group_by(week_of) %>% 
  summarise(weekly_cases = sum(new_cases))

weekly_cases <- function(df, c_date,c_cases){
  df %>% 
    mutate(week_of = floor_date({{c_date}}, unit = "1 week")) %>%
    group_by(week_of) %>%
    summarise(sum_cases = sum({{c_cases}})) %>% 
    ungroup() %>% 
    arrange(week_of)
}


covidTrends_covid <- weekly_cases(cvid_cases, new_date,  new_cases)


writeFun <- function(cdata1, cdata2,...){
  sheetList <- list(cdata1,cdata2)
  list_name <- c("cdata1", "cdata2","cdata3")
  names(sheetList) <- list_name
  if (length(sheetList)!= length(list_name)){
    print("Please enter same length of names..")
    
  }
  
  openxlsx::write.xlsx(sheetList, "tables.xlsx")
}

writeFun(covidTrends, covidTrends_covid,covidTrends)




