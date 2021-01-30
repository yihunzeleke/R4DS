library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(tidytext)
library(tm)
library(stringr)

# Importing data from my personal github
fda1 <- data.table::fread("https://raw.githubusercontent.com/yihunzeleke/data_repo/master/CAERSASCII-2004-2013.csv", 
                          header = TRUE, sep = ',',na.strings = c("", "NA")) %>% 
  janitor::clean_names() %>% drop_na()

fda2 <- data.table::fread("https://raw.githubusercontent.com/yihunzeleke/data_repo/master/CAERSASCII%202014-2019.csv", 
                          header = TRUE, sep = ',',na.strings = c("", "NA")) %>% 
  janitor::clean_names() %>% drop_na()


# Merging two data sets
fda_merged <- rbind(fda1, fda2, use.names = FALSE)

# The combined dataset is large enough and uses large memory and takes much time. I will use 10% of the datasets
set.seed(123)

# Date variable is not standard time
fda_combined <- fda_combined %>% 
  mutate(caers_created_date = gsub("-", "/", caers_created_date),
         date_of_event  = gsub("-", "/",date_of_event)) %>% 
  mutate(caers_created_date = parse_date_time(caers_created_date, "mdy"),
         date_of_event = parse_date_time(date_of_event,"mdy"))

# Convert Patient Age by Age_Units
fda_combined$patient_age <- ifelse(fda_combined$age_units == 'month(s)'
                                   &!is.na(fda_combined$patient_age),
                                   round(fda_combined$patient_age/12, digits = 3),
                                   ifelse(fda_combined$patient_age == 'day(s)' & !is.na(fda_combined$patient_age),
                                          round(fda_combined$patient_age/365, digits = 3),fda_combined$patient_age))


fda_combined <- fda_merged[sample(1:nrow(fda_merged),13007, replace = FALSE),]
write.csv(fda_combined, "fda_combined.csv", row.names = FALSE)

# remove files from the work space

rm(fda1, fda2, fda_merged)
# structure of the data















