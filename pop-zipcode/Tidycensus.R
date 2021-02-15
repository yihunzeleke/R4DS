
library(tidyverse)
library(tidycensus)


# census_api_key(my_cesus_api_key, install = TRUE)

census_api_key(Sys.getenv("CENSUS_API_KEY"))

acs_variables <- load_variables(year = "2019", dataset = "acs5", cache = T)
write.csv(acs_variables, "pop-zipcode/Clean Data/ACS-2019 Variables.csv", row.names = F)

zip_list = c("64101", "64102", 
             "64105", "64106", "64108", "64109", "64110", 
             "64111", "64112", "64113", "64114", "64116", 
             "64117", "64118", "64119", "64120", "64123", 
             "64124", "64125", "64126", "64127", "64128", 
             "64129", "64130", "64131", "64132", "64133", 
             "64134", "64136", "64137", "64138", "64139", 
             "64145", "64146", "64147", "64149", "64151", 
             "64152", "64153", "64154", "64155", "64156", 
             "64157", "64158", "64161", "64163", "64164",
             "64165", "64166", "64167")



pop_byzip <- get_acs(geography = "zcta",year = 2019,
                   variables = "B01003_001",
                   output = "tidy") %>% 
  filter(GEOID %in% zip_list) %>% 
  select(GEOID,variable, estimate)

pop_byzip <- pop_byzip %>% 
  left_join(acs_variables, by  = c("variable"= "name")) %>%
  mutate(label = str_remove(label, "!!Total"))

#==# get variable names
var_list_sex_age <- as.character(acs_variables %>% 
                                   filter(name >= "B01001_001" & name <= "B01001_049") %>% 
                                   select(name) %>% 
                                   pull())
#==# Sex by Age
pop_byzip_sex_age <-  get_acs(geography = "zcta",year = 2019,
                              variables = var_list_sex_age,
                              output = "tidy") %>% 
  filter(GEOID %in% zip_list) %>% 
  left_join(acs_variables, by = c("variable"="name"))

#==# Categorizes the Age group
pop_byzip_Sex_Age <- pop_byzip_sex_age %>% 
  mutate(age = case_when(
    str_detect(label,"Under 5 years$") ~ "Under 5 years",
    str_detect(label, "5 to 9 years$") ~ "5 to 9 years",
    str_detect(label, "10 to 14 years$") ~ "10 to 14 years",
    str_detect(label, "15 to 17 years$|18 and 19 years$") ~ "15 to 19 years",
    str_detect(label, "20 years$|21 years$|22 to 24 years$") ~ "20 to 24 years",
    str_detect(label,"25 to 29 years$") ~  "25 to 29 years",
    str_detect(label, "30 to 34 years$") ~ "30 to 34 years",
    str_detect(label, "35 to 39 years$") ~ "35 to 39 years",
    str_detect(label, "40 to 44 years$") ~ "40 to 44 years",
    str_detect(label, "45 to 49 years$") ~ "45 to 49 years",
    str_detect(label, "50 to 54 years$") ~ "50 to 54 years",
    str_detect(label, "55 to 59 years$") ~ "55 to 59 years",
    str_detect(label, "60 and 61 years$|62 to 64 years$") ~ "60 to 64 years",
    str_detect(label, "65 and 66 years$|67 to 69 years$") ~ "65 to 69 years",
    str_detect(label, "70 to 74 years$") ~ "70 to 74 years",
    str_detect(label, "75 to 79 years$") ~ "75 to 79 years",
    str_detect(label, "80 to 84 years$") ~ "80 to 84 years",
    str_detect(label, "85 years and over$") ~ "85 years and over",
    label == "Estimate:" ~ "Estimate_Total",
    label == "Estimate:!!Male:" ~ "Male_Total",
    label == "Estimate:!!Female:" ~ "Female_Total"
    )) %>% drop_na() %>% 
  mutate(age = factor(age, levels =c("Under 5 years", "5 to 9 years","10 to 14 years","15 to 19 years","20 to 24 years", "25 to 29 years",
                                     "30 to 34 years", "35 to 39 years","40 to 44 years",
                                     "45 to 49 years","50 to 54 years","55 to 59 years",
                                     "60 to 64 years","65 to 69 years","70 to 74 years",
                                     "75 to 79 years","80 to 84 years","85 years and over","Estimate_Total","Male_Total","Female_Total")))

#==# make wider
pop_byzip_Sex_Age_wide <- pop_byzip_Sex_Age %>% 
  group_by(GEOID, age) %>% 
  summarise(estimate = sum(estimate)) %>%
  select(GEOID, estimate, age) %>% 
  pivot_wider(names_from = age, values_from = estimate)

write.csv(pop_byzip_Sex_Age_wide, "pop-zipcode/Clean Data/Population Sex-Age.csv", row.names = F)

#==# population by race

var_list_race_alone <- as.character(acs_variables %>% 
                                   filter(name >= "B01001A_001" & name <= "B01001I_031") %>% 
                                   select(name) %>% 
                                   pull())

pop_byzip_sex_race_alone <-  get_acs(geography = "zcta",year = 2019,
                              variables = var_list_race_alone,
                              output = "tidy") %>% 
  filter(GEOID %in% zip_list) %>% 
  left_join(acs_variables, by = c("variable"="name")) 

#==# remove duplicate white race
pop_byzip_sex_race_alone <- pop_byzip_sex_race_alone %>% 
  filter(str_detect(variable, "^B01001H_", negate = T))

#==# clean data
zip_group <- pop_byzip_sex_race_alone %>% 
  mutate(race_category = str_remove_all(concept, "SEX BY AGE") %>% str_replace_all(.,"[[:punct:]]","")) %>% 
  mutate(age_category = str_remove_all(label, ":") %>% str_replace_all(.,"[[:punct:]]"," ")) %>% 
  select(GEOID, estimate, age_category, race_category)

#==# grouping and summarizing
zip_group_wide <- zip_group %>% 
  group_by(GEOID,age_category, race_category) %>% 
  summarise(estimate = sum(estimate)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = race_category, values_from = estimate)

#==# split data by zip code and write to csv
alldata_summmary <- split(zip_group_wide, zip_group_wide$GEOID)
temp_data <- 'C://Users//yihun//Documents//R4DS//pop-zipcode//Clean Data//temp_data'
paths <- file.path(temp_data, paste0("ZIP-", names(alldata_summmary), ".xlsx"))
walk2(alldata_summmary, paths, openxlsx::write.xlsx , row.names = F)
dir(temp_data)

#==# write data: with sheet name zip-code
openxlsx::write.xlsx(alldata_summmary, "pop-zipcode//Clean Data//ZIP-CODE.xlsx")











  
  