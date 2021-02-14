library(tidyverse)
library(tidycensus)

# my_cesus_api_key <- "6bf9d12594022aa02fd62b71cf0261c04c132565"
# census_api_key(my_cesus_api_key, install = TRUE)

census_api_key(Sys.getenv("CENSUS_API_KEY"))
acs_variables <- load_variables(year = "2019", dataset = "acs5", cache = T)
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

write.csv(acs_variables, "pop-zipcode/data/ACS-2019 Variables.csv", row.names = F)

var_list_sex_age <- as.character(acs_variables %>% 
                           filter(name >= "B01001A_001" & name <= "B01001A_031") %>% 
                           select(name) %>% 
                           pull())

pop_byzip <- get_acs(geography = "zcta",year = 2019,
                   variables = "B01003_001",
                   output = "tidy") %>% 
  filter(GEOID %in% zip_list) %>% 
  select(GEOID,variable, estimate)

pop_byzip <- pop_byzip %>% 
  left_join(acs_variables, by  = c("variable"= "name")) %>%
  mutate(label = str_remove(label, "Estimate!!"))

pop_byzip_sex_age <-  get_acs(geography = "zcta",year = 2019,
                              variables = var_list_sex_age) %>% 
  filter(GEOID %in% zip_list)







  
  
  