
taxa.plots <- purrr::map(taxa.slopes, ~plot.hist(.))
zip_list_name <- zip_list
names(zip_list_name) <- zip_list

alldata_summmary <- purrr::map(zip_list_name, ~my_func(pop_byzip_sex_race_alone,zip_list))

temp_data <- 'C://Users//yihun//Documents//R4DS//pop-zipcode//Clean Data//temp_data'
dir.create(temp_data)


walk2(paste0(temp_data, names(zip_list_name), ".csv"), alldata_summmary, write.csv)

walk2(paste0(path2, names(taxa.slopes), ".pdf"), taxa.plots, ggsave)


temp <- 'C://Users//yihun//Documents//R4DS//pop-zipcode//Clean Data//temp'
dir.create(temp)

GEOID <- my_func(pop_byzip_sex_race_alone, zip_list)
paths <- file.path(temp, paste0("GEOID-", names(GEOID), ".csv"))
walk2(GEOID, paths, write.csv)

dir(temp)
#> [1] "cyl-4.csv" "cyl-6.csv" "cyl-8.csv"


alldata_summmary <- purrr::map_dfr(zip_list, ~my_func(pop_byzip_sex_race_alone,zip_list_name) %>%my_func_wider())
temp_data <- 'C://Users//yihun//Documents//R4DS//pop-zipcode//Clean Data//temp_data'
paths <- file.path(temp_data, paste0("ZIP-", names(alldata_summmary), ".csv"))
walk2(alldata_summmary, paths, write.csv)
dir(temp_data)

my_func(pop_byzip_sex_race_alone, 64151)my_func_wider()
  my_func_wider()

  
  
  
  
  
  
  
  