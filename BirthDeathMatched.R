library(tidyverse)
library(haven)

# To merge the data birth death matched of the different year just for the we select only matched variables
# select the variable names and intersect each other


# FetalDeath2010 <- read_sav("Fetal Death/FetalDeath2010.sav")
# FetalDeath2011 <- read_sav("Fetal Death/FetalDeath2011.sav")
# FetalDeath2012 <- read_sav("Fetal Death/FetalDeath2012.sav")

FetalDeath2013 <- read_sav("Fetal Death/FetalDeath2013.sav")
FetalDeath2014 <- read_sav("Fetal Death/FetalDeath2014.sav")
FetalDeath2015 <- read_sav("Fetal Death/FetalDeath2015.sav")
FetalDeath2016 <- read_sav("Fetal Death/2016 Fetal death data file.sav")
FetalDeath2017 <- read_sav("Fetal Death/FetalDeath2017.sav")
FetalDeath2018 <- read_sav("Fetal Death/FetalDeath2018.sav")
FetalDeath2019 <- read_sav("Fetal Death/FetalDeath2019.sav")

# name_2010 <- as.character(names(FetalDeath2010))
# name_2011 <- as.character(names(FetalDeath2011))
# name_2012 <- as.character(names(FetalDeath2012))

name_2013 <- as.character(names(FetalDeath2013))
name_2014 <- as.character(names(FetalDeath2014))
name_2015 <- as.character(names(FetalDeath2015))
name_2016 <- as.character(names(FetalDeath2016))
name_2017 <- as.character(names(FetalDeath2017))
name_2018 <- as.character(names(FetalDeath2018))
name_2019 <- as.character(names(FetalDeath2019))

name2013_2018 <- Reduce(intersect, list(name_2013, name_2014, name_2015,name_2017, name_2018))

name2013_2018 <- as.data.frame(name2013_2018) 

name2013_2018 <- name2013_2018[, order(colnames(name2013_2018))]

write.csv(name2013_2018, "name2013-2018.csv",row.names = F)



name2016_2019 <- Reduce(intersect, list(name_2016, name_2019))

name2016_2019 <- as.data.frame(name2016_2019) 

name2016_2019 <- name2016_2019[, order(colnames(name2016_2019))]

write.csv(name2016_2019, "name2016_2019.csv",row.names = F)

common_variables <-  Reduce(intersect, list(name_2013, name_2014,
                                            name_2015, name_2016, 
                                            name_2017, name_2018, name_2019)) 
mergeCols <- as.character(common_variables)

# subsetting and rearranging by variable alphabetically 

fetal_2010_common <- FetalDeath2010 %>% 
  select(contains(common_variables))
fetal_2010_common <- fetal_2010_common[, order(colnames(fetal_2010_common))]

BDM_2011_common <- FetalDeath2011 %>% 
  select(contains(common_variables))
BDM_2011_common <- BDM_2011_common[, order(colnames(BDM_2011_common))]

BDM_2012_common <- FetalDeath2012 %>% 
  select(contains(common_variables))
BDM_2012_common <- BDM_2012_common[, order(colnames(BDM_2012_common))]


BDM_2013_common <- BDM_2013 %>% 
  select(contains(common_variables))
BDM_2013_common <- BDM_2013_common[, order(colnames(BDM_2013_common))]


BDM_2014_common <- BDM_2014 %>% 
  select(contains(common_variables))
BDM_2014_common <- BDM_2014_common[, order(colnames(BDM_2014_common))]

BDM_2015_common <- BDM_2015 %>% 
  select(contains(common_variables))
BDM_2015_common <- BDM_2015_common[, order(colnames(BDM_2015_common))]

BDM_2016_common <- BDM_2016 %>% 
  select(contains(common_variables))
BDM_2016_common <- BDM_2016_common[, order(colnames(BDM_2016_common))]

BDM_2017_common <- BDM_2017 %>% 
  select(contains(common_variables))
BDM_2017_common <- BDM_2017_common[, order(colnames(BDM_2017_common))]

BDM_2018_common <- BDM_2018 %>% 
  select(contains(common_variables))
BDM_2018_common <- BDM_2018_common[, order(colnames(BDM_2018_common))]

BDM_2019_common <- BDM_2019 %>% 
  select(contains(common_variables))
BDM_2019_common <- BDM_2019_common[, order(colnames(BDM_2019_common))]

# write out files to csv to merge on excel sheet 
write.csv(BDM_2013_common, 'BDM_2013_common.csv', row.names = F)
write.csv(BDM_2014_common, 'BDM_2014_common.csv', row.names = F)
write.csv(BDM_2015_common, 'BDM_2015_common.csv', row.names = F)
write.csv(BDM_2016_common, 'BDM_2016_common.csv', row.names = F)
write.csv(BDM_2017_common, 'BDM_2017_common.csv', row.names = F)
write.csv(BDM_2018_common, 'BDM_2018_common.csv', row.names = F)
write.csv(BDM_2019_common, 'BDM_2019_common.csv', row.names = F)











