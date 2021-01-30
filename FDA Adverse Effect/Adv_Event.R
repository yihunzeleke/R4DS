library(dplyr)
library(tidyr)
library(lubridate)
library(data.table)
library(ggplot2)
library(tidytext)
library(tm)
library(stringr)
# Importing Data 
# Set working directory
# setwd("C:/Course-Project")
# fda1 <- fread("CAERSASCII-2004-2013.csv",header = TRUE,sep=",",na.strings = c("","NA"))
# fda2 <- fread("CAERSASCII 2014-2019.csv", header = FALSE, sep =',', na.strings = c("","NA"))
# fda2 <- fda2[-1,] # row with names
# # Merging two datasets
# fda_combined <- rbind(fda1, fda2, use.names = FALSE)
# 
# #Renaming the column in proper  R-format by replacing the ' ' by '_'
# names(fda_combined) <- gsub(" ", "_", names(fda_combined))
# names(fda_combined)
# 
# ## Structure of the data:
# str(fda_combined)
# 
# # converting Date to standard time
# fda_combined$CAERS_Created_Date <- gsub("-", "/", fda_combined$CAERS_Created_Date)
# fda_combined$Date_of_Event <- gsub("-", "/", fda_combined$Date_of_Event)
# 
# fda_combined$CAERS_Created_Date <- parse_date_time(fda_combined$CAERS_Created_Date,"mdy")
# fda_combined$Date_of_Event <- parse_date_time(fda_combined$Date_of_Event,"mdy")
# 
# 
# ## Converting Pacient_Age by Age_Units
# fda_combined$Patient_Age <- as.numeric(as.character(fda_combined$Patient_Age))
# 
# fda_combined$Patient_Age <- ifelse(fda_combined$Age_Units == 'month(s)'
#                         &!is.na(fda_combined$Patient_Age),
#                         round(fda_combined$Patient_Age/12, digits = 3),
#                         ifelse(fda_combined$Age_Units == 'day(s)' & !is.na(fda_combined$Patient_Age),
#                         round(fda_combined$Patient_Age/365, digits = 3),fda_combined$Patient_Age))
# 
# 
# ## In the Product_Code there are observation set to be 40 and 41 for :40C,41G,40M,40P,40N
# 
# # fda_combined[Product_Code %in% c('40C','40M', '40P', '40N'), Product_Code:= '40']
# # fda_combined[Product_Code %in% '41G', Product_Code:= '41']
# # fda_combined$Product_Code <- as.numeric(crdata$Product_Code)
# 
# 
# ## ------------------------------------------------------------------------------------------------------
# ## ------------------------------------------------------------------------------------------------------
# library(tidyquant) # filling missing values based on before and after observations
# 
# 
# fda_combined <- fda_combined %>%
#   mutate(Date_of_Event = na.locf(Date_of_Event))
# 
# # Dropping 31 rows which has Date_of_Event greater than 2019
# fda_combined <-  fda_combined %>%
#   mutate(Time_Diff = CAERS_Created_Date - Date_of_Event) %>%
#   filter(!(Time_Diff < 0))
# fda_combined[,Time_Diff:= NULL]
# 
# library(VIM)
# names(fda_combined)
# colSums(is.na(fda_combined))
# # Missing value imputation for product_code
# fda_combined <- kNN(fda_combined, variable ='Product_Code', k=3)
# 
# fda_combined <- subset(fda_combined,select=Report_ID:Outcomes)
# 
# # Missing value imputation for porduct
# fda_combined <- kNN(fda_combined, variable ='Product', k=3)
# 
# fda_combined <- subset(fda_combined,select=Report_ID:Outcomes)
# 
# # Missing value imputation for MedDRA_Preferred_Terms
# fda_combined <- kNN(fda_combined,variable ='MedDRA_Preferred_Terms', k=3)
# 
# fda_combined <- subset(fda_combined,select = Report_ID:Outcomes)
# 
# 
# # Missing value imputation for Description
# fda_combined <- kNN(fda_combined,variable ='Description', k=3)
# 
# fda_combined <- subset(fda_combined,select = Report_ID:Outcomes)
# 
# # Imputation for continous and categorcal variables
# # imputation method would be pmm for continous and polyreg for categorical
# fda_combined$Age_Units <- factor(fda_combined$Age_Unit)
# fda_combined$Sex <- factor(fda_combined$Sex)
# fda_combined$Patient_Age <- as.numeric(fda_combined$Patient_Age)
# 
# library(mice)
# 
# temp <- mice(fda_combined[,8:10], m =3)
# 
# summary(temp)
# 
# tempdata <- complete(temp,3) # Imputed with 3 iteration
# 
# library(plyr)
# 
# tempdata$Sex <- revalue(tempdata$Sex,c("NR"="F", "U"="M"))
# s <- fda_combined[,-c(8:10)]
# fda_combined <- cbind(s,tempdata)
# # Rearrange columns
# fda_combined <- fda_combined[,c(1,2,3,4,5,6,7,10,11,12,8,9)]
# 
# write.csv(fda_combined,"FDA_COMBINED.csv", row.names = FALSE)

## ------------------------------------------------------------------------------------------------------
## ------------------------------------------------------------------------------------------------------

## Getting cleaned data
setwd("~/FDA")
fda_combined <- fread('FDA_COMBINED.csv')

# For system runing issues I only take some some 2000 data from the master dataset.
set.seed(123)
fda_combined <- fda_combined[sample(1:nrow(fda_combined),2000 ,replace=FALSE),]
fda_combined <- setDT(fda_combined) # conveting the dataframe to data.table
str(fda_combined)
colSums(is.na(fda_combined))
## ------------------------------------------------------------------------------------------------------
## EDA

# -- Report ID

#1.a.	Total number of reports reported to FDA

# 130079 cases were reported for FDA

#1.b. Does total number of reports corresponds to total number of patients facing issues
# No

#1.c. Are there any chances of duplicate reports since it is reported either by the patient/consumer or 
# their HCP (healthcare professional/doctor)
length(unique(fda_combined$Report_ID))

# fda_combined <- distinct(fda_combined) : duplicated rows/observations

#1.d. Are there reports of patients who have consumed multiple products and experienced adverse events? 
# Can something like a system could be built to identify them?
# ???

## -- CAERS Created Date

# 2.a. 	What is the change in number of reports reported against year?
# 2.b. Has there been any drastic change in numbers? If so why?
# There is a drastic change in yearly from 2015-2018, the increase in cosmotics market and
fda_combined %>% 
  group_by(Year = year(CAERS_Created_Date))%>% 
  summarise(Reports = n())


ggplot(fda_combined %>% 
    group_by(Year = factor(year(CAERS_Created_Date))) %>% 
    summarise(Count = n()))+geom_bar(aes(Year, Count), stat = "identity", fill = "coral1")+
    geom_label(aes(factor(Year), Count, label = Count), vjust = 0.5)+
    theme(axis.text.x = element_text(angle = 45, hjust = 1))


# 2.c. Find the difference between the time the report was created and the actual time of Adverse event 
# and check the difference in time for different age range.

fda_combined$CAERS_Created_Date <- as.Date(as.character(fda_combined$CAERS_Created_Date))
fda_combined$Date_of_Event <- as.Date(as.character(fda_combined$Date_of_Event))

fda_combined %>% 
  mutate(Time_Diff = CAERS_Created_Date - Date_of_Event) %>% 
   summarise(Mean = mean(Time_Diff),Min = min(Time_Diff), Max = max(Time_Diff))
  
# Which day/week of the week/month the AEs reported are more in numbers? Is there a tendency/trend in   
# people to report as the week goes on (reasons might be- trying to pass time on weekends, bad weekend,
# hangover and over eating on weekends, start of the week)  
#Thursday,Tuesday and Wednesday are the busy days to report the Adverse Event reports to FDA.
fda_combined %>% 
   group_by( WeekDay = lubridate::wday(fda_combined$CAERS_Created_Date,label = TRUE,abbr = FALSE, 
                       week_start = getOption("lubridate.week.start", 7),
                       locale = Sys.getlocale("LC_TIME")))%>% 
                       summarise(CEARS_NumberReports = n()) %>% 
  ggplot(aes(WeekDay , CEARS_NumberReports))+geom_bar(stat = 'identity')+
  theme(axis.text.x = element_text(color = "#993333",size=8 , angle = 45)) 


 
 
# Date of the Event

# Has the number of adverse events increased or decreased over years? 
# The number of advers events are increased since 2018 and 2019 there is high dropping down
fda_combined %>% 
  group_by(advers_evetnt = year(Date_of_Event))%>% 
  summarise(count = n())

# If it has increased, is it because more events are occurring or more events are being reported? 
# And vice versa (in case it has decreased)

fda_combined$CAERS_Created_Date <-  parse_date_time(fda_combined$CAERS_Created_Date, 'ymd')
fda_combined$Date_of_Event <-  parse_date_time(fda_combined$Date_of_Event, 'ymd')

 ggplot(fda_combined %>% 
 group_by(Year = factor(year(Date_of_Event))) %>% 
  summarise(numberofReports = n()))+ 
   geom_bar(aes(Year , numberofReports),stat = 'identity')+
   theme(axis.text.x = element_text(color = "#993333",size=8 , angle = 45))
 

# Product Type
    
# Which type (suspect/concomitant) has more counts of adverse events?  
# SUSPECT has more count  

fda_combined %>%
  group_by(Product_Type) %>%
  count() %>%
  arrange(-n)

# Can a causality be concluded on suspect/concomitant wrt to adverse events (MedDRA)

# * Causation is not in the data and cannot be. Data only contains correlation.
# * we determining the association between Product_Type and medDRA. 
# Ho: there is no association between Product_Type and medDRA
# HA: there is association between Product_Type and medDRA

fda_combined$MedDRA_Preferred_Terms <- factor(fda_combined$MedDRA_Preferred_Terms)
fda_combined$Product_Type <- factor(fda_combined$Product_Type)

chisq.test(fda_combined$Product_Type, fda_combined$MedDRA_Preferred_Terms,simulate.p.value = TRUE)

# X-squared = 61649, df = NA, p-value = 0.0004998
# we conclude that two we reject the null hypothesis, Product_Tpe and medDRA are dependent.


# Product/Product Code
# 	Total number of products reported
  length(fda_combined$Product) # Total products reported 130048
  length(unique(fda_combined$Product)) # Total Types of products reported 54076
                                       # Total Products atleast reported 2 and more  75972

# Most common product against which Adverse events has been reported maximum
  
  fda_combined %>% 
    group_by(Product) %>% 
    summarise(most_common_products = n()) %>% 
    arrange(desc(most_common_products),.by_group = TRUE) 
 
  
# Does the most common product in the list means the one which gets sold much?
# No 
 
# Has the report for the most common product has been consistent over the years (2004-2019, Mar)...
# Can do Time series
# There increase in 
 
common_product_20042019 <- fda_combined %>% 
          group_by(Product, CAERS_Created_Date) %>% 
          summarise(most_common_products = n()) %>% 
          arrange(desc(most_common_products),.by_group = TRUE) 
  
# some text cleaning for product

library(stringi)    
common_product_20042019$Product <- stri_replace_all_fixed(common_product_20042019$Product, 
                                pattern = c('""', '#','.','**',':','?'), replacement = c(""),
                                vectorize_all = FALSE)

common_product_20042019 <- common_product_20042019 %>% 
  group_by(Year = year(CAERS_Created_Date), Product) %>% 
  summarise(most_common_count = n()) %>% 
  arrange(desc(most_common_count),.by_group = TRUE)

ggplot(common_product_20042019)+geom_line(aes(Year, most_common_count), stat = 'identity')


#	Form clusters based on outcome 

outcome_clust <- subset(fda_combined,
                          select=c(Product,Outcomes))

outcome_clust <- outcome_clust %>% 
  mutate(Outcomes = strsplit(as.character(Outcomes),",")) %>% 
  unnest(Outcomes)


View(outcome_clust)
str(outcome_clust)

myCorpus <- Corpus(VectorSource(outcome_clust$Outcomes))
inspect(myCorpus[1:5])
myCorpus <- tm_map(myCorpus, content_transformer(tolower))
myCorpus <- tm_map(myCorpus,removePunctuation)
myCorpus <- tm_map(myCorpus, removeWords,stopwords("english"))
myCorpus <- tm_map(myCorpus, stripWhitespace)

inspect(myCorpus)
class(myCorpus)
docmat <- DocumentTermMatrix(myCorpus)
dtm_wt_idf <- weightTfIdf(docmat)

inspect(dtm_wt_idf)


mat_w <- as.matrix(dtm_wt_idf)
as.data.frame(mat_w)
str(mat_w)
View(mat_w)

outcome_clust <- cbind('Product'= outcome_clust$Product,
                        as.data.frame(mat_w))
str(outcome_clust)


#traindata_clust[,-1]#removing first row

clus_matr_k <- kmeans(outcome_clust[,-1], centers = 5, nstart = 20)

clus_matr_k$cluster
round(clus_matr_k$centers, digits = 1)

for (i in 1:5) 
{
  cat(paste("cluster ", i, ": ", sep = " "))
  sorted_clustre <- sort(clus_matr_k$centers[i, ], decreasing = T)
  cat(names(sorted_clustre)[1:5], "\n")
}



# A patient can have one Report ID with various Adverse events, product. So here, classify products
# based on Report Id for individual patients.

fda_combined %>% 
  #select(Report_ID,Product) %>% 
  group_by(Report_ID) %>% 
  summarise(Porcduct= n())


# Predict and forecast products sold


#Description
# What was the most common 'Description' wrt to Adverse event?

common_desctiption <- subset(fda_combined,
                       select=c(Description,Outcomes))

common_desctiption <- common_desctiption %>% 
  mutate(Description = strsplit(as.character(Description),",")) %>% 
  unnest(Description)


View(common_desctiption)
str(common_desctiption)

descCorpus <- Corpus(VectorSource(common_desctiption$Description))
inspect(descCorpus[1:5])
descCorpus <- tm_map(descCorpus, content_transformer(tolower))
descCorpus <- tm_map(descCorpus,removePunctuation)
descCorpus <- tm_map(descCorpus, removeWords,stopwords("english"))
descCorpus <- tm_map(descCorpus, stripWhitespace)

TrMt <- TermDocumentMatrix(descCorpus)
TrMt <- as.matrix(TrMt)
TrMt[1:20,1:20]
commonwords <- rowSums(TrMt)
commonwords <- subset(w, w>=5)
barplot(commonwords, las = 2, col = rainbow(22))

commonwords <- sort(rowSums(TrMt), decreasing = TRUE)
set.seed(123)
wordcloud(words = names(commonwords), freq = commonwords, max.words = 150,
          random.order =FALSE ,
          min.freq = 5,
          colors = brewer.pal(8,'Dark2'),
          scale = c(5,0.7),
          rot.per = 0.3)

#Which Product code has the maximum number of products assigned?

fda_combined %>% 
  group_by(Product_Code) %>% 
  summarise(count=n()) %>% 
  arrange(desc(count),.by_group = TRUE) %>% 
    View()
  
# Sentiment analysis of Product description 
destrain_des <- subset(fda_combined,
                                select=c(Product_Type,Description))

head(destrain_des)
View(destrain_des)


# exploring the loaded data
str(destrain_des)
destrain_des$Product_Type <- factor(destrain_des$Product_Type)
str(destrain_des$Product_Type )
table(destrain_des$Product_Type)

destrain_des <- destrain_des %>% 
  mutate(Description = strsplit(as.character(Description), ",")) %>% 
  unnest(Description)

# Data preparation - processing text data for analysis
des_corpus <- Corpus(VectorSource(fda_combined$Description))
print(des_corpus)
inspect(des_corpus[1:3])

# basic text cleaning steps
corpus_clean <- tm_map(des_corpus, tolower)
corpus_clean <- tm_map(des_corpus, removeNumbers)
corpus_clean <- tm_map(des_corpus, removeWords, stopwords())
corpus_clean <- tm_map(des_corpus, removePunctuation)
corpus_clean <- tm_map(des_corpus, stripWhitespace)
inspect(corpus_clean[1:10]) # after cleanup

des_corpus_dtm <- DocumentTermMatrix(corpus_clean)
des_corpus_dtm_mat <- as.matrix(des_corpus_dtm)

#nrow(fda_combined)
## Data preparation - creating training and test datasets 75:25
des_train <- destrain_des[1:1300, ]
des_test <- destrain_des[1301:2000, ]

des_dtm_train <- des_corpus_dtm_mat[1:1300, ]
des_dtm_test <- des_corpus_dtm_mat[1:2000, ]

des_corpus_train <- corpus_clean[1:1300]
des_corpus_test <- corpus_clean[1:2000]

#check if split of CONCOMITANT/SUSPECT are having equal or not
#w.r.t training and test 
prop.table(table(des_train$Product_Type))
prop.table(table(des_test$Product_Type))

#words with repeat atleast 5 times
#ptype_dict <- findFreqTerms(ptype_dtm_train,5)

#transformation of 1,0 into Yes,No
convert_count<-function(x)
{
  x <- ifelse(x>0,1,0)#if x>0 1 is replaced o.w 0
  x <- factor(x,levels=c(0,1),labels=c("No","Yes"))
}

des_train <- apply(des_train,MARGIN =2,convert_count)
des_test <- apply(des_test,MARGIN =2,convert_count)

## training a model on the data
classifier <- naiveBayes(des_train, destrain_des$Product_Type[1:1300])

## Step 4 - evaluating model performance
test_pred <- predict(classifier, destrain_des$Product_Type[1301:2000])

## CONFUSION MATRIX
CrossTable(test_pred,  destrain_des$Product_Type[1301:2000],
           prop.chisq = FALSE, prop.t = FALSE,
           dnn = c('predicted', 'actual'))
### -----------------------------------------------------------------------------------------------------

# Patient Age
#	Which age was the most affected ones in the list?

  ggplot(fda_combined)+geom_histogram(aes(Patient_Age))
# Find the min and max age, and then see - Can age be grouped into Age group class and check for 
  # the most affected group? 
min(fda_combined$Patient_Age)
max(fda_combined$Patient_Age)

age_cat <- fda_combined %>% 
  mutate(Age_Category=cut(Patient_Age, breaks=c(-Inf, 25, 50,75, Inf), 
                          labels=c("Teenage(0-25)","Adult(25-50)","Elder(50-75)","75+")))
ggplot(age_cat)+ geom_bar(aes(Age_Category))

age_cat %>% 
  group_by(Age_Category) %>% 
  summarise(count = n())

# What was the most common Adverse event in all the age group? Was the Adverse event against the same 
# product or different in different age group? Which type of customers bought it more?

grouby_age <- age_cat %>% 
  group_by(Age_Category, Outcomes) %>% 
  summarise( count = n())
 # arrange(desc(count),.by_group = TRUE) 
  

grouby_age <- setDT(grouby_age)
  
grouby_age[grouby_age[, .I[count == max(count)], by=Age_Category]$V1]
  
 #	Which age range seems to be the most dangerous?  
 #  Elder(50-75)
  
# Which age group suffers the most severe outcomes?
# Elder(50-75)
sever_age <- filter(age_cat, grepl('Death|Disability|Life|Hospitalization', Outcomes))
 
sever_age %>% 
    group_by(Age_Category) %>% 
   summarise(count= n())
    
# Sex
#	Which gender has reported more Adverse events?
  fda_combined %>%  
    group_by(Sex) %>% 
    count()
  
# Common AE reported by male and females respectively
sex_group <- fda_combined %>% select(Sex,Product,Outcomes) 
   
  sex_group %>% 
  group_by(Sex, Outcomes) %>% 
  summarise(AE_Reported = n()) %>% 
  arrange(desc(AE_Reported),.by_group = TRUE) %>% 
  slice(1:10) # Top 10 Common AE reported by male and females 

  # Common product against which AE has been reported (both male and female)
  
  sex_group %>% 
    group_by(Sex,Product) %>% 
    summarise(AE_Reported = n()) %>% 
    arrange(desc(AE_Reported),.by_group = TRUE) %>% 
    slice(1:10) %>% # Top 10 Common Products against AE  
  ggplot(aes(Product, AE_Reported, fill= Sex ))+geom_bar(stat = 'identity')+
  theme(axis.text.x = element_text(size = 8, angle = 45, vjust = 0.7))+
  coord_flip()
    
  # MedDRA Preferred Terms
  #	Total number of adverse events reported
  length(fda_combined$MedDRA_Preferred_Terms)
  
  # The most common AE/Symptoms
  
  AE_Symptoms <- fda_combined %>%
    select(MedDRA_Preferred_Terms) %>% 
    group_by(MedDRA_Preferred_Terms) %>%
    mutate(linenumber = row_number(),
           chapter = cumsum(str_detect(MedDRA_Preferred_Terms, regex("^chapter [\\divxlc]",
           ignore_case = TRUE)))) %>%
           ungroup()

  
  library(tidytext)
  
  AE_Symptoms <- AE_Symptoms %>%
    unnest_tokens(word, MedDRA_Preferred_Terms)
  
  data(stop_words)
  AE_Symptoms <- AE_Symptoms %>%
    anti_join(stop_words)
   
  AE_Symptoms %>%
      count(word, sort = TRUE)  

  # Viaualize the most common words listed morethan 100 times 
  AE_Symptoms %>%
       count(word, sort = TRUE) %>%
       filter(n > 100) %>%
       mutate(word = reorder(word, n)) %>%
       ggplot(aes(word, n)) +
       geom_col() +
       xlab(NULL) +
        coord_flip()
  
# Did all the common symptoms take place due to the same product?
  AE_Product <- fda_combined %>%
    select(Product,MedDRA_Preferred_Terms) %>% 
    group_by(MedDRA_Preferred_Terms) %>%
    mutate(linenumber = row_number(),chapter = cumsum(str_detect(MedDRA_Preferred_Terms, 
                 regex("^chapter [\\divxlc]", ignore_case = TRUE)))) %>%
    ungroup()
                                                     
  
   AE_Product <- AE_Product %>%
    unnest_tokens(word, MedDRA_Preferred_Terms)
  
   data(stop_words)
   AE_Product <- AE_Product %>%
     anti_join(stop_words)
   
   AE_Product <- AE_Product %>% 
     group_by(Product) %>% 
  
     count(word, sort = TRUE)  
   
   # Visualize the most common product with the AE symptoms
   AE_Product %>%
    filter(n >= 5) %>%
     ggplot(aes(word, n, fill = Product)) +
     geom_col() +
     xlab(NULL) +
     coord_flip()

 #AE as per year/month/weekday  
 AE_Date <-   fda_combined %>%
   select(CAERS_Created_Date, MedDRA_Preferred_Terms) %>% 
   group_by(MedDRA_Preferred_Terms) %>%
   mutate(linenumber = row_number(),
          chapter = cumsum(str_detect(MedDRA_Preferred_Terms, regex("^chapter [\\divxlc]",
                                                                    ignore_case = TRUE)))) %>%
   ungroup()
   
   
 AE_Date <- AE_Date %>%
   unnest_tokens(word, MedDRA_Preferred_Terms)
 
 data(stop_words)
 AE_Date <- AE_Date %>%
   anti_join(stop_words)
 
 AE_Date <- AE_Date %>% 
   group_by(CAERS_Created_Date) %>% 
   
   count(word, sort = TRUE)  
   
# AE by Year   
 
 AE_Date %>%
   group_by(Year = year(CAERS_Created_Date)) %>% 
   filter(n >=5) %>%
   ggplot(aes(factor(Year), n, fill =word )) +
   geom_col() +
   xlab(NULL) 
 
   
# AE by Month
 AE_Date %>%
   group_by(Month = (lubridate::month(CAERS_Created_Date, label = TRUE))) %>% 
   filter(n >= 5) %>%
   ggplot(aes(Month, y = n, fill = word)) +
   geom_col() +
   xlab(NULL) 
   
 
 # AE by WeekDay
 AE_Date %>%
   group_by(Weekday = (lubridate::wday(CAERS_Created_Date, label = TRUE))) %>% 
   filter(n >= 5) %>%
   ggplot(aes(word, y = n, fill = factor(Weekday)))+
   geom_col() +
   xlab(NULL) +
   theme(axis.text.x = element_text(size = 8, angle = 45, vjust = 0.6))
 
 

                                     
      