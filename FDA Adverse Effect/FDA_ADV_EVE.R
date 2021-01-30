library(tidyr)
library(dplyr)
library(plyr)
library(tidyverse)
library(data.table)
library(lubridate)
library(tm)
library(e1071)#naivebayes
library(gmodels)
setwd("D:\\Task SA New")

train_data1 <- fread("D:\\Task SA New\\CAERSASCII-2004-2013.csv",
                     header=TRUE, sep=",",na.strings=c("","NA"))

train_data_one <- train_data1[sample(1:nrow(train_data1), 500, replace=FALSE),]
nrow(train_data_one)
View(train_data_one)



train_data2 <- fread('D:\\Task SA New\\CAERSASCII 2014-2019.csv',
                     header=F, sep=",",na.strings=c("","NA"))
train_data_two <- train_data2[sample(1:nrow(train_data2), 800, replace=FALSE),]
train_data_two <- train_data_two[-1,]
nrow(train_data_two)
View(train_data_two)

train_data_master <- rbind(train_data_one,train_data_two,use.names=FALSE)
nrow(train_data_master)
View(train_data_master)

names(train_data_master)
#Renaming the column in proper  R-format by replacing the ' ' by '_'
names(train_data_master) <- gsub(" ", "_", names(train_data_master))
names(train_data_master)
#head(train_data_master$Age_Units,1000)


for (i in 1:nrow(train_data_master))
{
  Patient_Age=train_data_master[i,8]
  Age_Units=train_data_master[i,9]
  train_data_master[i,] <- within(train_data_master[i,],Patient_Age[Age_Units=="month(s)"] <- as.numeric(Patient_Age)/12)
}

for (i in 1:nrow(train_data_master))
{
  Patient_Age=train_data_master[i,8]
  Age_Units=train_data_master[i,9]
  train_data_master[i,] <- within(train_data_master[i,],Patient_Age[Age_Units=="day(s)"] <- as.numeric(Patient_Age)/365)
}

View(train_data_master)
str(train_data_master$Patient_Age)

train_data_master$Patient_Age <- round(as.numeric(train_data_master$Patient_Age),digits=4)
colSums(is.na(train_data_master))

train_data_master$CAERS_Created_Date <- gsub("-", "/", train_data_master$CAERS_Created_Date)
train_data_master$Date_of_Event <- gsub("-", "/", train_data_master$Date_of_Event)

train_data_master$CAERS_Created_Date <- parse_date_time(train_data_master$CAERS_Created_Date,"mdy")
train_data_master$Date_of_Event <- parse_date_time(train_data_master$Date_of_Event,"mdy")
#install.packages('VIM')
library(VIM)
names(train_data_master)

train_data_master <- kNN(train_data_master,
                         variable = c('Product_Code','Description',
                                      'Patient_Age','Age_Units','Sex','MedDRA_Preferred_Terms'), k=5)
train_data_master <- subset(train_data_master,select=Report_ID:Outcomes)
#str(train_data_master$Date_of_Event)
#install.packages('zoo')
library(zoo)
dtna <- as.Date(train_data_master$Date_of_Event, format = "%y-%m-%d")
str(x)

train_data_master$Date_of_Event <- as.Date(train_data_master$Date_of_Event)

train_data_master$Date_of_Event  <-  na.approx(dtna)

colSums(is.na(train_data_master))
names(train_data_master)



View(train_data_master)


############################################################################################

#Model-1 Using naive bayes
#aggregate(length(unique(train_data_master$Report_ID)))



#aggregate(train_data_master$Report_ID, train_data_master, function(x) length(unique(x)))
#table(is.na(train_data_master))


traindata_producttype <- subset(train_data_master,
                                select=c(Product_Type,MedDRA_Preferred_Terms))

head(traindata_producttype)
View(traindata_producttype)


# exploring the loaded data
str(traindata_producttype)
traindata_producttype$Product_Type <- factor(traindata_producttype$Product_Type)
str(traindata_producttype$Product_Type )
table(traindata_producttype$Product_Type)

traindata_producttype <- traindata_producttype %>% 
  mutate(MedDRA_Preferred_Terms = strsplit(as.character(MedDRA_Preferred_Terms), ",")) %>% 
  unnest(MedDRA_Preferred_Terms)

# Data preparation - processing text data for analysis
ptype_corpus <- Corpus(VectorSource(traindata_producttype$MedDRA_Preferred_Terms))
print(ptype_corpus)
inspect(ptype_corpus[1:3])

# basic text cleaning steps
corpus_clean <- tm_map(ptype_corpus, tolower)
corpus_clean <- tm_map(ptype_corpus, removeNumbers)
corpus_clean <- tm_map(ptype_corpus, removeWords, stopwords())
corpus_clean <- tm_map(ptype_corpus, removePunctuation)
corpus_clean <- tm_map(ptype_corpus, stripWhitespace)
inspect(corpus_clean[1:10]) # after cleanup

ptype_corpus_dtm <- DocumentTermMatrix(corpus_clean)
ptype_corpus_dtm_mat <- as.matrix(ptype_corpus_dtm)
#nrow(traindata_producttype)
## Data preparation - creating training and test datasets 75:25
ptype_train <- traindata_producttype[1:3170, ]
ptype_test <- traindata_producttype[3171:4201, ]

ptype_dtm_train <- ptype_corpus_dtm_mat[1:3170, ]
ptype_dtm_test <- ptype_corpus_dtm_mat[3171:4201, ]

ptype_corpus_train <- corpus_clean[1:3170]
ptype_corpus_test <- corpus_clean[3171:4201]

#check if split of CONCOMITANT/SUSPECT are having equal or not
#w.r.t training and test 
prop.table(table(ptype_train$Product_Type))
prop.table(table(ptype_train$Product_Type))

#words with repeat atleast 5 times
#ptype_dict <- findFreqTerms(ptype_dtm_train,5)

#ptype_train <- DocumentTermMatrix(ptype_corpus_train,list(dictionary=ptype_dict))
#ptype_test <- DocumentTermMatrix(ptype_corpus_test,list(dictionary=ptype_dict))

#transformation of 1,0 into Yes,No
convert_count<-function(x)
{
  x <- ifelse(x>0,1,0)#if x>0 1 is replaced o.w 0
  x <- factor(x,levels=c(0,1),labels=c("No","Yes"))
}

ptype_train <- apply(ptype_train,MARGIN =2,convert_count)
ptype_test <- apply(ptype_test,MARGIN =2,convert_count)

## training a model on the data
ptype_classifier <- naiveBayes(ptype_train, ptype_train$Product_Type)

## Step 4 - evaluating model performance
ptype_test_pred <- predict(ptype_classifier, ptype_test)

## CONFUSION MATRIX
CrossTable(ptype_test_pred, ptype_test$Product_Type,
           prop.chisq = FALSE, prop.t = FALSE,
           dnn = c('predicted', 'actual'))



#################################################################################################################################

#Model-2 K Means traindatamaster_description vs MedDRA_Preferred_Terms

View(train_data_master)
colSums(is.na(train_data_master))

traindata_clust <- subset(train_data_master,
                                select=c(Product,Outcomes))

traindata_clust <- traindata_clust %>% 
  mutate(Outcomes = strsplit(as.character(Outcomes),",")) %>% 
  unnest(Outcomes)


View(traindata_clust)
str(traindata_clust)

myCorpus <- Corpus(VectorSource(traindata_clust$Outcomes))
inspect(myCorpus[1:5])
myCorpus <- tm_map(myCorpus, content_transformer(tolower))
myCorpus <- tm_map(myCorpus,removePunctuation)
myCorpus <- tm_map(myCorpus, removeWords,stopwords("english"))
myCorpus <- tm_map(myCorpus, stripWhitespace)

inspect(myCorpus)
class(myCorpus)
dtm <- DocumentTermMatrix(myCorpus)
dtm_wt_idf <- weightTfIdf(dtm)

inspect(dtm_wt_idf)


mat_w <- as.matrix(dtm_wt_idf)
as.data.frame(mat_w)
str(mat_w)
View(mat_w)

traindata_clust <-cbind('Product'=traindata_clust$Product,
                           as.data.frame(mat_w))

#traindata_clust[,-1]#removing first row

clus_matr_k <- kmeans(traindata_clust[,-1], centers = 5, nstart = 20)

round(clus_matr_k$centers, digits = 1)

for (i in 1:5) 
{
  cat(paste("cluster ", i, ": ", sep = " "))
  s <- sort(clus_matr_k$centers[i, ], decreasing = T)
  cat(names(s)[1:5], "\n")
}


###########################################################################'''


#Model-3 : K Means traindatamaster_description vs MedDRA_Preferred_Terms
names(train_data_master)

traindatamaster_description <- subset(train_data_master,select=c(Description,MedDRA_Preferred_Terms))
View(traindatamaster_description)

traindatamaster_description <- traindatamaster_description %>% 
  mutate(MedDRA_Preferred_Terms = strsplit(as.character(MedDRA_Preferred_Terms), ",")) %>% 
  unnest(MedDRA_Preferred_Terms)


unique(as.factor(traindatamaster_description$Description))
traindatamaster_description$Description <- as.factor(traindatamaster_description$Description)

src <- VCorpus(VectorSource(traindatamaster_description$MedDRA_Preferred_Terms))

corpus <- tm_map(src, content_transformer(tolower))
corpus <- tm_map(src, removePunctuation)
corpus <- tm_map(src, stripWhitespace)
corpus <- tm_map(src, removeWords, stopwords('english'))


dtm <- DocumentTermMatrix(corpus)
mat_w <- weightTfIdf(dtm)
mat_w <- as.matrix(mat_w)


norm_eucl <- function(m)  m/apply(m,1,function(x) sum(x^2)^.5)

mat_norm <- norm_eucl(mat_w)

'''
#Elbow Method for finding the optimal number of clusters
set.seed(123)
# Compute and plot wss for k = 2 to k = 15.
k.max <- 15
norm_data <- mat_norm
wss <- sapply(1:k.max,function(k){kmeans(norm_data, k, nstart=50,iter.max = 15 )$tot.withinss})
wss
plot(1:k.max, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="K->No. of clusters",
     ylab="clusters within")
'''

set.seed(345)
k <- 9
kmeans_Result$cluster[1:5]
kmeans_Result <- kmeans(mat_norm, k)


count(kmeans_Result$cluster)

#Model Performance
res <- data.frame('actual'=traindatamaster_description$Description, 
                     'predicted'=kmeans_Result$cluster)
res <- res[order(res[,1]),]

res$counter <- 1
res_agg <- aggregate(counter~actual+predicted,data=res,FUN='sum')

res_agg
library(ggplot2)
ggplot(data=res_agg, aes(x=actual, y=predicted, size=counter)) + geom_point()

###############################################################################################################

#Model 4


traindatamaster_outcome <- subset(train_data_master,
                                  select=c(Product_Type,Outcomes))
View(traindatamaster_outcome)

traindatamaster_outcome <- traindatamaster_outcome %>% 
  mutate(Outcomes = strsplit(as.character(Outcomes),",")) %>% 
  unnest(Outcomes)

str(traindatamaster_outcome$Product_Type) #not factor

unique(as.factor(traindatamaster_outcome$Product_Type))
traindatamaster_outcome$Product_Type <- as.factor(traindatamaster_outcome$Product_Type)
#change to factor
View(traindatamaster_outcome$Product_Type)

src <- VCorpus(VectorSource(traindatamaster_outcome$Outcomes))

corpus <- tm_map(src, content_transformer(tolower))
corpus <- tm_map(src, removeNumbers)
corpus <- tm_map(src, removePunctuation)
corpus <- tm_map(src, stripWhitespace)
corpus <- tm_map(src, removeWords, stopwords('english'))


dtm <- DocumentTermMatrix(corpus)
mat_w <- as.matrix(dtm)
as.data.frame(mat_w)
str(mat_w)
View(mat_w)

datamaster_outcome <-cbind('Product_Type'=traindatamaster_outcome$Product_Type,
                           as.data.frame(mat_w))

str(datamaster_outcome)
colnames(datamaster_outcome)
View(datamaster_outcome)


## Split data into Train and Test
Train <- createDataPartition(datamaster_outcome$Product_Type, p=0.6, list=FALSE)
training <- datamaster_outcome[Train, ]
testing <- datamaster_outcome[-Train, ]


## Logistic Regression Model Fit
mod_fit <- train(Product_Type ~ ., data=training, method="glm", family="binomial")

print(mod_fit)

### validation of predicted values
pred = predict(mod_fit, newdata=testing)
accuracy <- table(pred, testing[,"Product_Type"])
sum(diag(accuracy))/sum(accuracy)

pred <-  predict(mod_fit, newdata=testing)
confusionMatrix(data=pred, testing$Product_Type)



###############################################################################################################

#Model 5 SVM Model for the same above
#install.packages("kernlab")
library(kernlab)

## Train the model
model_svm <- ksvm(Product_Type ~ ., 
                  data = training, 
                  kernel = "vanilladot")
                                      
model_svm

## Evaluate model performance
pred <- predict(model_svm, testing)

head(pred)

table(pred,testing$Product_Type)

#install.packages("gmodels")
library(gmodels)
## CONFUSION MATRIX
CrossTable(pred, testing,
           prop.chisq = FALSE, prop.t = FALSE,
           dnn = c('predicted', 'actual'))
















