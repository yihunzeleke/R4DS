
library(readr)
library(randomForest)
library(caret)

churndata <- read_csv("https://raw.githubusercontent.com/yihunzeleke/data_repo/master/Churn%20Data.csv")

churndata$Churn <- as.factor(churndata$Churn)
churndata$Churn <- ifelse(churndata$Churn == 0, "Not-Churn", "Churn")

library(caTools)


set.seed(100)

split <- sample.split(churndata$Churn, SplitRatio = 0.7)

train <- subset(churndata, split == TRUE)
test <- subset(churndata, split == FALSE)

write.csv(train, "train.csv", row.names = F)
write.csv(test, "test.csv", row.names = F)

# Step 2:Train model with logistics regression using glm function
train <- read.csv("train.csv", stringsAsFactors = T)



model <- randomForest(Churn ~ ContractRenewal + CustServCalls + RoamMins , data = train, ntree = 500, mtry = 2, importance = T)


# pred <- ifelse(predict(model, test, type = 'response') >= 0.5,1,0)
# 
# pred <- ifelse(pred > 0.5, 1, 0)

saveRDS(model, "model.rds")

