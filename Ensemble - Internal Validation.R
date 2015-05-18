library(magrittr)
library(dplyr)
library(caret)
library(pROC)
library(randomForest)

library(kernlab)
library(e1071)

setwd("C:/Users/ericn_000/Dropbox/Education/MS Predictive Analytics/PREDICT 498 Capstone/498 Capstone Project/Data/Training")
#setwd("C:/Users/eness/Dropbox/Education/MS Predictive Analytics/PREDICT 498 Capstone/498 Capstone Project/Data/Training")

car.data <- read.csv(file="CarLemons_Final_05032015_Training_ECN_01.csv", header=TRUE)
car.data.tbl <- tbl_df(car.data)

car.data.tbl %<>% select(IsBadBuy, Log_VehBCost, StandardMake, StandardModel,
                         MMRAcquisitionRetailAveragePrice, State,  VehOdo,
                         PurchDayofWeek, Inter_Odo_Age, model_body_style,
                         SubModel_Format, Size) %>%
                    mutate(StandardModel = as.factor(gsub("\\s|-|\\&","_",StandardModel,perl=TRUE)))

car.data.tbl.dummies <- select(car.data.tbl,IsBadBuy, StandardMake, StandardModel, PurchDayofWeek, State,
                               model_body_style, SubModel_Format, Size)

dummies <- dummyVars(IsBadBuy ~ ., data = car.data.tbl.dummies)
car.data.tbl.dummies <- as.data.frame((predict(dummies, newdata = car.data.tbl.dummies)))

car.data.tbl.dummies %<>%
  select(StandardMake.CHEVROLET, StandardMake.SATURN,
                  
         StandardModel.EXPLORER, StandardModel.STRATUS,StandardModel.MAXIMA,
         
         StandardModel.PT_CRUISER,StandardModel.GS, 
         StandardModel.TAURUS, StandardModel.DURANGO,
         
         # StandardModel.WINDSTAR , StandardModel.MALIBU, 
          PurchDayofWeek.Monday,State.FL, State.VA,
         
         model_body_style.Sedan, SubModel_Format.REGCAB, Size.VAN )

car.data.tbl %<>%
  bind_cols(car.data.tbl.dummies) %>%
  select(-StandardMake,-StandardModel,-PurchDayofWeek, -State, 
         -model_body_style, -SubModel_Format, -Size)

car.data.tbl %<>%
  mutate(IsBadBuy = as.factor(IsBadBuy))

# Build random forest model
#rf.model <- randomForest(IsBadBuy~.,data=car.data.tbl,ntree=100, nodesize=5, mtry=9)

rf.model <- randomForest(IsBadBuy~.,data=car.data.tbl,ntree=100,  mtry=3)

# rf.model <- train(IsBadBuy~.,data=car.data.tbl,method="rf",
#                   trControl=trainControl(method="cv",number=5),
#                   allowParallel=TRUE)

# build linear regression model
logit.model <- glm(IsBadBuy~.,data=car.data.tbl,family="binomial")

# build support vector machine
# svm.model <- train(IsBadBuy~.,data=car.data.tbl,method="svmLinear",
#                    trControl=trainControl(method="cv",number=5))
svm.model <- svm(IsBadBuy~.,data=car.data.tbl)


# load test data set
car.data.test <- read.csv(file="CarLemons_Final_05032015_Test_ECN_01.csv", header=TRUE)
car.data.test.tbl <- tbl_df(car.data.test)

car.data.test.tbl %<>% select(IsBadBuy, Log_VehBCost, StandardMake, StandardModel,
                         MMRAcquisitionRetailAveragePrice, State,  VehOdo,
                         PurchDayofWeek, Inter_Odo_Age, model_body_style,
                         SubModel_Format, Size) %>%
                      mutate(StandardModel = as.factor(gsub("\\s|-|\\&","_",StandardModel,perl=TRUE)))

car.data.test.tbl.dummies <- select(car.data.test.tbl,IsBadBuy, StandardMake, StandardModel, PurchDayofWeek, State,
                               model_body_style, SubModel_Format, Size)

dummies <- dummyVars(IsBadBuy ~ ., data = car.data.test.tbl.dummies)
car.data.test.tbl.dummies <- as.data.frame((predict(dummies, newdata = car.data.test.tbl.dummies)))

car.data.test.tbl.dummies %<>%
  select(StandardMake.CHEVROLET, StandardMake.SATURN,
         
         StandardModel.EXPLORER, StandardModel.STRATUS,StandardModel.MAXIMA,
         
         StandardModel.PT_CRUISER,StandardModel.GS, 
         StandardModel.TAURUS, StandardModel.DURANGO,
         
         # StandardModel.WINDSTAR , StandardModel.MALIBU, 
         PurchDayofWeek.Monday,State.FL, State.VA,
         
         model_body_style.Sedan, SubModel_Format.REGCAB, Size.VAN )

car.data.test.tbl %<>%
  bind_cols(car.data.test.tbl.dummies) %>%
  select(-StandardMake,-StandardModel,-PurchDayofWeek, -State, 
         -model_body_style, -SubModel_Format, -Size)

car.data.test.tbl %<>%
  mutate(IsBadBuy = as.factor(IsBadBuy))

# make predictions for all models
car.data.tbl$IsBadBuyPred.rf <- as.double(predict(rf.model, car.data.tbl, type="response")) - 1
car.data.test.tbl$IsBadBuyPred.rf <- as.double(predict(rf.model, car.data.test.tbl, type="response")) - 1


car.data.tbl$IsBadBuyProb.logit <- predict(logit.model, car.data.tbl, type="response")
car.data.tbl$IsBadBuyPred.logit <- 0
car.data.tbl$IsBadBuyPred.logit[car.data.tbl$IsBadBuyProb.logit > 0.50] <- 1

car.data.test.tbl$IsBadBuyProb.logit <- predict(logit.model, car.data.test.tbl, type="response")
car.data.test.tbl$IsBadBuyPred.logit <- 0
car.data.test.tbl$IsBadBuyPred.logit[car.data.test.tbl$IsBadBuyProb.logit > 0.50] <- 1

car.data.tbl$IsBadBuyPred.svm <- as.double(predict(svm.model, car.data.tbl, type="response")) - 1
car.data.test.tbl$IsBadBuyPred.svm <- as.double(predict(svm.model, car.data.test.tbl, type="response")) - 1

car.data.tbl %<>%
  mutate(IsBadBuyPred.ensemble = ifelse(IsBadBuyPred.rf + IsBadBuyPred.logit + IsBadBuyPred.svm >= 2,1,0))
car.data.test.tbl %<>%
  mutate(IsBadBuyPred.ensemble = ifelse(IsBadBuyPred.rf + IsBadBuyPred.logit + IsBadBuyPred.svm >= 2,1,0))


# roc.training <- roc(car.data.tbl$IsBadBuy,car.data.tbl$IsBadBuyProb)
# plot(roc.training)
# roc.test <- roc(car.data.test.tbl$IsBadBuy,car.data.test.tbl$IsBadBuyProb)
# plot(roc.test)

setwd("../../Models/Ensemble")
sink("ConfusionTables.txt")

summary(rf.model)

summary(logit.model)

summary(svm.model)

training.table.rf <- table(car.data.tbl$IsBadBuy, car.data.tbl$IsBadBuyPred.rf)
confusionMatrix(training.table.rf)

test.table.rf <- table(car.data.test.tbl$IsBadBuy, car.data.test.tbl$IsBadBuyPred.rf)
confusionMatrix(test.table.rf)

training.table.logit <- table(car.data.tbl$IsBadBuy, car.data.tbl$IsBadBuyPred.logit)
confusionMatrix(training.table.logit)

test.table.logit <- table(car.data.test.tbl$IsBadBuy, car.data.test.tbl$IsBadBuyPred.logit)
confusionMatrix(test.table.logit)

training.table.svm <- table(car.data.tbl$IsBadBuy, car.data.tbl$IsBadBuyPred.svm)
confusionMatrix(training.table.svm)

test.table.svm <- table(car.data.test.tbl$IsBadBuy, car.data.test.tbl$IsBadBuyPred.svm)
confusionMatrix(test.table.svm)

training.table.ensemble <- table(car.data.tbl$IsBadBuy, car.data.tbl$IsBadBuyPred.ensemble)
confusionMatrix(training.table.ensemble)

test.table.ensemble <- table(car.data.test.tbl$IsBadBuy, car.data.test.tbl$IsBadBuyPred.ensemble)
confusionMatrix(test.table.ensemble)

sink()

# get results for kaggle test set




