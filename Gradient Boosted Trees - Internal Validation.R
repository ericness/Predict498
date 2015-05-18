library(magrittr)
library(plyr)
library(dplyr)
library(caret)
library(pROC)
library(gbm)

setwd("C:/Users/ericn_000/Dropbox/Education/MS Predictive Analytics/PREDICT 498 Capstone/498 Capstone Project/Data/Training")
#setwd("C:/Users/eness/Dropbox/Education/MS Predictive Analytics/PREDICT 498 Capstone/498 Capstone Project/Data/Training")

car.data <- read.csv(file="CarLemons_Final_05032015_Training_ECN_01.csv", header=TRUE)
car.data.tbl <- tbl_df(car.data)

car.data.tbl %<>% select(IsBadBuy, Log_VehBCost, StandardMake, StandardModel,
                         MMRAcquisitionRetailAveragePrice, 
                         State,  VehOdo,
                         PurchDayofWeek, Inter_Odo_Age, model_body_style,
                         SubModel_Format, Size) %>%
                mutate(StandardModel = as.factor(gsub("\\s|-|\\&","_",StandardModel,perl=TRUE)))

car.data.tbl.dummies <- select(car.data.tbl,IsBadBuy, StandardMake, StandardModel, PurchDayofWeek, State,
                               model_body_style, SubModel_Format, Size)

dummies <- dummyVars(IsBadBuy ~ ., data = car.data.tbl.dummies)
car.data.tbl.dummies <- as.data.frame((predict(dummies, newdata = car.data.tbl.dummies)))

# car.data.tbl.dummies %<>%
#   select(StandardMake.CHEVROLET, StandardMake.SATURN, 
#          
#          StandardModel.EXPLORER, StandardModel.STRATUS,StandardModel.MAXIMA,
#          
#          StandardModel.PT_CRUISER,StandardModel.GS, 
#          StandardModel.TAURUS, StandardModel.DURANGO,
#          
#          PurchDayofWeek.Monday,State.FL, State.VA,
#          
#          model_body_style.Sedan, SubModel_Format.REGCAB, Size.VAN )

car.data.tbl %<>%
  bind_cols(car.data.tbl.dummies) %>%
  select(-StandardMake,-StandardModel,-PurchDayofWeek, -State, 
         -model_body_style, -SubModel_Format, -Size)

car.data.tbl %<>%
  mutate(IsBadBuy = as.factor(IsBadBuy))

# Build logistic regression model
#rf.model <- randomForest(IsBadBuy~.,data=car.data.tbl,ntree=100, nodesize=5, mtry=9)

#gbm.model <- randomForest(IsBadBuy~.,data=car.data.tbl,ntree=100,  mtry=3)

# n.trees, interaction.depth, shrinkage

gbmGrid <-  expand.grid(interaction.depth = 5, #c(1, 5, 9),
                        n.trees = (3:3)*50,
                        shrinkage = 0.1)

gbm.model <- train(IsBadBuy~.,data=car.data.tbl,method="gbm",
                  trControl=trainControl(method="cv",number=5),
                  verbose=FALSE,tuneGrid=gbmGrid)
                  
#setwd("C:/Users/ericn_000/Dropbox/Education/MS Predictive Analytics/PREDICT 498 Capstone/498 Capstone Project/Data/Kaggle")
#setwd("C:/Users/eness/Dropbox/Education/MS Predictive Analytics/PREDICT 498 Capstone/498 Capstone Project/Data/Kaggle")

car.data.test <- read.csv(file="CarLemons_Final_05032015_Test_ECN_01.csv", header=TRUE)
car.data.test.tbl <- tbl_df(car.data.test)

car.data.test.tbl %<>% select(IsBadBuy, VehicleAge, VehBCost_ind, StandardMake, StandardModel,
                         MMRAcquisitionRetailAveragePrice_ind, 
                         State, CityType, VehOdo_ind,
                         PurchDayofWeek  )

car.data.test.tbl.dummies <- select(car.data.test.tbl,IsBadBuy, StandardMake,StandardModel, PurchDayofWeek, State)

dummies.test <- dummyVars(IsBadBuy ~ ., data = car.data.test.tbl.dummies)
car.data.test.tbl.dummies <- as.data.frame((predict(dummies.test, newdata = car.data.test.tbl.dummies)))

car.data.test.tbl.dummies %<>%
  select(StandardMake.CHEVROLET, StandardMake.SATURN, 
         
         StandardModel.EXPLORER, StandardModel.STRATUS,StandardModel.MAXIMA,
         
         `StandardModel.PT CRUISER`,StandardModel.GS, 
         StandardModel.TAURUS, StandardModel.DURANGO,
         
         PurchDayofWeek.Monday,State.FL, State.VA,
         
         model_body_style.Sedan, SubModel_Format.REGCAB, Size.VAN )

car.data.test.tbl %<>%
  bind_cols(car.data.test.tbl.dummies) %>%
  select(-StandardMake,-StandardModel,-PurchDayofWeek, -State)

car.data.test.tbl %<>%
  mutate(  StandardModel.PTCRUISER = `StandardModel.PT CRUISER`,
         StandardModel.GRANDAM = `StandardModel.GRAND AM`,
         StandardModel.GRANDCHEROKEE = `StandardModel.GRAND CHEROKEE`,
         StandardModel.GRANDMARQUIS = `StandardModel.GRAND MARQUIS`,
         StandardModel.LSERIES = `StandardModel.L SERIES`) %>%
  select( -`StandardModel.PT CRUISER`,
         -`StandardModel.GRAND AM`,
         -`StandardModel.GRAND CHEROKEE`,
    -`StandardModel.GRAND MARQUIS`, -`StandardModel.L SERIES`)

car.data.test.tbl %<>%
  mutate(IsBadBuy = as.factor(IsBadBuy))

#summary(car.data.test.tbl$State)
#car.data.test.tbl$State[car.data.test.tbl$State == "WI"] <- "MN"
#car.data.test.tbl$State <- as.factor(as.character(car.data.test.tbl$State))


car.data.tbl$IsBadBuyProb <- predict(rf.model, car.data.tbl, type="response")

car.data.test.tbl$IsBadBuyProb <- predict(rf.model, car.data.test.tbl, type="response")


# roc.training <- roc(car.data.tbl$IsBadBuy,car.data.tbl$IsBadBuyProb)
# plot(roc.training)
# roc.test <- roc(car.data.test.tbl$IsBadBuy,car.data.test.tbl$IsBadBuyProb)
# plot(roc.test)


car.data.tbl$IsBadBuyPred <- predict(rf.model, car.data.tbl, type="response")
#car.data.tbl$IsBadBuyPred <- 0
#car.data.tbl$IsBadBuyPred[car.data.tbl$IsBadBuyProb > 0.50] <- 1

car.data.test.tbl$IsBadBuyPred <- predict(rf.model, car.data.test.tbl, type="response")
#car.data.test.tbl$IsBadBuyPred <- 0
#car.data.test.tbl$IsBadBuyPred[car.data.test.tbl$IsBadBuyProb > 0.50] <- 1

setwd("../../Models/Random Forest")
sink("ConfusionTables.txt")

summary(rf.model)

training.table <- table(car.data.tbl$IsBadBuy, car.data.tbl$IsBadBuyPred)
confusionMatrix(training.table)

test.table <- table(car.data.test.tbl$IsBadBuy, car.data.test.tbl$IsBadBuyPred)
confusionMatrix(test.table)

sink()
