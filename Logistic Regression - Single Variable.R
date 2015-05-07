library(magrittr)
library(dplyr)
library(caret)

setwd("C:/Users/ericn_000/Dropbox/Education/MS Predictive Analytics/PREDICT 498 Capstone/498 Capstone Project/Data/Training")
#setwd("C:/Users/eness/Dropbox/Education/MS Predictive Analytics/PREDICT 498 Capstone/498 Capstone Project/Data/Training")

car.data <- read.csv(file="CarLemons_Final_05032015_Training_ECN.csv", header=TRUE)
car.data.tbl <- tbl_df(car.data)

# car.data.tbl %<>% select(IsBadBuy, VehicleAge, VehBCost, StandardModel, StandardModel,
#                          MMRAcquisitionRetailAveragePrice, 
#                          State, Division, CityType, model_body_style, VehOdo,
#                          PurchDayofWeek  )
car.data.tbl %<>% select(IsBadBuy, StandardModel )

car.data.tbl.dummies <- select(car.data.tbl,IsBadBuy, StandardModel)

dummies <- dummyVars(IsBadBuy ~ ., data = car.data.tbl.dummies)
car.data.tbl.dummies <- as.data.frame((predict(dummies, newdata = car.data.tbl.dummies)))

car.data.tbl %<>%
  bind_cols(car.data.tbl.dummies) %>%
  select(-StandardModel)

car.data.tbl %<>%
  select(-StandardModel.300)

# Build logistic regression model
logit.model <- glm(IsBadBuy~.,data=car.data.tbl,family="binomial")
summary(logit.model)
