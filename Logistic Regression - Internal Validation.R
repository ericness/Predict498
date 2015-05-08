library(magrittr)
library(dplyr)
library(caret)
library(pROC)

setwd("C:/Users/ericn_000/Dropbox/Education/MS Predictive Analytics/PREDICT 498 Capstone/498 Capstone Project/Data/Training")
#setwd("C:/Users/eness/Dropbox/Education/MS Predictive Analytics/PREDICT 498 Capstone/498 Capstone Project/Data/Training")

car.data <- read.csv(file="CarLemons_Final_05032015_Training_ECN_01.csv", header=TRUE)
car.data.tbl <- tbl_df(car.data)

car.data.tbl %<>% select(IsBadBuy, Log_VehBCost, StandardMake, StandardModel,
                         MMRAcquisitionRetailAveragePrice, 
                         State,  VehOdo,
                         PurchDayofWeek, Inter_Odo_Age, model_body_style,
                         SubModel_Format, Size)

      #  VehicleAge, CityType, Hurricane_ind, model_engine_torque_rpm, mode_engine_valves_per_cyl, Division, model_engine_type

car.data.tbl.dummies <- select(car.data.tbl,IsBadBuy, StandardMake, StandardModel, PurchDayofWeek, State,
                               model_body_style, SubModel_Format, Size) # , Division, model_engine_type

dummies <- dummyVars(IsBadBuy ~ ., data = car.data.tbl.dummies)
car.data.tbl.dummies <- as.data.frame((predict(dummies, newdata = car.data.tbl.dummies)))

car.data.tbl.dummies %<>%
  select(StandardMake.CHEVROLET, StandardMake.SATURN, # StandardMake.SUZUKI,
         
         #StandardMake.DODGE,StandardMake.HONDA,
         #StandardMake.ISUZU,StandardMake.SCION,StandardMake.TOYOTA,        
         
         StandardModel.EXPLORER, StandardModel.STRATUS,StandardModel.MAXIMA,
         
         `StandardModel.PT CRUISER`,StandardModel.GS, 
         StandardModel.TAURUS, StandardModel.DURANGO,
         
         # StandardModel.WINDSTAR , StandardModel.MALIBU, 
         
         # 3 star variables
         #StandardModel.ALERO, StandardModel.AVENGER, StandardModel.AVEO, StandardModel.AVIATOR,StandardModel.CAVALIER,
         #StandardModel.CENTURY, StandardModel.COOPER, StandardModel.ESCORT, StandardModel.EXPEDITION, StandardModel.FOCUS,
         #`StandardModel.GRAND AM`, `StandardModel.GRAND CHEROKEE`, `StandardModel.GRAND MARQUIS`, `StandardModel.L SERIES`,
         #StandardModel.MONTANA, StandardModel.MONTERO, StandardModel.MOUNTAINEER, StandardModel.MUSTANG, StandardModel.NEON,
         #StandardModel.PROTEGE, StandardModel.SABLE, StandardModel.SORENTO, StandardModel.TRACKER, StandardModel.VOYAGER,
         #StandardModel.XTERRA,
         #StandardModel.MONTANA, StandardModel.MONTERO, StandardModel.MOUNTAINEER,StandardModel.PROTEGE,
         
         # 2 star variables
         #StandardModel.4RUNNER, StandardModel.BONNEVILLE, StandardModel.BRAVADA, StandardModel.CHEROKEE, StandardModel.FORENZA,
         #StandardModel.FRONTIER, StandardModel.G35, StandardModel.G5, `StandardModel.GRAND VITARA`, StandardModel.GS,
         #StandardModel.INTREPID, StandardModel.LESABRE, StandardModel.LS, StandardModel.M, StandardModel.MAZDA6, StandardModel.MONTEREY,
         #StandardModel.PATHFINDER, StandardModel.RENDEZVOUS, `StandardModel.S SERIES`, StandardModel.SENTRA, StandardModel.SUNFIRE,
         #StandardModel.TAURUS, StandardModel.TIBURON, StandardModel.VENTURE, `StandardModel.XL-7`,
         
         PurchDayofWeek.Monday,State.FL, State.VA,
         
         # PurchDayofWeek.Tuesday, State.TX, model_body_style.Hatchback, SubModel_Format.WAGON, `model_engine_type.in-line`, `Division.West South Central`
         
         model_body_style.Sedan, SubModel_Format.REGCAB, Size.VAN )

car.data.tbl %<>%
  bind_cols(car.data.tbl.dummies) %>%
  select(-StandardMake,-StandardModel,-PurchDayofWeek, -State, 
         -model_body_style, -SubModel_Format, -Size) # , -Division, -model_engine_type

# check variable selection
#car.data.tbl %<>% select(-IsBadBuyProb, -IsBadBuyPred)
#logit.vars <- regsubsets(IsBadBuy~.,data=car.data.tbl,method="seqrep",nvmax=20)


# Build logistic regression model
logit.model <- glm(IsBadBuy~.,data=car.data.tbl,family="binomial")

#setwd("C:/Users/ericn_000/Dropbox/Education/MS Predictive Analytics/PREDICT 498 Capstone/498 Capstone Project/Data/Kaggle")
#setwd("C:/Users/eness/Dropbox/Education/MS Predictive Analytics/PREDICT 498 Capstone/498 Capstone Project/Data/Kaggle")

car.data.test <- read.csv(file="CarLemons_Final_05032015_Test_ECN_01.csv", header=TRUE)
car.data.test.tbl <- tbl_df(car.data.test)

car.data.test.tbl %<>% select(IsBadBuy, Log_VehBCost, StandardMake, StandardModel,
                         MMRAcquisitionRetailAveragePrice, 
                         State,  VehOdo,
                         PurchDayofWeek, Inter_Odo_Age, model_body_style,
                         SubModel_Format, Size)

#  VehicleAge, CityType, Hurricane_ind, model_engine_torque_rpm, mode_engine_valves_per_cyl, Division, model_engine_type

car.data.test.tbl.dummies <- select(car.data.test.tbl,IsBadBuy, StandardMake, StandardModel, PurchDayofWeek, State,
                               model_body_style, SubModel_Format, Size) # , Division, model_engine_type

dummies <- dummyVars(IsBadBuy ~ ., data = car.data.test.tbl.dummies)
car.data.test.tbl.dummies <- as.data.frame((predict(dummies, newdata = car.data.test.tbl.dummies)))

car.data.test.tbl.dummies %<>%
  select(StandardMake.CHEVROLET, StandardMake.SATURN, # StandardMake.SUZUKI,
         
         #StandardMake.DODGE,StandardMake.HONDA,
         #StandardMake.ISUZU,StandardMake.SCION,StandardMake.TOYOTA,        
         
         StandardModel.EXPLORER, StandardModel.STRATUS,StandardModel.MAXIMA,
         
         `StandardModel.PT CRUISER`,StandardModel.GS, 
         StandardModel.TAURUS, StandardModel.DURANGO,
         
         # StandardModel.WINDSTAR , StandardModel.MALIBU, 
         
         # 3 star variables
         #StandardModel.ALERO, StandardModel.AVENGER, StandardModel.AVEO, StandardModel.AVIATOR,StandardModel.CAVALIER,
         #StandardModel.CENTURY, StandardModel.COOPER, StandardModel.ESCORT, StandardModel.EXPEDITION, StandardModel.FOCUS,
         #`StandardModel.GRAND AM`, `StandardModel.GRAND CHEROKEE`, `StandardModel.GRAND MARQUIS`, `StandardModel.L SERIES`,
         #StandardModel.MONTANA, StandardModel.MONTERO, StandardModel.MOUNTAINEER, StandardModel.MUSTANG, StandardModel.NEON,
         #StandardModel.PROTEGE, StandardModel.SABLE, StandardModel.SORENTO, StandardModel.TRACKER, StandardModel.VOYAGER,
         #StandardModel.XTERRA,
         #StandardModel.MONTANA, StandardModel.MONTERO, StandardModel.MOUNTAINEER,StandardModel.PROTEGE,
         
         # 2 star variables
         #StandardModel.4RUNNER, StandardModel.BONNEVILLE, StandardModel.BRAVADA, StandardModel.CHEROKEE, StandardModel.FORENZA,
         #StandardModel.FRONTIER, StandardModel.G35, StandardModel.G5, `StandardModel.GRAND VITARA`, StandardModel.GS,
         #StandardModel.INTREPID, StandardModel.LESABRE, StandardModel.LS, StandardModel.M, StandardModel.MAZDA6, StandardModel.MONTEREY,
         #StandardModel.PATHFINDER, StandardModel.RENDEZVOUS, `StandardModel.S SERIES`, StandardModel.SENTRA, StandardModel.SUNFIRE,
         #StandardModel.TAURUS, StandardModel.TIBURON, StandardModel.VENTURE, `StandardModel.XL-7`,
         
         PurchDayofWeek.Monday,State.FL, State.VA,
         
         # PurchDayofWeek.Tuesday, State.TX, model_body_style.Hatchback, SubModel_Format.WAGON, `model_engine_type.in-line`, `Division.West South Central`
         
         model_body_style.Sedan, SubModel_Format.REGCAB, Size.VAN )

car.data.test.tbl %<>%
  bind_cols(car.data.test.tbl.dummies) %>%
  select(-StandardMake,-StandardModel,-PurchDayofWeek, -State, 
         -model_body_style, -SubModel_Format, -Size) # , -Division, -model_engine_type

#summary(car.data.test.tbl$State)
#car.data.test.tbl$State[car.data.test.tbl$State == "WI"] <- "MN"
#car.data.test.tbl$State <- as.factor(as.character(car.data.test.tbl$State))


#car.data.tbl$IsBadBuyProb <- predict(logit.model, car.data.tbl, type="response")

#car.data.test.tbl$IsBadBuyProb <- predict(logit.model, car.data.test.tbl, type="response")


# roc.training <- roc(car.data.tbl$IsBadBuy,car.data.tbl$IsBadBuyProb)
# plot(roc.training)
# roc.test <- roc(car.data.test.tbl$IsBadBuy,car.data.test.tbl$IsBadBuyProb)
# plot(roc.test)


car.data.tbl$IsBadBuyProb <- predict(logit.model, car.data.tbl, type="response")
car.data.tbl$IsBadBuyPred <- 0
car.data.tbl$IsBadBuyPred[car.data.tbl$IsBadBuyProb > 0.60] <- 1

car.data.test.tbl$IsBadBuyProb <- predict(logit.model, car.data.test.tbl, type="response")
car.data.test.tbl$IsBadBuyPred <- 0
car.data.test.tbl$IsBadBuyPred[car.data.test.tbl$IsBadBuyProb > 0.60] <- 1

setwd("../../Models/Logistic Regression")
sink("ConfusionTables.txt")

#summary(logit.vars)

summary(logit.model)

training.table <- table(car.data.tbl$IsBadBuy, car.data.tbl$IsBadBuyPred)
confusionMatrix(training.table)

test.table <- table(car.data.test.tbl$IsBadBuy, car.data.test.tbl$IsBadBuyPred)
confusionMatrix(test.table)

sink()
