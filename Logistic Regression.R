library(magrittr)
library(dplyr)
library(caret)

setwd("C:/Users/ericn_000/Dropbox/Education/MS Predictive Analytics/PREDICT 498 Capstone/498 Capstone Project/Data/Training")
#setwd("C:/Users/eness/Dropbox/Education/MS Predictive Analytics/PREDICT 498 Capstone/498 Capstone Project/Data/Training")

car.data <- read.csv(file="CarLemons_Final_05032015_Training_ECN.csv", header=TRUE)
car.data.tbl <- tbl_df(car.data)

car.data.tbl %<>% select(IsBadBuy, VehicleAge, VehBCost, StandardMake, StandardModel,
                         MMRAcquisitionRetailAveragePrice, 
                         State, Division, CityType, model_body_style, VehOdo,
                         PurchDayofWeek  )

car.data.tbl.dummies <- select(car.data.tbl,IsBadBuy, StandardMake, StandardModel, PurchDayofWeek, State)

dummies <- dummyVars(IsBadBuy ~ ., data = car.data.tbl.dummies)
car.data.tbl.dummies <- as.data.frame((predict(dummies, newdata = car.data.tbl.dummies)))

car.data.tbl.dummies %<>%
    select(StandardMake.CHEVROLET,StandardMake.DODGE,StandardMake.HONDA,
           StandardMake.ISUZU,StandardMake.SCION,StandardMake.TOYOTA,        
           
           StandardModel.EXPLORER, StandardModel.STRATUS,StandardModel.MAXIMA,

           `StandardModel.PT CRUISER`,StandardModel.GS, StandardModel.WINDSTAR ,
           
           # 3 star variables
           StandardModel.ALERO, StandardModel.AVENGER, StandardModel.AVEO, StandardModel.AVIATOR,StandardModel.CAVALIER,
           StandardModel.CENTURY, StandardModel.COOPER, StandardModel.ESCORT, StandardModel.EXPEDITION, StandardModel.FOCUS,
           `StandardModel.GRAND AM`, `StandardModel.GRAND CHEROKEE`, `StandardModel.GRAND MARQUIS`, `StandardModel.L SERIES`,
           StandardModel.MONTANA, StandardModel.MONTERO, StandardModel.MOUNTAINEER, StandardModel.MUSTANG, StandardModel.NEON,
           StandardModel.PROTEGE, StandardModel.SABLE, StandardModel.SORENTO, StandardModel.TRACKER, StandardModel.VOYAGER,
           StandardModel.XTERRA,
           StandardModel.MONTANA, StandardModel.MONTERO, StandardModel.MOUNTAINEER,StandardModel.PROTEGE,
           
           # 2 star variables
           #StandardModel.4RUNNER, StandardModel.BONNEVILLE, StandardModel.BRAVADA, StandardModel.CHEROKEE, StandardModel.FORENZA,
           #StandardModel.FRONTIER, StandardModel.G35, StandardModel.G5, `StandardModel.GRAND VITARA`, StandardModel.GS,
           #StandardModel.INTREPID, StandardModel.LESABRE, StandardModel.LS, StandardModel.M, StandardModel.MAZDA6, StandardModel.MONTEREY,
           #StandardModel.PATHFINDER, StandardModel.RENDEZVOUS, `StandardModel.S SERIES`, StandardModel.SENTRA, StandardModel.SUNFIRE,
           #StandardModel.TAURUS, StandardModel.TIBURON, StandardModel.VENTURE, `StandardModel.XL-7`,
           
           PurchDayofWeek.Monday,PurchDayofWeek.Tuesday,State.TX)

car.data.tbl %<>%
  bind_cols(car.data.tbl.dummies) %>%
  select(-StandardMake,-StandardModel,-PurchDayofWeek, -State)


# Build logistic regression model
logit.model <- glm(IsBadBuy~.,data=car.data.tbl,family="binomial")
summary(logit.model)

setwd("C:/Users/ericn_000/Dropbox/Education/MS Predictive Analytics/PREDICT 498 Capstone/498 Capstone Project/Data/Kaggle")
#setwd("C:/Users/eness/Dropbox/Education/MS Predictive Analytics/PREDICT 498 Capstone/498 Capstone Project/Data/Kaggle")

car.data.test <- read.csv(file="KaggleTestWithAllDataSourcesAdded.csv", header=TRUE)
car.data.test.tbl <- tbl_df(car.data.test)


car.data.test.tbl.dummies <- select(car.data.test.tbl,IsBadBuy, StandardMake,StandardModel, PurchDayofWeek, State)

dummies.test <- dummyVars(IsBadBuy ~ ., data = car.data.test.tbl.dummies)
car.data.test.tbl.dummies <- as.data.frame((predict(dummies.test, newdata = car.data.test.tbl.dummies)))

car.data.test.tbl.dummies %<>%
  select(StandardMake.CHEVROLET,StandardMake.DODGE,StandardMake.HONDA,
         StandardMake.ISUZU,StandardMake.SCION,StandardMake.TOYOTA,  
         
         StandardModel.EXPLORER, StandardModel.STRATUS,StandardModel.MAXIMA,
         
         `StandardModel.PT CRUISER`,StandardModel.GS, StandardModel.WINDSTAR ,
         
         StandardModel.ALERO, StandardModel.AVENGER, StandardModel.AVEO, StandardModel.AVIATOR,StandardModel.CAVALIER,
         StandardModel.CENTURY, StandardModel.COOPER, StandardModel.ESCORT, StandardModel.EXPEDITION, StandardModel.FOCUS,
         `StandardModel.GRAND AM`, `StandardModel.GRAND CHEROKEE`, `StandardModel.GRAND MARQUIS`, `StandardModel.L SERIES`,
         StandardModel.MONTANA, StandardModel.MONTERO, StandardModel.MOUNTAINEER, StandardModel.MUSTANG, StandardModel.NEON,
         StandardModel.PROTEGE, StandardModel.SABLE, StandardModel.SORENTO, StandardModel.TRACKER, StandardModel.VOYAGER,
         StandardModel.XTERRA,
         StandardModel.MONTANA, StandardModel.MONTERO, StandardModel.MOUNTAINEER,StandardModel.Protege,
         
         # 2 star variables
         #StandardModel.4RUNNER, StandardModel.BONNEVILLE, StandardModel.BRAVADA, StandardModel.CHEROKEE, StandardModel.FORENZA,
         #StandardModel.FRONTIER, StandardModel.G35, StandardModel.G5, `StandardModel.GRAND VITARA`, StandardModel.GS,
         #StandardModel.INTREPID, StandardModel.LESABRE, StandardModel.LS, StandardModel.M, StandardModel.MAZDA6, StandardModel.MONTEREY,
         #StandardModel.PATHFINDER, StandardModel.RENDEZVOUS, `StandardModel.S SERIES`, StandardModel.SENTRA, StandardModel.SUNFIRE,
         #StandardModel.TAURUS, StandardModel.TIBURON, StandardModel.VENTURE, `StandardModel.XL-7`,
         
         PurchDayofWeek.Monday,PurchDayofWeek.Tuesday,State.TX)

car.data.test.tbl %<>%
  bind_cols(car.data.test.tbl.dummies) %>%
  select(-StandardModel,-PurchDayofWeek, -State)


#summary(car.data.test.tbl$State)
#car.data.test.tbl$State[car.data.test.tbl$State == "WI"] <- "MN"
#car.data.test.tbl$State <- as.factor(as.character(car.data.test.tbl$State))

car.data.test.tbl$IsBadBuyProb <- predict(logit.model, car.data.test.tbl, type="response")
car.data.test.tbl$IsBadBuy <- 0
car.data.test.tbl$IsBadBuy[car.data.test.tbl$IsBadBuyProb > 0.40] <- 1

setwd("C:/Users/ericn_000/Dropbox/Education/MS Predictive Analytics/PREDICT 498 Capstone/498 Capstone Project/Data/Submissions")
#setwd("C:/Users/eness/Dropbox/Education/MS Predictive Analytics/PREDICT 498 Capstone/498 Capstone Project/Data/Submissions")
write.csv(car.data.test.tbl[c("RefId","IsBadBuy")], "LogisticModelCutover40Predictions_12.csv", row.names=FALSE)

