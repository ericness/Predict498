
#setwd("C:/Users/ericn_000/Dropbox/Education/MS Predictive Analytics/PREDICT 498 Capstone/498 Capstone Project/Data/Kaggle")
setwd("C:/Users/eness/Dropbox/Education/MS Predictive Analytics/PREDICT 498 Capstone/498 Capstone Project/Data/Kaggle")

car.data.training <- read.csv(file="training.csv", header=TRUE)
str(car.data.training)

car.data.training$MMRAcquisitionAuctionAveragePrice <- as.integer(car.data.training$MMRAcquisitionAuctionAveragePrice)
car.data.training$MMRAcquisitionAuctionCleanPrice <- as.integer(car.data.training$MMRAcquisitionAuctionCleanPrice)
car.data.training$MMRAcquisitionRetailAveragePrice <- as.integer(car.data.training$MMRAcquisitionRetailAveragePrice)
car.data.training$MMRAcquisitonRetailCleanPrice <- as.integer(car.data.training$MMRAcquisitonRetailCleanPrice)
car.data.training$MMRCurrentAuctionAveragePrice <- as.integer(car.data.training$MMRCurrentAuctionAveragePrice)
car.data.training$MMRCurrentAuctionCleanPrice <- as.integer(car.data.training$MMRCurrentAuctionCleanPrice)
car.data.training$MMRCurrentRetailAveragePrice <- as.integer(car.data.training$MMRCurrentRetailAveragePrice)
car.data.training$MMRCurrentRetailCleanPrice <- as.integer(car.data.training$MMRCurrentRetailCleanPrice)

unwantedcols <- c("RefId","PurchDate","Model","Trim","SubModel")

car.data.training <- car.data.training[,!(names(car.data.training) %in% unwantedcols)]

logit.model <- glm(IsBadBuy~.,data=car.data.training,family="binomial")

car.data.test <- read.csv(file="test.csv", header=TRUE)

car.data.test$MMRAcquisitionAuctionAveragePrice <- as.integer(car.data.test$MMRAcquisitionAuctionAveragePrice)
car.data.test$MMRAcquisitionAuctionCleanPrice <- as.integer(car.data.test$MMRAcquisitionAuctionCleanPrice)
car.data.test$MMRAcquisitionRetailAveragePrice <- as.integer(car.data.test$MMRAcquisitionRetailAveragePrice)
car.data.test$MMRAcquisitonRetailCleanPrice <- as.integer(car.data.test$MMRAcquisitonRetailCleanPrice)
car.data.test$MMRCurrentAuctionAveragePrice <- as.integer(car.data.test$MMRCurrentAuctionAveragePrice)
car.data.test$MMRCurrentAuctionCleanPrice <- as.integer(car.data.test$MMRCurrentAuctionCleanPrice)
car.data.test$MMRCurrentRetailAveragePrice <- as.integer(car.data.test$MMRCurrentRetailAveragePrice)
car.data.test$MMRCurrentRetailCleanPrice <- as.integer(car.data.test$MMRCurrentRetailCleanPrice)

unwantedcols <- c("PurchDate","Model","Trim","SubModel")

car.data.test <- car.data.test[,!(names(car.data.test) %in% unwantedcols)]

str(car.data.test$Color)
summary(car.data.test$Color)
summary(car.data.training$Color)

car.data.test$Color[car.data.test$Color == "PINK"] <- "NOT AVAIL"
car.data.test$Color[car.data.test$Color == ""] <- "NOT AVAIL"
car.data.test$Color <- as.factor(as.character(car.data.test$Color))

summary(car.data.test$VNST)
car.data.test$VNST[car.data.test$VNST == "WI"] <- "MN"
car.data.test$VNST <- as.factor(as.character(car.data.test$VNST))

car.data.test$IsBadBuyProb <- predict(logit.model, car.data.test, type="response")
car.data.test$IsBadBuy <- 0
car.data.test$IsBadBuy[car.data.test$IsBadBuyProb > 0.5] <- 1

write.csv(car.data.test[c("RefId","IsBadBuy")], "NaiveModelPredictions.csv", row.names=FALSE)
