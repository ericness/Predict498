library(magrittr)
library(dplyr)
library(caret)

#setwd("C:/Users/ericn_000/Dropbox/Education/MS Predictive Analytics/PREDICT 498 Capstone/498 Capstone Project/Data/Final")
#setwd("C:/Users/eness/Dropbox/Education/MS Predictive Analytics/PREDICT 498 Capstone/498 Capstone Project/Data/Final")

setwd("C:/Users/ericn_000/Dropbox/Education/MS Predictive Analytics/PREDICT 498 Capstone/498 Capstone Project/Data/Training")

#car.data <- read.csv(file="KaggleTestWithAllDataSourcesAdded.csv", header=TRUE)

#car.data <- read.csv(file="CarLemons_Final_05052015_One_kick_Two_NonKick.csv", header=TRUE)
car.data <- read.csv(file="CarLemons_Final_05032015_Test_ECN_01.csv", header=TRUE)
car.data.tbl <- tbl_df(car.data)

# Steve's rules
# only buy cars less than 5 years old
# only buy American made cars
car.data.tbl.buyer1 <- mutate(car.data.tbl, IsBadBuyPred = ifelse(Nationality=="AMERICAN" & VehicleAge < 6, 0, 1)) 

# Jill's rules
# MMRAcquisitionRetailAveragePrice - MMRAcquisitionAuctionAveragePrice > 1500
car.data.tbl.buyer2 <- mutate(car.data.tbl, IsBadBuyPred = ifelse(MMRAcquisitionRetailAveragePrice - MMRAcquisitionAuctionAveragePrice > 1500, 0, 1)) 

# Karthik's rules
# only buys cars, not trucks or suvs
# less than 80k miles
car.data.tbl.buyer3 <- mutate(car.data.tbl,
                              IsBadBuyPred = ifelse(VehOdo < 80000 &
                                                  model_body_style %in% c("Convertible","Coupe","Hatchback","Sedan","Subcompact Car"), 0, 1))

# Combined rules by vote
car.data.tbl.buyer1.forjoin <- select(car.data.tbl.buyer1,Refid,IsBadBuy,IsBadBuyPred)
car.data.tbl.buyer2.forjoin <- select(car.data.tbl.buyer2,Refid,IsBadBuy,IsBadBuyPred)
car.data.tbl.buyer3.forjoin <- select(car.data.tbl.buyer3,Refid,IsBadBuy,IsBadBuyPred)

car.data.tbl.allbuyer <- inner_join(car.data.tbl.buyer1.forjoin,car.data.tbl.buyer2.forjoin,by="Refid")
car.data.tbl.allbuyer <- inner_join(car.data.tbl.allbuyer,car.data.tbl.buyer3.forjoin,by="Refid")
car.data.tbl.allbuyer %<>%
    mutate(IsBadBuyPred.z = IsBadBuyPred) %>%
    mutate(IsBadBuy = ifelse(IsBadBuyPred.x + IsBadBuyPred.y + IsBadBuyPred.z >= 2,1,0))

# write results to file

setwd("C:/Users/ericn_000/Dropbox/Education/MS Predictive Analytics/PREDICT 498 Capstone/498 Capstone Project/Models/Current Buyers")

sink("ConfusionMatrix.txt")

confusionMatrix(table(car.data.tbl.buyer1$IsBadBuy,car.data.tbl.buyer1$IsBadBuyPred))

confusionMatrix(table(car.data.tbl.buyer2$IsBadBuy,car.data.tbl.buyer2$IsBadBuyPred))

confusionMatrix(table(car.data.tbl.buyer3$IsBadBuy,car.data.tbl.buyer3$IsBadBuyPred))

confusionMatrix(table(car.data.tbl.allbuyer$IsBadBuy,car.data.tbl.allbuyer$IsBadBuyPred))

sink()

#write.csv(car.data.tbl.buyer1[c("RefId","IsBadBuy")], "Buyer1Predictions.csv", row.names=FALSE)
#
# write.csv(car.data.tbl.buyer2[c("RefId","IsBadBuy")], "Buyer2Predictions.csv", row.names=FALSE)
# 
# table(car.data.tbl.buyer3$IsBadBuy)
# 
# write.csv(car.data.tbl.buyer3[c("RefId","IsBadBuy")], "Buyer3Predictions.csv", row.names=FALSE)
# 
# write.csv(car.data.tbl.allbuyer[c("RefId","IsBadBuy")], "AllBuyerPredictions.csv", row.names=FALSE)
