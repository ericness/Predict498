library(magrittr)
library(dplyr)

setwd("C:/Users/ericn_000/Dropbox/Education/MS Predictive Analytics/PREDICT 498 Capstone/498 Capstone Project/Data/Kaggle")
#setwd("C:/Users/eness/Dropbox/Education/MS Predictive Analytics/PREDICT 498 Capstone/498 Capstone Project/Data/Final")

car.data <- read.csv(file="KaggleTestWithAllDataSourcesAdded.csv", header=TRUE)
car.data.tbl <- tbl_df(car.data)

# Steve's rules
# only buy cars less than 5 years old
# only buy American made cars
car.data.tbl.buyer1 <- mutate(car.data.tbl, IsBadBuy = ifelse(Nationality=="AMERICAN" & VehicleAge < 6, 0, 1)) %>%
    select(RefId,IsBadBuy)
table(car.data.tbl.buyer1$IsBadBuy)

setwd("C:/Users/ericn_000/Dropbox/Education/MS Predictive Analytics/PREDICT 498 Capstone/498 Capstone Project/Data/Submissions")
write.csv(car.data.tbl.buyer1[c("RefId","IsBadBuy")], "Buyer1Predictions.csv", row.names=FALSE)

# Jill's rules
# MMRAcquisitionRetailAveragePrice - MMRAcquisitionAuctionAveragePrice > 1500
car.data.tbl.buyer2 <- mutate(car.data.tbl, IsBadBuy = ifelse(MMRAcquisitionRetailAveragePrice - MMRAcquisitionAuctionAveragePrice > 1500, 0, 1)) %>%
  select(RefId,IsBadBuy)
table(car.data.tbl.buyer2$IsBadBuy)

write.csv(car.data.tbl.buyer2[c("RefId","IsBadBuy")], "Buyer2Predictions.csv", row.names=FALSE)

# Karthik's rules
# only buys cars, not trucks or suvs
# less than 80k miles
car.data.tbl.buyer3 <- mutate(car.data.tbl,
                              IsBadBuy = ifelse(VehOdo < 80000 &
                                                  model_body_style %in% c("Convertible","Coupe","Hatchback","Sedan","Subcompact Car"), 0, 1)) %>%
  select(RefId,IsBadBuy)
table(car.data.tbl.buyer3$IsBadBuy)

write.csv(car.data.tbl.buyer3[c("RefId","IsBadBuy")], "Buyer3Predictions.csv", row.names=FALSE)

# Combined rules by vote
car.data.tbl.allbuyer <- inner_join(car.data.tbl.buyer1,car.data.tbl.buyer2,by="RefId")
car.data.tbl.allbuyer <- inner_join(car.data.tbl.allbuyer,car.data.tbl.buyer3,by="RefId")
car.data.tbl.allbuyer %<>% mutate(IsBadBuy.z = IsBadBuy) %>%
    mutate(IsBadBuy = ifelse(IsBadBuy.x + IsBadBuy.y + IsBadBuy.z >= 2,1,0))

write.csv(car.data.tbl.allbuyer[c("RefId","IsBadBuy")], "AllBuyerPredictions.csv", row.names=FALSE)
