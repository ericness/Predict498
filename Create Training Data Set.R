library(magrittr)
library(dplyr)
library(caret)

setwd("C:/Users/ericn_000/Dropbox/Education/MS Predictive Analytics/PREDICT 498 Capstone/498 Capstone Project/Data/Final")
#setwd("C:/Users/eness/Dropbox/Education/MS Predictive Analytics/PREDICT 498 Capstone/498 Capstone Project/Data/Final")

car.data <- read.csv(file="CarLemons_Final_05032015.csv", header=TRUE)
car.data.tbl <- tbl_df(car.data)

car.data.tbl.bad <- filter(car.data.tbl, IsBadBuy == 1)
car.data.tbl.good <- filter(car.data.tbl, IsBadBuy == 0)

set.seed(108)

car.data.tbl.bad.trainingindex <- createDataPartition(car.data.tbl.bad$StandardModel, p=.75, list = FALSE,times = 1)
car.data.tbl.bad.training <- car.data.tbl.bad[car.data.tbl.bad.trainingindex,]
car.data.tbl.bad.test <- car.data.tbl.bad[-car.data.tbl.bad.trainingindex,]

car.data.tbl.good.sampleindex <- createDataPartition(car.data.tbl.good$StandardModel, p=.33, list = FALSE,times = 1)
car.data.tbl.good.sample <- car.data.tbl.good[car.data.tbl.good.sampleindex,]

car.data.tbl.good.trainingindex <- createDataPartition(car.data.tbl.good.sample$StandardModel, p=.75, list = FALSE,times = 1)
car.data.tbl.good.training <- car.data.tbl.good.sample[car.data.tbl.good.trainingindex,]
car.data.tbl.good.test <- car.data.tbl.good.sample[-car.data.tbl.good.trainingindex,]

car.data.tbl.training <- bind_rows(car.data.tbl.bad.training, car.data.tbl.good.training)
car.data.tbl.test <- bind_rows(car.data.tbl.bad.test, car.data.tbl.good.test)

setwd("C:/Users/ericn_000/Dropbox/Education/MS Predictive Analytics/PREDICT 498 Capstone/498 Capstone Project/Data/Training")

write.csv(car.data.tbl.training, "CarLemons_Final_05032015_Training_ECN_01.csv", row.names=FALSE)
write.csv(car.data.tbl.test, "CarLemons_Final_05032015_Test_ECN_01.csv", row.names=FALSE)
