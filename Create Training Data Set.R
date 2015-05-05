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
car.data.tbl.good.sampleindex <- createDataPartition(car.data.tbl.good$StandardModel, p=.33, list = FALSE,times = 1)

car.data.tbl.good.sample <- car.data.tbl.good[car.data.tbl.good.sampleindex,]

car.data.tbl.training <- bind_rows(car.data.tbl.bad, car.data.tbl.good.sample)

setwd("C:/Users/ericn_000/Dropbox/Education/MS Predictive Analytics/PREDICT 498 Capstone/498 Capstone Project/Data/Training")
write.csv(car.data.tbl.training, "CarLemons_Final_05032015_Training_ECN.csv", row.names=FALSE)
