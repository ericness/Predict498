library(magrittr)
library(dplyr)
library(leaps)
library(glmnet)
library(rpart)
library(rpart.plot)

#setwd("C:/Users/ericn_000/Dropbox/Education/MS Predictive Analytics/PREDICT 498 Capstone/498 Capstone Project/Data/Final")
#setwd("C:/Users/eness/Dropbox/Education/MS Predictive Analytics/PREDICT 498 Capstone/498 Capstone Project/Data/Final")

setwd("C:/Users/ericn_000/Dropbox/Education/MS Predictive Analytics/PREDICT 498 Capstone/498 Capstone Project/Data/Training")
#setwd("C:/Users/eness/Dropbox/Education/MS Predictive Analytics/PREDICT 498 Capstone/498 Capstone Project/Data/Training")

#car.data <- read.csv(file="KaggleTrainingWithAddedCarDataAndDependent.csv", header=TRUE)
#car.data <- read.csv(file="CarLemons_Final_04212015.csv", header=TRUE)


car.data <- read.csv(file="CarLemons_Final_05032015_Training_ECN.csv", header=TRUE)


####################

car.data$IsBadBuy <- as.factor(car.data$IsBadBuy)

car.data.tbl <- tbl_df(car.data)

# car.data.tbl %<>% select(IsBadBuy, CityType, Color, Division, FacilityCode, Hurricane_ind,
#                          IndianPopulationRatio, Make, MMRAcquisitionAuctionAveragePrice,
#                          mode_engine_valves_per_cyl, model_body_style, model_doors, model_engine_num_cyl,
#                          model_engine_position, model_engine_torque_rpm, model_engine_type,
#                          model_seats, model_sold_in_us, Nationality, PurchDayofWeek,
#                          PurchMonth, Region, Size, StandardMake, StandardModel, State,
#                          SubModel_Format, VehBCost, VehicleAge, VehOdo, WheelType, ZipCode)
car.data.tbl %<>% select(IsBadBuy, CityType, Color, Division, FacilityCode, Hurricane_ind,
                         IndianPopulationRatio, Make, MMRAcquisitionAuctionAveragePrice,
                         mode_engine_valves_per_cyl, model_body_style, model_doors, model_engine_num_cyl,
                         model_engine_torque_rpm, model_engine_type,
                         model_seats, model_sold_in_us, Nationality, PurchDayofWeek,
                         PurchMonth, Region, Size, StandardMake, StandardModel, State,
                         SubModel_Format, VehBCost, VehicleAge, VehOdo, WheelType, ZipCode)


# forward variable selection
regfit.fwd <- regsubsets(IsBadBuy ~ ., data=car.data.tbl, nvmax=15, method="forward")

sink("../../Predict498_Repo/regit_fwd_20150505.txt")
summary(regfit.fwd)
sink()

# backward variable selection
regfit.back <- regsubsets(IsBadBuy ~ ., data=car.data.tbl, nvmax=15, method="backward")

sink("../../Predict498_Repo/regit_back.txt")
summary(regfit.back)
sink()

# LASSO
grid = 10^seq(10,-2,length=100)

car.data.tbl %<>% na.omit()

car.data.tbl.predictors <- model.matrix(IsBadBuy~.,car.data.tbl)[,-1]
regfit.lasso <- glmnet(car.data.tbl.predictors,as.integer(car.data.tbl$IsBadBuy),alpha=1, lambda=grid)

plot(regfit.lasso)

set.seed(108)
cv.out = cv.glmnet(car.data.tbl.predictors, as.integer(car.data.tbl$IsBadBuy),alpha=1)
plot(cv.out)
bestlam=cv.out$lambda.min

regfit.lasso.coeff <- predict(regfit.lasso,type="coefficients",s=bestlam)
head(regfit.lasso.coeff,10)

regfit.lasso.coeff

# Decision Tree

car.data.tbl.limited <- select(car.data.tbl,-WheelType)
ClassTree  <- rpart(IsBadBuy ~ ., method="poisson", control=rpart.control(maxdepth=8), data=car.data.tbl.limited)
printcp(ClassTree) # display the results 
plotcp(ClassTree) # visualize cross-validation results 
summary(ClassTree) # detailed summary of splits
post(ClassTree, file = "tree.ps", title = "Classification Tree")

prp(ClassTree, extra=1,tweak=.8)
prp(ClassTree, extra=1, uniform=F, branch=1, yesno=F, border.col=0, xsep="/")


######################

# Attempt 2

################

car.data$IsBadBuy <- as.factor(car.data$IsBadBuy)

car.data.tbl <- tbl_df(car.data)


car.data.tbl %<>% select(IsBadBuy, CityType,  Division, model_body_style,  
                         model_seats, model_sold_in_us, PurchDayofWeek,
                         PurchMonth, Region, Size, StandardMake, StandardModel, State,
                         SubModel_Format, VehBCost, VehicleAge, VehOdo, ZipCode,
                         
                         VehOdo_ind,  VehBCost_ind, WarrantyCost_ind, HouseholdsPerZipCode_ind, 
                          PersonsPerHousehold_ind, model_engine_power_rpm_ind, Model_engine_power_ps_ind, model_engine_cc_ind,
                          model_lkm_hwy_ind,  MMRAcquisitionAuctionAveragePrice_ind,
                         MMRCurrentAuctionAveragePrice_ind, MMRAcquisitionAuctionCleanPrice_ind,
                         MMRCurrentAuctionCleanPrice_ind,  MMRCurrentRetailAveragePrice_ind,
                         City, 
                         MMRCurrentRetailCleanPrice_ind, County, MMRAcquisitionRetailAveragePrice_ind,
                         MMRAcquisitonRetailCleanPrice_ind)


# forward variable selection
regfit.fwd <- regsubsets(IsBadBuy ~ ., data=car.data.tbl, nvmax=10, method="forward")

sink("../../Predict498_Repo/regit_fwd_new.txt")
summary(regfit.fwd)
sink()


# backward variable selection
regfit.back <- regsubsets(IsBadBuy ~ ., data=car.data.tbl, nvmax=10, method="backward")

sink("../../Predict498_Repo/regit_back_new.txt")
summary(regfit.back)
sink()

# stepwise variable selection
regfit.step <- regsubsets(IsBadBuy ~ ., data=car.data.tbl, nvmax=10, method="seqrep")

sink("../../Predict498_Repo/regit_step_new.txt")
summary(regfit.step)
sink()


# LASSO
grid = 10^seq(10,-2,length=100)

car.data.tbl %<>% na.omit()

car.data.tbl.predictors <- model.matrix(IsBadBuy~.,car.data.tbl)[,-1]
regfit.lasso <- glmnet(car.data.tbl.predictors,as.integer(car.data.tbl$IsBadBuy),alpha=1, lambda=grid)

plot(regfit.lasso)

set.seed(108)
cv.out = cv.glmnet(car.data.tbl.predictors, as.integer(car.data.tbl$IsBadBuy),alpha=1)
plot(cv.out)
bestlam=cv.out$lambda.min

regfit.lasso.coeff <- predict(regfit.lasso,type="coefficients",s=bestlam)
head(regfit.lasso.coeff,10)

regfit.lasso.coeff
