library(ggplot2)
library(dplyr)

setwd("C:/Users/ericn_000/Dropbox/Education/MS Predictive Analytics/PREDICT 498 Capstone/498 Capstone Project/Data/Final")
#setwd("C:/Users/eness/Dropbox/Education/MS Predictive Analytics/PREDICT 498 Capstone/498 Capstone Project/Data/Final")

car.data <- read.csv(file="CarLemons_Final_05052015.csv", header=TRUE)
car.data.tbl <- tbl_df(car.data)

summary(car.data$VehOdo)
ggplot(car.data, aes(x=VehOdo)) + geom_histogram()

# all MMR are skewed left and have long tail, but no outliers
summary(car.data$MMRAcquisitionAuctionAveragePrice)
ggplot(car.data, aes(x=MMRAcquisitionAuctionAveragePrice)) + geom_histogram()

summary(car.data$MMRAcquisitionAuctionCleanPrice)
ggplot(car.data, aes(x=MMRAcquisitionAuctionCleanPrice)) + geom_histogram()

summary(car.data$MMRAcquisitionRetailAveragePrice)
ggplot(car.data, aes(x=MMRAcquisitionRetailAveragePrice)) + geom_histogram(fill="blue",binwidth=500) +
  ggtitle("Histogram of MMRAcquisitionRetailAveragePrice")

summary(car.data$MMRAcquisitonRetailCleanPrice)
ggplot(car.data, aes(x=MMRAcquisitonRetailCleanPrice)) + geom_histogram()

summary(car.data$MMRCurrentAuctionAveragePrice)
ggplot(car.data, aes(x=MMRCurrentAuctionAveragePrice)) + geom_histogram()

summary(car.data$MMRCurrentAuctionCleanPrice)
ggplot(car.data, aes(x=MMRCurrentAuctionCleanPrice)) + geom_histogram()

summary(car.data$MMRCurrentRetailAveragePrice)
ggplot(car.data, aes(x=MMRCurrentRetailAveragePrice)) + geom_histogram()

summary(car.data$MMRCurrentRetailCleanPrice)
ggplot(car.data, aes(x=MMRCurrentRetailCleanPrice)) + geom_histogram()

head(car.data$MMRCurrentRetailCleanPrice, desc)

car.data.tbl %>% 
    select(MMRCurrentRetailCleanPrice)  %>% 
   arrange(desc(MMRCurrentRetailCleanPrice)) %>% 
   head(30)

# skewed but no outliers
summary(car.data$WarrantyCost)
ggplot(car.data, aes(x=WarrantyCost)) + geom_histogram()

# skewed right
summary(car.data$WhitePopulationRatio)
ggplot(car.data, aes(x=WhitePopulationRatio)) + geom_histogram()

# skewed left
summary(car.data$BlackPopulationRatio)
ggplot(car.data, aes(x=BlackPopulationRatio)) + geom_histogram()

summary(car.data$HispanicPopulationRatio)
ggplot(car.data, aes(x=HispanicPopulationRatio)) + geom_histogram()

summary(car.data$AsianPopulationRatio)
ggplot(car.data, aes(x=AsianPopulationRatio)) + geom_histogram()

summary(car.data$HawaiianPopulationRatio)
ggplot(car.data, aes(x=HawaiianPopulationRatio)) + geom_histogram()

summary(car.data$IndianPopulationRatio)
ggplot(car.data, aes(x=IndianPopulationRatio)) + geom_histogram()

summary(car.data$OtherPopulationRatio)
ggplot(car.data, aes(x=OtherPopulationRatio)) + geom_histogram()

summary(car.data$MalePopulationRatio)
ggplot(car.data, aes(x=MalePopulationRatio)) + geom_histogram()

# Daytona Beach, FL has low female population ratio
summary(car.data$FemalePopulationRatio)
ggplot(car.data, aes(x=FemalePopulationRatio)) + geom_histogram()

summary(car.data$PersonsPerHousehold)
ggplot(car.data, aes(x=PersonsPerHousehold)) + geom_histogram()

# skewed left - thrown off by California, all tail cities in CA
summary(car.data$AverageHouseValue)
ggplot(car.data, aes(x=AverageHouseValue)) + geom_histogram()

# skewed left but no outliers
summary(car.data$IncomePerHousehold)
ggplot(car.data, aes(x=IncomePerHousehold)) + geom_histogram()

# skewed left because of CO etc.
summary(car.data$Elevation)
ggplot(car.data, aes(x=Elevation)) + geom_histogram()

# similar to MMR variables
summary(car.data$VehBCost)
ggplot(car.data, aes(x=VehBCost)) + geom_histogram()

summary(car.data$model_engine_cc)
ggplot(car.data, aes(x=model_engine_cc)) + geom_histogram()

summary(car.data$model_engine_power_ps)
ggplot(car.data, aes(x=model_engine_power_ps)) + geom_histogram()

summary(car.data$model_engine_power_rpm)
ggplot(car.data, aes(x=model_engine_power_rpm)) + geom_histogram()

summary(car.data$model_engine_torque_nm)
ggplot(car.data, aes(x=model_engine_torque_nm)) + geom_histogram()

summary(car.data$model_engine_torque_rpm)
ggplot(car.data, aes(x=model_engine_torque_rpm)) + geom_histogram()

summary(car.data$model_engine_bore_mm)
ggplot(car.data, aes(x=model_engine_bore_mm)) + geom_histogram()

summary(car.data$model_engine_stroke_mm)
ggplot(car.data, aes(x=model_engine_stroke_mm)) + geom_histogram()

summary(car.data$model_weight_kg)
ggplot(car.data, aes(x=model_weight_kg)) + geom_histogram()

summary(car.data$model_length_mm)
ggplot(car.data, aes(x=model_length_mm)) + geom_histogram()

summary(car.data$model_width_mm)
ggplot(car.data, aes(x=model_width_mm)) + geom_histogram()

summary(car.data$model_height_mm)
ggplot(car.data, aes(x=model_height_mm)) + geom_histogram()

summary(car.data$model_wheelbase)
ggplot(car.data, aes(x=model_wheelbase)) + geom_histogram()

summary(car.data$model_lkm_hwy)
ggplot(car.data, aes(x=model_lkm_hwy)) + geom_histogram()

summary(car.data$model_lkm_mixed)
ggplot(car.data, aes(x=model_lkm_mixed)) + geom_histogram()

summary(car.data$model_lkm_city)
ggplot(car.data, aes(x=model_lkm_city)) + geom_histogram()

summary(car.data$model_fuel_cap_l)
ggplot(car.data, aes(x=model_fuel_cap_l)) + geom_histogram()

# Dodge Ram Pickup is outlier
summary(car.data$Recall_Ct)
ggplot(car.data, aes(x=Recall_Ct)) + geom_histogram(fill="blue") + ggtitle("Histogram of Recall_Ct")






