library(ggplot2)

setwd("C:/Users/ericn_000/Dropbox/Education/MS Predictive Analytics/PREDICT 498 Capstone/498 Capstone Project/Data/Final")
#setwd("C:/Users/eness/Dropbox/Education/MS Predictive Analytics/PREDICT 498 Capstone/498 Capstone Project/Data/Final")

#car.data <- read.csv(file="KaggleTrainingWithAddedCarDataAndDependent.csv", header=TRUE)
car.data <- read.csv(file="CarLemons_Final_04212015.csv", header=TRUE)

car.data$IsBadBuy <- as.factor(car.data$IsBadBuy)


ggplot(car.data, aes(x=IsBadBuy,y=VehOdo,fill=IsBadBuy)) + geom_boxplot()
ggplot(car.data, aes(x=IsBadBuy,y=MMRAcquisitionAuctionAveragePrice,fill=IsBadBuy)) + geom_boxplot()
ggplot(car.data, aes(x=IsBadBuy,y=MMRAcquisitionAuctionCleanPrice,fill=IsBadBuy)) + geom_boxplot()
ggplot(car.data, aes(x=IsBadBuy,y=MMRAcquisitionRetailAveragePrice,fill=IsBadBuy)) + geom_boxplot()
ggplot(car.data, aes(x=IsBadBuy,y=MMRAcquisitonRetailCleanPrice,fill=IsBadBuy)) + geom_boxplot()
ggplot(car.data, aes(x=IsBadBuy,y=MMRCurrentAuctionAveragePrice,fill=IsBadBuy)) + geom_boxplot()
ggplot(car.data, aes(x=IsBadBuy,y=MMRCurrentAuctionCleanPrice,fill=IsBadBuy)) + geom_boxplot()
ggplot(car.data, aes(x=IsBadBuy,y=MMRCurrentRetailAveragePrice,fill=IsBadBuy)) + geom_boxplot()
ggplot(car.data, aes(x=IsBadBuy,y=MMRCurrentRetailCleanPrice,fill=IsBadBuy)) + geom_boxplot()
ggplot(car.data, aes(x=IsBadBuy,y=WarrantyCost,fill=IsBadBuy)) + geom_boxplot()
ggplot(car.data, aes(x=IsBadBuy,y=WhitePopulationRatio,fill=IsBadBuy)) + geom_boxplot()
ggplot(car.data, aes(x=IsBadBuy,y=BlackPopulationRatio,fill=IsBadBuy)) + geom_boxplot()
ggplot(car.data, aes(x=IsBadBuy,y=HispanicPopulationRatio,fill=IsBadBuy)) + geom_boxplot()
ggplot(car.data, aes(x=IsBadBuy,y=AsianPopulationRatio,fill=IsBadBuy)) + geom_boxplot()
ggplot(car.data, aes(x=IsBadBuy,y=HawaiianPopulationRatio,fill=IsBadBuy)) + geom_boxplot()
ggplot(car.data, aes(x=IsBadBuy,y=IndianPopulationRatio,fill=IsBadBuy)) + geom_boxplot()
ggplot(car.data, aes(x=IsBadBuy,y=OtherPopulationRatio,fill=IsBadBuy)) + geom_boxplot()
ggplot(car.data, aes(x=IsBadBuy,y=MalePopulationRatio,fill=IsBadBuy)) + geom_boxplot()
ggplot(car.data, aes(x=IsBadBuy,y=FemalePopulationRatio,fill=IsBadBuy)) + geom_boxplot()
ggplot(car.data, aes(x=IsBadBuy,y=PersonsPerHousehold,fill=IsBadBuy)) + geom_boxplot()
ggplot(car.data, aes(x=IsBadBuy,y=AverageHouseValue,fill=IsBadBuy)) + geom_boxplot()
ggplot(car.data, aes(x=IsBadBuy,y=IncomePerHousehold,fill=IsBadBuy)) + geom_boxplot()
ggplot(car.data, aes(x=IsBadBuy,y=Elevation,fill=IsBadBuy)) + geom_boxplot()
ggplot(car.data, aes(x=IsBadBuy,y=VehBCost,fill=IsBadBuy)) + geom_boxplot()


ggplot(car.data, aes(x=IsBadBuy,y=model_engine_cc,fill=IsBadBuy)) + geom_boxplot()
ggplot(car.data, aes(x=IsBadBuy,y=model_engine_power_ps,fill=IsBadBuy)) + geom_boxplot()
ggplot(car.data, aes(x=IsBadBuy,y=model_engine_power_rpm,fill=IsBadBuy)) + geom_boxplot()
ggplot(car.data, aes(x=IsBadBuy,y=model_engine_torque_nm,fill=IsBadBuy)) + geom_boxplot()
ggplot(car.data, aes(x=IsBadBuy,y=model_engine_torque_rpm,fill=IsBadBuy)) + geom_boxplot()
ggplot(car.data, aes(x=IsBadBuy,y=model_engine_bore_mm,fill=IsBadBuy)) + geom_boxplot()
ggplot(car.data, aes(x=IsBadBuy,y=model_engine_stroke_mm,fill=IsBadBuy)) + geom_boxplot()
ggplot(car.data, aes(x=IsBadBuy,y=model_weight_kg,fill=IsBadBuy)) + geom_boxplot()
ggplot(car.data, aes(x=IsBadBuy,y=model_length_mm,fill=IsBadBuy)) + geom_boxplot()
ggplot(car.data, aes(x=IsBadBuy,y=model_width_mm,fill=IsBadBuy)) + geom_boxplot()
ggplot(car.data, aes(x=IsBadBuy,y=model_height_mm,fill=IsBadBuy)) + geom_boxplot()
ggplot(car.data, aes(x=IsBadBuy,y=model_wheelbase,fill=IsBadBuy)) + geom_boxplot()
ggplot(car.data, aes(x=IsBadBuy,y=model_lkm_hwy,fill=IsBadBuy)) + geom_boxplot()
ggplot(car.data, aes(x=IsBadBuy,y=model_lkm_mixed,fill=IsBadBuy)) + geom_boxplot()
ggplot(car.data, aes(x=IsBadBuy,y=model_lkm_city,fill=IsBadBuy)) + geom_boxplot()
ggplot(car.data, aes(x=IsBadBuy,y=model_fuel_cap_l,fill=IsBadBuy)) + geom_boxplot()


ggplot(car.data, aes(PurchMonth, ..count..)) + 
  geom_bar(aes(fill = IsBadBuy), position = "fill")
ggplot(car.data, aes(PurchDayofWeek, ..count..)) + 
  geom_bar(aes(fill = IsBadBuy), position = "fill")
ggplot(car.data, aes(Auction, ..count..)) + 
  geom_bar(aes(fill = IsBadBuy), position = "fill")
ggplot(car.data, aes(VehYear, ..count..)) + 
  geom_bar(aes(fill = IsBadBuy), position = "fill")
ggplot(car.data, aes(VehicleAge, ..count..)) + 
  geom_bar(aes(fill = IsBadBuy), position = "fill")
ggplot(car.data, aes(Make, ..count..)) + 
  geom_bar(aes(fill = IsBadBuy), position = "fill")
ggplot(car.data, aes(SubModel_Format, ..count..)) + 
  geom_bar(aes(fill = IsBadBuy), position = "fill")
ggplot(car.data, aes(Color, ..count..)) + 
  geom_bar(aes(fill = IsBadBuy), position = "fill")
ggplot(car.data, aes(Transmission, ..count..)) + 
  geom_bar(aes(fill = IsBadBuy), position = "fill")
ggplot(car.data, aes(WheelType, ..count..)) + 
  geom_bar(aes(fill = IsBadBuy), position = "fill")
ggplot(car.data, aes(Nationality, ..count..)) + 
  geom_bar(aes(fill = IsBadBuy), position = "fill")
ggplot(car.data, aes(Size, ..count..)) + 
  geom_bar(aes(fill = IsBadBuy), position = "fill")
ggplot(car.data, aes(TopThreeAmericanName, ..count..)) + 
  geom_bar(aes(fill = IsBadBuy), position = "fill")
ggplot(car.data, aes(VNZIP1, ..count..)) + 
  geom_bar(aes(fill = IsBadBuy), position = "fill")
ggplot(car.data, aes(VNST, ..count..)) + 
  geom_bar(aes(fill = IsBadBuy), position = "fill")
ggplot(car.data, aes(IsOnlineSale, ..count..)) + 
  geom_bar(aes(fill = IsBadBuy), position = "fill")
ggplot(car.data, aes(ZipCode, ..count..)) + 
  geom_bar(aes(fill = IsBadBuy), position = "fill")
ggplot(car.data, aes(State, ..count..)) + 
  geom_bar(aes(fill = IsBadBuy), position = "fill")
ggplot(car.data, aes(CityType, ..count..)) + 
  geom_bar(aes(fill = IsBadBuy), position = "fill")
ggplot(car.data, aes(TimeZone, ..count..)) + 
  geom_bar(aes(fill = IsBadBuy), position = "fill")
ggplot(car.data, aes(Region, ..count..)) + 
  geom_bar(aes(fill = IsBadBuy), position = "fill")
ggplot(car.data, aes(Division, ..count..)) + 
  geom_bar(aes(fill = IsBadBuy), position = "fill")
ggplot(car.data, aes(FacilityCode, ..count..)) + 
  geom_bar(aes(fill = IsBadBuy), position = "fill")
ggplot(car.data, aes(CityDeliveryIndicator, ..count..)) + 
  geom_bar(aes(fill = IsBadBuy), position = "fill")
ggplot(car.data, aes(CarrierRouteRateSortation, ..count..)) + 
  geom_bar(aes(fill = IsBadBuy), position = "fill")
ggplot(car.data, aes(Income_Ind, ..count..)) + 
  geom_bar(aes(fill = IsBadBuy), position = "fill")


ggplot(car.data, aes(StandardMake, ..count..)) + 
  geom_bar(aes(fill = IsBadBuy), position = "fill")
ggplot(car.data, aes(StandardModel, ..count..)) + 
  geom_bar(aes(fill = IsBadBuy), position = "fill")

ggplot(car.data, aes(model_body_style, ..count..)) + 
  geom_bar(aes(fill = IsBadBuy), position = "fill")
ggplot(car.data, aes(model_engine_position, ..count..)) + 
  geom_bar(aes(fill = IsBadBuy), position = "fill")
ggplot(car.data, aes(model_engine_num_cyl, ..count..)) + 
  geom_bar(aes(fill = IsBadBuy), position = "fill")
ggplot(car.data, aes(model_engine_type, ..count..)) + 
  geom_bar(aes(fill = IsBadBuy), position = "fill")
ggplot(car.data, aes(mode_engine_valves_per_cyl, ..count..)) + 
  geom_bar(aes(fill = IsBadBuy), position = "fill")
ggplot(car.data, aes(model_drive, ..count..)) + 
  geom_bar(aes(fill = IsBadBuy), position = "fill")
ggplot(car.data, aes(model_seats, ..count..)) + 
  geom_bar(aes(fill = IsBadBuy), position = "fill")
ggplot(car.data, aes(model_doors, ..count..)) + 
  geom_bar(aes(fill = IsBadBuy), position = "fill")
ggplot(car.data, aes(model_sold_in_us, ..count..)) + 
  geom_bar(aes(fill = IsBadBuy), position = "fill")

ggplot(car.data, aes(FEMA_ind, ..count..)) + 
  geom_bar(aes(fill = IsBadBuy), position = "fill")
ggplot(car.data, aes(Flood_ind, ..count..)) + 
  geom_bar(aes(fill = IsBadBuy), position = "fill")
ggplot(car.data, aes(Hurricane_ind, ..count..)) + 
  geom_bar(aes(fill = IsBadBuy), position = "fill")
ggplot(car.data, aes(Typhoon_ind, ..count..)) + 
  geom_bar(aes(fill = IsBadBuy), position = "fill")
ggplot(car.data, aes(Coastal_storm_ind, ..count..)) + 
  geom_bar(aes(fill = IsBadBuy), position = "fill")

# new variables 4/26
ggplot(car.data, aes(x=IsBadBuy,y=Recall_Ct,fill=IsBadBuy)) + geom_boxplot()

ggplot(car.data, aes(VehOdo_ind, ..count..)) + 
  geom_bar(aes(fill = IsBadBuy), position = "fill")
ggplot(car.data, aes(MMRAcquisitionAuctionAveragePrice_ind, ..count..)) + 
  geom_bar(aes(fill = IsBadBuy), position = "fill")
ggplot(car.data, aes(MMRAcquisitionAuctionCleanPrice_ind, ..count..)) + 
  geom_bar(aes(fill = IsBadBuy), position = "fill")
ggplot(car.data, aes(MMRAcquisitionRetailAveragePrice_ind, ..count..)) + 
  geom_bar(aes(fill = IsBadBuy), position = "fill")
ggplot(car.data, aes(MMRAcquisitonRetailCleanPrice_ind, ..count..)) + 
  geom_bar(aes(fill = IsBadBuy), position = "fill")
ggplot(car.data, aes(MMRCurrentAuctionAveragePrice_ind, ..count..)) + 
  geom_bar(aes(fill = IsBadBuy), position = "fill")
ggplot(car.data, aes(MMRCurrentAuctionCleanPrice_ind, ..count..)) + 
  geom_bar(aes(fill = IsBadBuy), position = "fill")
ggplot(car.data, aes(MMRCurrentRetailAveragePrice_ind, ..count..)) + 
  geom_bar(aes(fill = IsBadBuy), position = "fill")
ggplot(car.data, aes(MMRCurrentRetailCleanPrice_ind, ..count..)) + 
  geom_bar(aes(fill = IsBadBuy), position = "fill")
ggplot(car.data, aes(VehBCost_ind, ..count..)) + 
  geom_bar(aes(fill = IsBadBuy), position = "fill")
ggplot(car.data, aes(WarrantyCost_ind, ..count..)) + 
  geom_bar(aes(fill = IsBadBuy), position = "fill")
ggplot(car.data, aes(HouseholdsPerZipCode_ind, ..count..)) + 
  geom_bar(aes(fill = IsBadBuy), position = "fill")
ggplot(car.data, aes(PersonsPerHousehold_ind, ..count..)) + 
  geom_bar(aes(fill = IsBadBuy), position = "fill")
ggplot(car.data, aes(AverageHouseValue_ind, ..count..)) + 
  geom_bar(aes(fill = IsBadBuy), position = "fill")
ggplot(car.data, aes(IncomePerHousehold_ind, ..count..)) + 
  geom_bar(aes(fill = IsBadBuy), position = "fill")
ggplot(car.data, aes(Elevation_ind, ..count..)) + 
  geom_bar(aes(fill = IsBadBuy), position = "fill")
ggplot(car.data, aes(MedianAge_ind, ..count..)) + 
  geom_bar(aes(fill = IsBadBuy), position = "fill")
ggplot(car.data, aes(Model_engine_power_ps_ind, ..count..)) + 
  geom_bar(aes(fill = IsBadBuy), position = "fill")
ggplot(car.data, aes(model_engine_cc_ind, ..count..)) + 
  geom_bar(aes(fill = IsBadBuy), position = "fill")
ggplot(car.data, aes(model_engine_power_rpm_ind, ..count..)) + 
  geom_bar(aes(fill = IsBadBuy), position = "fill")
ggplot(car.data, aes(model_engine_torque_nm_ind, ..count..)) + 
  geom_bar(aes(fill = IsBadBuy), position = "fill")
ggplot(car.data, aes(model_engine_torque_rpm_ind, ..count..)) + 
  geom_bar(aes(fill = IsBadBuy), position = "fill")
ggplot(car.data, aes(model_engine_bore_mm_ind, ..count..)) + 
  geom_bar(aes(fill = IsBadBuy), position = "fill")
ggplot(car.data, aes(model_engine_stroke_mm_ind, ..count..)) + 
  geom_bar(aes(fill = IsBadBuy), position = "fill")
ggplot(car.data, aes(model_weight_kg_ind, ..count..)) + 
  geom_bar(aes(fill = IsBadBuy), position = "fill")
ggplot(car.data, aes(model_length_mm_ind, ..count..)) + 
  geom_bar(aes(fill = IsBadBuy), position = "fill")
ggplot(car.data, aes(model_width_mm_ind, ..count..)) + 
  geom_bar(aes(fill = IsBadBuy), position = "fill")
ggplot(car.data, aes(model_height_mm_ind, ..count..)) + 
  geom_bar(aes(fill = IsBadBuy), position = "fill")
ggplot(car.data, aes(model_wheelbase_ind, ..count..)) + 
  geom_bar(aes(fill = IsBadBuy), position = "fill")
ggplot(car.data, aes(model_lkm_hwy_ind, ..count..)) + 
  geom_bar(aes(fill = IsBadBuy), position = "fill")
ggplot(car.data, aes(model_lkm_mixed_ind, ..count..)) + 
  geom_bar(aes(fill = IsBadBuy), position = "fill")
ggplot(car.data, aes(model_lkm_city_ind, ..count..)) + 
  geom_bar(aes(fill = IsBadBuy), position = "fill")
ggplot(car.data, aes(model_fuel_cap_l_ind, ..count..)) + 
  geom_bar(aes(fill = IsBadBuy), position = "fill")
ggplot(car.data, aes(WhitePopulationRatio_ind, ..count..)) + 
  geom_bar(aes(fill = IsBadBuy), position = "fill")
ggplot(car.data, aes(BlackPopulationRatio_ind, ..count..)) + 
  geom_bar(aes(fill = IsBadBuy), position = "fill")
ggplot(car.data, aes(HispanicPopulationRatio_ind, ..count..)) + 
  geom_bar(aes(fill = IsBadBuy), position = "fill")
ggplot(car.data, aes(AsianPopulationRatio_ind, ..count..)) + 
  geom_bar(aes(fill = IsBadBuy), position = "fill")
ggplot(car.data, aes(HawaiianPopulationRatio_ind, ..count..)) + 
  geom_bar(aes(fill = IsBadBuy), position = "fill")
ggplot(car.data, aes(IndianPopulationRatio_ind, ..count..)) + 
  geom_bar(aes(fill = IsBadBuy), position = "fill")
ggplot(car.data, aes(OtherPopulationRatio_ind, ..count..)) + 
  geom_bar(aes(fill = IsBadBuy), position = "fill")
ggplot(car.data, aes(MalePopulationRatio_ind, ..count..)) + 
  geom_bar(aes(fill = IsBadBuy), position = "fill")
ggplot(car.data, aes(FemalePopulationRatio_ind, ..count..)) + 
  geom_bar(aes(fill = IsBadBuy), position = "fill")



