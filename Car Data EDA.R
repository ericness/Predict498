library(ggplot2)

setwd("C:/Users/ericn_000/Dropbox/Education/MS Predictive Analytics/PREDICT 498 Capstone/498 Capstone Project")

car.data <- read.csv(file="KaggleTrainingWithAddedCarDataAndDependent.csv", header=TRUE)

car.data$IsBadBuy <- as.factor(car.data$IsBadBuy)

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
