#HW 3 for STAT 6324 - logistic regression
#EDA

#install.packages('caret')
#install.packages('InformationValue')
#install.packages('ISLR')
library(ggplot2)
library(dplyr)
library(caTools)
library(caret)
library(InformationValue)
library(ISLR)
library(ROCR)
library(mice)
library(VIM)
library(performanceEstimation)
library(mltools)

#Read in data and look at variables
hotel <- read.csv("/Users/allisonking/Downloads/hotel_bookings.csv")
str(hotel)
summary(hotel)

#Visualize missing data
md.pattern(hotel)
simple_aggr = aggr(hotel, col=mdc(1:2), numbers=TRUE, sortVars=TRUE, labels=names(hotel), cex.axis=.7, 
                   gap=3, ylab=c("Proportion of missingness","Missingness Pattern"))

#Remove variable with highest count of missing values, which is Children
hotel1 = subset(hotel, select = -c(children))

#Write a function that picks apart character columns and makes them  numeric based on their unique values
convert_category <- function(x) {
  new_x <- as.numeric(factor(x, levels=unique(x)))
  return(new_x)
}

#Create new dataset by mutating columns with convert category function on all nonnumeric columns
hotel_new <- mutate_if(hotel1, function (x) !is.numeric(x), convert_category)

#Correlation matrix to make sure there is no multicollinearity among the variables
mydata <- hotel_new[, c(1:31)]
cormat <- round(cor(mydata),2) 
head(cormat)

#Based on the correlation matrix, we will be sure to remove reservation_status due to multicollinearity 

#Bar graph showing total number of cancellations by month
bar1 <- ggplot(data=hotel1, aes(x=arrival_date_month, y=sum(is_canceled)))
bar1 + geom_bar(stat="identity", fill="deeppink") + xlab("Month") + ylab("Total # of Cancellations") + ggtitle("Cancellations by Month")

#Bar graph showing bookings by hotel type
barplot(table(hotel$hotel), col="green", xlab="Hotel Type", ylab="Count", main="Bar Plot of Bookings by Hotel Type")

#Bar graph showing cancellations by deposit type and hotel type
bar1 <- ggplot(data=hotel1, aes(x=deposit_type, y=sum(is_canceled), fill=hotel)) 
bar1 + geom_bar(stat="identity")

#Scatterplot showing days_in_waiting_list as a function of lead_time, color-coded depending on whether customers cancelled their reservation or not
ggplot(hotel_new, aes(x = days_in_waiting_list, y = lead_time, color = is_canceled)) + geom_point()

#Split data between training and testing sets, prepping for logistic regression model
set.seed(48)
split = sample.split(hotel_new, SplitRatio = 0.70)
Train = subset(hotel_new, split==TRUE)
Test = subset(hotel_new, split==FALSE)
str(Train)
str(Test)

#Initial logistic regression model on training data
TrainModel = glm(is_canceled ~ hotel + lead_time + arrival_date_year + arrival_date_month + arrival_date_week_number + 
                   stays_in_weekend_nights + stays_in_week_nights + adults + babies + meal + country + market_segment + 
                   distribution_channel + is_repeated_guest + previous_cancellations + previous_bookings_not_canceled +
                   reserved_room_type + assigned_room_type + booking_changes + deposit_type + agent + company + days_in_waiting_list +
                   customer_type + adr + required_car_parking_spaces + total_of_special_requests + 
                   reservation_status_date, data = Train)
summary(TrainModel)

#Refine model - remove "days_in_waiting_list" column as it's the least significant (p=0.859)
TrainModel = glm(is_canceled ~ hotel + lead_time + arrival_date_year + arrival_date_month + arrival_date_week_number + 
                   stays_in_weekend_nights + stays_in_week_nights + adults + babies + meal + country + market_segment + 
                   distribution_channel + is_repeated_guest + previous_cancellations + previous_bookings_not_canceled +
                   reserved_room_type + assigned_room_type + booking_changes + deposit_type + agent + company +
                   customer_type + adr + required_car_parking_spaces + total_of_special_requests + 
                   reservation_status_date, data = Train)
summary(TrainModel)

#Refine model - remove "market_segment" column as it's the least significant (p=0.245)
TrainModel = glm(is_canceled ~ hotel + lead_time + arrival_date_year + arrival_date_month + arrival_date_week_number + 
                   stays_in_weekend_nights + stays_in_week_nights + adults + babies + meal + country + 
                   distribution_channel + is_repeated_guest + previous_cancellations + previous_bookings_not_canceled +
                   reserved_room_type + assigned_room_type + booking_changes + deposit_type + agent + company +
                   customer_type + adr + required_car_parking_spaces + total_of_special_requests + 
                   reservation_status_date, data = Train)
summary(TrainModel)

#Refine model - remove "meal" column as it's the least significant (p=0.177)
TrainModel = glm(is_canceled ~ hotel + lead_time + arrival_date_year + arrival_date_month + arrival_date_week_number + 
                   stays_in_weekend_nights + stays_in_week_nights + adults + babies + country + 
                   distribution_channel + is_repeated_guest + previous_cancellations + previous_bookings_not_canceled +
                   reserved_room_type + assigned_room_type + booking_changes + deposit_type + agent + company +
                   customer_type + adr + required_car_parking_spaces + total_of_special_requests + 
                   reservation_status_date, data = Train)
summary(TrainModel)

#Refine model - remove "arrival_date_month" column as it's the least significant (p=0.110)
TrainModel = glm(is_canceled ~ hotel + lead_time + arrival_date_year + arrival_date_week_number + 
                   stays_in_weekend_nights + stays_in_week_nights + adults + babies + country + 
                   distribution_channel + is_repeated_guest + previous_cancellations + previous_bookings_not_canceled +
                   reserved_room_type + assigned_room_type + booking_changes + deposit_type + agent + company +
                   customer_type + adr + required_car_parking_spaces + total_of_special_requests + 
                   reservation_status_date, data = Train)
summary(TrainModel)

#Refine model - remove "babies" column as it's the least significant (p=0.081)
TrainModel = glm(is_canceled ~ hotel + lead_time + arrival_date_year + arrival_date_week_number + 
                   stays_in_weekend_nights + stays_in_week_nights + adults + country + 
                   distribution_channel + is_repeated_guest + previous_cancellations + previous_bookings_not_canceled +
                   reserved_room_type + assigned_room_type + booking_changes + deposit_type + agent + company +
                   customer_type + adr + required_car_parking_spaces + total_of_special_requests + 
                   reservation_status_date, data = Train)
summary(TrainModel)

#Compare predictions
PredictTrain = predict(TrainModel, type="response")
#Type = response - gives probabilities as predictions
table(Train$is_canceled, PredictTrain > 0.5)

#create confusion matrix on training set
threshold=0.5
predicted_values<-ifelse(predict(TrainModel,type="response") > threshold, 1, 0)
actual_values<-TrainModel$y
conf_matrix<-table(predicted_values,actual_values)
conf_matrix
#calculate sensitivity
sensitivity(predicted_values,actual_values)
#calculate specificity
specificity(predicted_values,actual_values)

#AUC and ROC for training set
ROCRpred = prediction(predicted_values, actual_values)
ROCCurve = performance(ROCRpred, "tpr", "fpr")
plot(ROCCurve)
plot(ROCCurve, colorize=TRUE, print.cutoffs.at=seq(0,1,0.1), text.adj=c(-0.2,0.7))
as.numeric(performance(ROCRpred, "auc")@y.values)

#AUC and ROC for testing set
PredictTest = predict(TrainModel, type="response", newdata=Test)
table(Test$is_canceled, PredictTest > 0.9)
ROCRpred2 = prediction(PredictTest, Test$is_canceled)
ROCCurve2 = performance(ROCRpred2, "tpr", "fpr")
plot(ROCCurve2)
plot(ROCCurve2, colorize=TRUE, print.cutoffs.at=seq(0,1,0.1), text.adj=c(-0.2,0.7))
as.numeric(performance(ROCRpred2, "auc")@y.values)

#Hot encoding
#Create dataframe of a couple of significant categorical varaibles for hot encoding 
#dummyVars fcn will convert categorical features of this list into binary/numeric columns
hotencod <- data.frame(hotel$customer_type, hotel$deposit_type, hotel$distribution_channel, hotel$is_canceled)

#Split into training and testing set
set.seed(48)
split = sample.split(hotencod, SplitRatio = 0.70)
TrainEnc = subset(hotencod, split==TRUE)
TestEnc = subset(hotencod, split==FALSE)

#Dummify the data - training set
dmy <- dummyVars(" ~ .", data = TrainEnc)
trsf1 <- data.frame(predict(dmy, newdata = TrainEnc))

#Logistic regression model on hot encoded data
TrainModel_hc = glm(hotel.is_canceled ~ ., data = trsf1)
summary(TrainModel_hc)

#Check class imbalance
#view distribution of response variable
table(hotel_new$is_canceled)
barplot(prop.table(table(hotel_new$is_canceled)),
        col = rainbow(2),
        ylim = c(0, .7),
        main = "Class Distribution")

#use SMOTE to create new dataset that is more balanced
newdata <- smote(is_canceled ~ hotel + lead_time + arrival_date_year + arrival_date_week_number + 
                   stays_in_weekend_nights + stays_in_week_nights + adults + country + 
                   distribution_channel + is_repeated_guest + previous_cancellations + previous_bookings_not_canceled +
                   reserved_room_type + assigned_room_type + booking_changes + deposit_type + agent + company +
                   customer_type + adr + required_car_parking_spaces + total_of_special_requests + 
                   reservation_status_date, hotel_new, perc.over = 30, k = 5, perc.under = 1)
table(newdata$is_canceled)

#view distribution of response variable in new dataset
table(new_df$is_canceled)

#AUC and ROC for SMOTE training set
ROCRpred = prediction(predicted_values, actual_values)
ROCCurve = performance(ROCRpred, "tpr", "fpr")
plot(ROCCurve)
plot(ROCCurve, colorize=TRUE, print.cutoffs.at=seq(0,1,0.1), text.adj=c(-0.2,0.7))
as.numeric(performance(ROCRpred, "auc")@y.values)

#AUC and ROC for SMOTE testing set
PredictTest = predict(TrainModel, type="response", newdata=Test)
table(Test$is_canceled, PredictTest > 0.9)
ROCRpred2 = prediction(PredictTest, Test$is_canceled)
ROCCurve2 = performance(ROCRpred2, "tpr", "fpr")
plot(ROCCurve2)
plot(ROCCurve2, colorize=TRUE, print.cutoffs.at=seq(0,1,0.1), text.adj=c(-0.2,0.7))
as.numeric(performance(ROCRpred2, "auc")@y.values)

