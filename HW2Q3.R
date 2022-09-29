#Problem 3
library(ggplot2)
library(caTools)
library(car)
library(lmtest)

#Read in data
airbnb <- read.csv('/Users/allisonking/Downloads/AB_NYC_2019.csv')

#Clean the data, replace blank values with 0s for calculations
airbnb$reviews_per_month %>% replace(is.na(.), 0)
airbnb$reviews_per_month <- airbnb$reviews_per_month %>% replace(is.na(.), 0)

#Scatterplot to look at linearity between price and reviews per month
plot(airbnb$price, airbnb$reviews_per_month, main = "Price vs Reviews Per Month (Untransformed)")

#Scatterplot to look at linearity of transformed data (between log.price and log.reviews_per_month)
plot(airbnb$log.price, airbnb$log.reviews_per_month, main = "Logged Price vs Reviews Per Month")

#QQ Plot and histogram of x (price) to show (ab)normality of data untransformed
qqplot(airbnb$price, airbnb$reviews_per_month, col="Deeppink", main="QQ Plot of Untransformed Data")
hist(airbnb$price, col="Gold")

#QQ Plot of logged data and histogram of x (log.price) to show normalization
airbnb$log.price <- log(airbnb$price)
airbnb$log.reviews_per_month <- log(airbnb$reviews_per_month)
qqplot(airbnb$log.price, airbnb$log.reviews_per_month, col="Deeppink", main="QQ Plot of Logged Data")
hist(airbnb$log.price, col="Gold")

#Correlation matrix to make sure there is no multicollinearity among the variables
mydata <- airbnb[, c(10, 11, 12, 14, 15, 16)]
cormat <- round(cor(mydata),2) 
head(cormat)

#Linear model build out for numeric variables
set.seed(1234)

#Remove -Inf values to proceed with LM calculation
new.airbnb <- subset(airbnb, log.reviews_per_month!="-Inf" & log.price != "-Inf")

#Durbin Watson test to check for autocorrelation
dummymodel <- lm(log.reviews_per_month ~ log.price, data=new.airbnb)
durbinWatsonTest(dummymodel)

#Perform the Goldfeld Quandt test to check last assumption, removing the middle 20%
gqtest(dummymodel, order.by = ~log.price, data = new.airbnb, fraction = 10000)

#Use 70% of dataset as training set and 30% as test set
sample <- sample.split(new.airbnb$host_id, SplitRatio = 0.7)
train  <- subset(new.airbnb, sample == TRUE)
test   <- subset(new.airbnb, sample == FALSE)

#LM function on logged data. The test formula is log.reviews_per_month = 0.012*log.price - 0.577.
airbnb.lm.train <- lm(log.reviews_per_month ~ log.price, data=train)
airbnb.lm.test <- lm(log.reviews_per_month ~ log.price, data=test)
summary(airbnb.lm.test)

#Linear regression on training data
smooth <- ggplot(data=train, aes(x=log.price, y=log.reviews_per_month, color=room_type)) + 
  geom_point(aes(shape=room_type), size=1.5) + xlab("Price") + ylab("Reviews Per Month") + 
  ggtitle("Scatterplot on Training Data")
smooth + geom_smooth(method="lm")

#Linear regression on testing data
smooth <- ggplot(data=test, aes(x=log.price, y=log.reviews_per_month, color=room_type)) + 
  geom_point(aes(shape=room_type), size=1.5) + xlab("Price") + ylab("Reviews Per Month") + 
  ggtitle("Scatterplot on Testing Data")
smooth + geom_smooth(method="lm")

#Ratio plot for test data
ratioxy=test$log.price/test$log.reviews_per_month
plot(ratioxy,type='l',col='blue')
abline(h=1)
