#Read in data
airbnb <- read.csv('/Users/allisonking/Downloads/AB_NYC_2019.csv')

#Load in libraries
library(tidyverse)
library(dplyr)
library(hrbrthemes)
library(plyr) 
library(ggplot2)
library(treemapify)
library(caTools)

#Clean the data, replace blank values with 0s for calculations
airbnb$last_review <- as.Date(airbnb$last_review)
airbnb$reviews_per_month %>% replace(is.na(.), 0)
airbnb$reviews_per_month <- airbnb$reviews_per_month %>% replace(is.na(.), 0)
airbnb$number_of_reviews %>% replace(is.na(.), 0)
airbnb$number_of_reviews <- airbnb$number_of_reviews %>% replace(is.na(.), 0)

#Order data in descending order based on number_of_reviews
data_new2 <- airbnb %>%     
  arrange(desc(number_of_reviews)) %>% 
  slice(1:10)
data_new2  

#Bar graph showing busiest hosts
barplot(table(data_new2$host_name), col="black", xlab="Host", ylab="Count", main="Bar Plot of Busiest Hosts")
bar1 <- ggplot(data=data_new2, aes(x=host_name, y=number_of_reviews, fill=neighbourhood_group)) 
bar1 + geom_bar(stat="identity", position="dodge") + 
  scale_fill_manual(values=c("orange", "blue", "darkgreen", "purple", "red"),
                    name="Neighborhoods",
                    breaks=c("Manhattan", "Queens", "Bronx", "Brooklyn", "Staten Island"),
                    labels=c("Manhattan", "Queens", "Bronx", "Brooklyn", "Staten Island"))

#Treemap of the above visual, but not as pretty
ggplot(data_new2, aes(area = number_of_reviews, fill = neighbourhood_group, label = host_name)) +
  geom_treemap() +
  geom_treemap_text(colour = "white",
                    place = "centre",
                    size = 15)

#Bar graph showing average price
bar1 <- ggplot(data=data_new2, aes(x=host_name, y=mean(price)))
bar1 + geom_bar(stat="identity", fill="deeppink") + xlab("Host Name") + ylab("Avg Price Per Night ($)") + ggtitle("Top 10 Hosts Avg $/Night")


#Correlation matrix to see if there is any correlation between variables
mydata <- airbnb[, c(10, 11, 12, 14, 15, 16)]
cormat <- round(cor(mydata),2) 
head(cormat)

#QQ Plots to show (ab)normality of data
par(mfrow=c(2,1))
qqnorm(subset(airbnb, airbnb$neighbourhood_group == "Manhattan")$price, col="Deeppink", main="QQ Plot of Manhattan Price")
qqnorm(subset(airbnb, airbnb$neighbourhood_group == "Manhattan")$number_of_reviews, col="Deeppink", main="QQ Plot of Manhattan # of Reviews")

qqnorm(subset(airbnb, airbnb$neighbourhood_group == "Brooklyn")$price, col="Orange", main="QQ Plot of Brooklyn Price")
qqnorm(subset(airbnb, airbnb$neighbourhood_group == "Brooklyn")$number_of_reviews, col="Orange", main="QQ Plot of Brooklyn # of Reviews")

qqnorm(subset(airbnb, airbnb$neighbourhood_group == "Queens")$price, col="Gold", main="QQ Plot for Queens Price")
qqnorm(subset(airbnb, airbnb$neighbourhood_group == "Queens")$number_of_reviews, col="Gold", main="QQ Plot of Queens # of Reviews")

qqnorm(subset(airbnb, airbnb$neighbourhood_group == "Bronx")$price, col="Green", main="QQ Plot of Bronx Price")
qqnorm(subset(airbnb, airbnb$neighbourhood_group == "Bronx")$number_of_reviews, col="Green", main="QQ Plot of Bronx # of Reviews")

qqnorm(subset(airbnb, airbnb$neighbourhood_group == "Staten Island")$price, col="Blue", main="QQ Plot of Staten Island Price")
qqnorm(subset(airbnb, airbnb$neighbourhood_group == "Staten Island")$number_of_reviews, col="Blue", main="QQ Plot of Staten Island # of Reviews")

#Number of listings per neighborhood group
counts <- table(airbnb$neighbourhood_group)
barplot(counts, main="Count of Listings by Neighborhood Group",xlab="Neighborhood", col = "violetred1")

#Number of reviews per neighborhood group
ggplot(data = aggregate(airbnb$number_of_reviews, list(airbnb$neighbourhood_group), sum), aes(Group.1, x)) + geom_col() + 
  xlab("Neighborhood Group") +  ylab("Number of Reviews") + ggtitle("Count of Reviews Per Neighborhood Group") + geom_bar(stat = "identity", fill = "orange")


#Average listing price per neighborhood group
ggplot(data = aggregate(airbnb$price, list(airbnb$neighbourhood_group), mean), aes(Group.1, x)) + geom_col() + 
  xlab("Neighborhood Group") +  ylab("Average Price ($)") + ggtitle("Average Listing Price Per Neighborhood Group") + geom_bar(stat = "identity", fill = "orange")

#Count host ID by neighborhood group
ggplot(aggregate(airbnb$host_id, list(airbnb$neighbourhood_group), FUN=length), aes(Group.1, x)) + geom_col() + 
  xlab("Neighborhood Group") +  ylab("Count of Host ID") + ggtitle("Number of Hosts Per Neighborhood") + geom_bar(stat = "identity", fill = "gold")

#Room type and reviews_per_month
reviews.min <- ddply(airbnb, "room_type", summarise, xval=min(reviews_per_month), yval=min(price)) 
reviews.max <- ddply(airbnb, "room_type", summarise, xval=max(reviews_per_month), yval=max(price)) 
reviews <- rbind(reviews.min, reviews.max)  

ggplot(reviews, aes(x=xval, y=yval, group = room_type, color=room_type)) +
  geom_line(aes(linetype=room_type), size = 1.2) + geom_point(aes(shape=room_type), size = 4) +
  scale_shape_manual(values=c(6, 5, 7)) + scale_linetype_manual(values=c("dotdash", "dotted", "solid")) +
  xlab("Reviews Per Month") + ylab("Price") + ggtitle("Line Plot of Reviews Per Month and Price")

#Neighborhood Group and number of reviews
reviews.1 <- ddply(airbnb, "neighbourhood_group", summarise, xval=min(number_of_reviews), yval=min(price)) 
reviews.2 <- ddply(airbnb, "neighbourhood_group", summarise, xval=max(number_of_reviews), yval=max(price)) 
reviews3 <- rbind(reviews.1, reviews.2)  

ggplot(reviews3, aes(x=xval, y=yval, group = neighbourhood_group, color=neighbourhood_group)) +
  geom_line(aes(linetype=neighbourhood_group), size = 1.2) + geom_point(aes(shape=neighbourhood_group), size = 4) +
  scale_shape_manual(values=c(6, 5, 7, 4, 2)) + scale_linetype_manual(values=c("dotdash", "dotted", "solid", "longdash", "dotdash")) +
  xlab("Number of Reviews") + ylab("Price") + ggtitle("Line Plot of Number of Reviews and Price by Neighborhood")

#Linear regression on untransformed data
smooth <- ggplot(data=airbnb, aes(x=price, y=reviews_per_month, color=room_type)) + 
  geom_point(aes(shape=room_type), size=1.5) + xlab("Price") + ylab("Reviews Per Month") + 
  ggtitle("Scatterplot on Untransformed Data")
smooth + geom_smooth(method="lm")

#Linear model build out for log of numeric variables
set.seed(1234)

airbnb$log.price <- log(airbnb$price)
airbnb$log.number_of_reviews <- log(airbnb$number_of_reviews)
airbnb$log.reviews_per_month <- log(airbnb$reviews_per_month)
airbnb$log.availability <- log(airbnb$availability_365)

#Use 70% of dataset as training set and 30% as test set
sample <- sample.split(airbnb$host_id, SplitRatio = 0.7)
train  <- subset(airbnb, sample == TRUE)
test   <- subset(airbnb, sample == FALSE)

#Linear regression on training data
smooth <- ggplot(data=train, aes(x=log.price, y=log.reviews_per_month, color=room_type)) + 
  geom_point(aes(shape=room_type), size=1.5) + xlab("Price (Log)") + ylab("Reviews Per Month (Log") + 
  ggtitle("Scatterplot on Training Data with Log Transformations")
smooth + geom_smooth(method="lm")

#Linear regression on testing data
smooth <- ggplot(data=test, aes(x=log.price, y=log.reviews_per_month, color=room_type)) + 
  geom_point(aes(shape=room_type), size=1.5) + xlab("Price (Log)") + ylab("Reviews Per Month (Log") + 
  ggtitle("Scatterplot on Testing Data with Log Transformations")
smooth + geom_smooth(method="lm")

## Scatter plot on availability vs # of reviews
scatter <- ggplot(data=airbnb, aes(x = number_of_reviews, y = availability_365)) 
scatter + geom_point(aes(color=neighbourhood_group, shape=neighbourhood_group)) + xlab("# of Reviews") + ylab("Availability") + ggtitle("Availability VS. # of Reviews by Neighborhood")

