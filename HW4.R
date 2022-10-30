#Problem 8 
#Load in boot library for cv.glm() function
library(boot)

#Part A - generate random data
set.seed(1)
x <- rnorm(100)
y <- x - 2 * x^2 + rnorm (100)

#Part B - Scatterplot of the original data
plot(x, y, ylab = "y", xlab = "x")

#Part C - Leave One Out Cross Validation
#Take one observation out and remaining observations used as training set

#Combine x and y into data frame
newdata <- data.frame(x, y)

#Powers being stored: 1-4 as we are going up to a 4th degree polynomial
pows <- 1:4

#Create a cross validation error vector errorcv with initial error of 0 repeated 4 times for 4 polynomials
errorcv <- rep(0, 4)

#Create an empty list for values regarding how well the model fits
fit <- list()  

#For every power, loop through and fit a model to the data. Then check the error using the cv.glm function. 
#Then it prints out the metrics surrounding the model's performance for each term added to the polynomial equation (from 1-4). 
for (i in pows) {
  fit[[i]] <- glm(y ~ poly(x, i), data = newdata)
  errorcv[i] <- cv.glm(newdata, fit[[i]])$delta[1]
  cat("Model #:", i, "\n")
  print(tidy(fit[[i]]))
}

#Review error for all models
print(errorcv)

#Part D - Use another seed and repeat part C
set.seed(37)

#Powers being stored: 1-4 as we are going up to a 4th degree polynomial
pows1 <- 1:4

#Create a cross validation error vector errorcv with initial error of 0 repeated 4 times for 4 polynomials
errorcv1 <- rep(0, 4)

#Create an empty list for values regarding how well the model fits
fit1 <- list()  

#For every power, loop through and fit a model to the data. Then check the error using the cv.glm function. 
#Then it prints out the metrics surrounding the model's performance for each term added to the polynomial equation (from 1-4). 
for (i in pows1) {
  fit1[[i]] <- glm(y ~ poly(x, i), data = newdata)
  errorcv1[i] <- cv.glm(newdata, fit1[[i]])$delta[1]
  cat("Model #:", i, "\n")
  print(tidy(fit[[i]]))
}

#Review error for all models
print(errorcv1)

#Part F
fit4 <- glm(y ~ poly(x, 4))
cv.glm(newdata, fit4)$delta[1]
summary(fit4)

#Problem 9
library('MASS')
data('Boston')
#Learning about the data columns
?Boston

#Part A - Estimate for population mean
mu_hat <- mean(Boston$medv)

#Part B - Estimate of standard error for mu_hat
stderr <- sqrt(var(Boston$medv)/nrow(Boston))

#Part C - Estimate  the standard error for mu_hat using the bootstrap method
set.seed(1)

#Function that calculates the mean
mean.fcn = function(data, index) {
  mu <- mean(data[index])
  return(mu)
}

#Initiate the bootstrap method using the boot function with 1000 iterations
boot_stderr = boot(Boston$medv, mean.fcn, R=1000)

#Print the output
print(boot_stderr)

#Part D - Provide a confidence interval for the mean
mu_boot = boot_stderr$t0
se_boot = sd(boot_stderr$t)

#Calculate the 95% confidence interval
low <- mu_boot - 2 * se_boot
high <- mu_boot + 2 * se_boot
conf.int <- c(low, high)
print(conf.int)

#Compare to t.test
t.test(Boston$medv)

#Part E - Estimate for median
median_hat <- median(Boston$medv)
print(median_hat)

#Part F - Estimate  the standard error for median_hat using the bootstrap method
#Create a function to find the median
median.fcn = function(data, index) {
  med <- median(data[index])
  return(med)
}

#Initiate the bootstrap method using the boot function with 1000 iterations
boot_stderr1 = boot(Boston$medv, median.fcn, R=1000)

#Print the output
print(boot_stderr1)

#Part G - Provide an estimate for the 10th percentile for medv
mu_hat0.1 = quantile(Boston$medv, 0.1)
print(mu_hat0.1)

#Part H - Use the bootstrap method to estimate the standard error of mu_hat0.1
set.seed(1)

#Define the quant function to return the quantile at 0.1
quant.fn = function(data, index) {
  boot_muhat0.1 <- quantile(data[index],c(0.1))
  return (boot_muhat0.1)
}

#Initiate the bootstrap method using the boot function with 1000 iterations
quantSE_boot = boot(Boston$medv, quant.fn, R=1000)

#Print the result
print(quantSE_boot)

