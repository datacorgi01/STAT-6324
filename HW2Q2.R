#Problem 2
#Part a - generate random data for x and y
randx1 <- rnorm(100, mean = 60, sd = 5)
randy <- rnorm(100, mean = 150, sd = 15)
df <- data.frame(randx1, randy)

#Part b - check linearity
test_lm <- lm(randy ~ randx1, data=df)

#From part b, slope of randx is -.7477 and intercept is 194.71

#Part c - cost and gradient descent function
#Gradient descent algorithm takes in an x, y, alpha (learning rate), ct (convergence threshold), n (number of observations), and max iterations
gradDesc <- function(x, y, alpha, ct, n, max_its) {
  plot(x, y, col = "orange", pch = 16, main = "Gradient Descent Algorithm Plot")
  #Generate uniform random numbers
  B1 <- runif(1, 0, 1)
  B0 <- runif(1, 0, 1)
  
  #Predicted y values
  yhat <- B1 * x + B0
  
  #MSE cost function
  MSE <- sum((yhat - y) ^ 2) / n
  
  #Other variable defs
  converged = F
  iterations = 0
  
  #Gradient descent algorithm
  while(converged == F) {
    B1_gd <- B1 - alpha * ((1 / n) * (sum((yhat - y) * x)))
    B0_gd <- B0 - alpha * ((1 / n) * (sum(yhat - y)))
    B1 <- B1_gd
    B0 <- B0_gd
    yhat <- B1 * x + B0
    
    #New MSE cost function with updated beta values
    MSE_new <- sum((yhat - y) ^ 2) / n
    
    #End the function if the MSE difference is less than the convergence threshold
    if(MSE - MSE_new <= ct) {
      abline(B0, B1) 
      converged = T
      return(paste("Optimal intercept:", B0, "Optimal slope:", B1))
    }
    iterations = iterations + 1
    
    #End the function if the number of iterations exceeds the max number of iterations specified
    if(iterations > max_its) { 
      abline(B0, B1) 
      converged = T
      return(paste("Optimal intercept:", B0, "Optimal slope:", B1))
    }
  }
}

#Part d - run below and get roughly the same slope and intercept as the lm output
gradDesc(randx1, randy, 0.00009, .06, 100, 25000000)
