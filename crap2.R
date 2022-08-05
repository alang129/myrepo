#######################################################
#######################################################
#Part 1: Monte Carlo Function
'Data generating process, alpha is the MA-parameter'
dgp <-  function(n, alpha){
  errors <- arima.sim(model = list(ma = c(alpha)), n = n) # MA(1) process
  beta0 <- rep(2, n) #beta0 = 2
  beta1 <- 3 #beta1 = 3
  x <- 0:(n-1) #x vector
  y <- beta0 + errors
  return(as.vector(y))
}

'create Matrix V (var-cov matrix)'
create_covMat <- function(n, alpha){
  covMat <- diag((1+alpha^2), n, n)
  
  for(i in 1:n-1){
    covMat[i,i+1] <- alpha
    covMat[i+1,i] <- alpha
  }
  return(covMat)
}

'Cholsky decomposition'
create_cholsky <- function(matrix){
  inverse <- solve(matrix)
  cholsky <- chol(inverse)
  return(cholsky)
}



'Complete MC simulation'
MC_sim <- function(n, alpha){
  y <- dgp(n, alpha) #creates data (n draws) with alpha as MA parameter
  V <- create_covMat(n, alpha) #creates var-cov matrix V
  C <- create_cholsky(matrix=V) #Cholsky decomposition
  
  y.new <- C%*%y #transform y
  intercept <- C%*%rep(1, length(y)) #transform intercept
  
  myGLS <- lm(y.new ~ intercept + 0)
  myOLS <- lm(y ~ 1)
  
  sumGLS <- summary(myGLS)
  sumOLS <- summary(myOLS)
  
  diff <- sumOLS$sigma^2 - sumGLS$sigma^2 
  output1 <- c(sumOLS$sigma^2, sumGLS$sigma^2, diff)
  
  return(sumOLS$sigma^2)
}




'MC_sim(n, alpha) is a function, that creates a data vector with n entrys, where
we have a underlying model y=2 + error, where the error term follows an MA(1) process
with coefficient alpha.'


# This summary function is for multiple functions, not needed I guess
'summary_function <- function(sum_fun, data_input){
  
  summary_matrix <- matrix(nrow=length(data_input), ncol=length(sum_fun))
  
  for(i in 1:length(data_input)){
  input <- list(data_input[[i]])
  output <- sapply(sum_fun, do.call, input) #works
  summary_matrix[i,] <- output
  }
  output <- as.data.frame(summary_matrix)
  names(output) <- sum_fun
  return(output)
  
}'

