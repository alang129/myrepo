


dgp <-  function(n){
  errors <- arima.sim(model = list(ma = c(0.5)), n = n) # MA(1)
  beta0 <- rep(2, n) #beta0 = 2
  beta1 <- 3 #beta1 = 3
  x <- 0:(n-1) #x vector
  y <- beta0 + errors
  return(y)
}


#Create Matrix with length of vector y and set all elements on the main diagonal

create_covMat <- function(n){ #creates matrix V
  covMat <- diag((1+0.5^2), n, n)
  
  for(i in 1:n-1){
    covMat[i,i+1] <- 0.5
    covMat[i+1,i] <- 0.5
  }
  
  return(covMat)
}


create_cholsky <- function(matrix){ #cholsky decomposition
  inverse <- solve(matrix)
  cholsky <- chol(inverse)
  return(cholsky)
}


trasform_variables <- function(y, C){
  #C <- create_cholsky(matrix)
  y_tilde <- C%*%y
  return(y_tilde)
}




MC_sim <- function(n){
  y <- dgp(n)
  V <- create_covMat(n)
  C <- create_cholsky(matrix=V)
  y.new <- trasform_variables(y, C)
  intercept <- C%*%rep(1, length(y))
  
  myGLS <- lm(y.new ~ intercept + 0)
  myOLS <- lm(y ~ 1)
  
  sumGLS <- summary(myGLS)
  sumOLS <- summary(myOLS)
  diff <- sumGLS$sigma^2 - sumOLS$sigma^2
  output <- c(sumGLS$sigma^2, sumOLS$sigma^2, diff)
  
  
  return(output)
}

MC_sim(n=100)


vec <- seq(10, 2000, 10)
MC <- sapply(vec, function(i) MC_sim(n=i))
rowMeans(MC)





