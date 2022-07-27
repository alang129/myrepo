

'Data generating process, alpha is the MA-parameter'
dgp <-  function(n, alpha){
  errors <- arima.sim(model = list(ma = c(alpha)), n = n) # MA(1)
  beta0 <- rep(2, n) #beta0 = 2
  beta1 <- 3 #beta1 = 3
  x <- 0:(n-1) #x vector
  y <- beta0 + errors
  return(y)
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

#'Transformation for GLS'
#trasform_GLS <- function(y, C){
#  y_tilde <- C%*%y
#  return(y_tilde)
#}



'Complete MC simulation'
MC_sim <- function(n, alpha){
  set.seed(1234)
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
  
  return(output1)
}




'MC_sim(n, alpha) is a function, that creates a data vector with n entrys, where
we have a underlying model y=2 + error, where the error term follows an MA(1) process
with coefficient alpha.'




