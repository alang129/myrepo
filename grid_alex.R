require(utils)
library(tidyverse)
library(purrr)

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


#######################################################
#######################################################
#Part 2: Grid 



#Base R version
expand.grid(var_a = 0:10, sex = c("Male","Female"), dead = c("yes", "no"))


#tidyverse version

grid <- expand_grid(n = c(10,20,30), alpha =  c(0.5, 1), beta = c(0,1))
grid2 <- expand_grid(n = c(10,20,30), mean =  c(0, 1), sd = c(0, 0.5, 1))
nrow(grid)


a <- as.list(unlist(grid2[,1]))
b <- as.list(unlist(grid2[,2]))
c <- as.list(unlist(grid2[,3]))

list(a,b,c)


test <- function(n, mean, sd){
  a <- rnorm(n, mean, sd)
  return(c(a))
}

pmap(list(a,b,c), rnorm)


a <- as.list(unlist(grid[,1]))
b <- as.list(unlist(grid[,2]))

map2(a, b, dgp)
pmap(a, b, dgp) #dpmap doesnt work with 2 elements

data <- dgp(20, 1)
class(data)


map2(x, y, ~ .x * .y)
map2_dbl(y, z, ~ .x / .y)

