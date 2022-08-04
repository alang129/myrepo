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


####################
#test run with rnorm
create_grid <- function(input){
  storage <- list()
  name_vec <- c()
  
  for(i in 1:length(input)){ #1:3
    a <- as.numeric(input[[i]][[2]])
    b <- as.numeric(input[[i]][[3]])
    c <- as.numeric(input[[i]][[4]])
    output <- seq(from=a, to=b, by=c)
    storage[[i]] <-  output
    name_vec[i] <- input[[i]][[1]]
  }
  
grid <- expand_grid(unlist(storage[1])
            , unlist(storage[2])
            , unlist(storage[3])
            , unlist(storage[4])
            , unlist(storage[5]))

names(grid) <- name_vec

  return(grid)
  #return(list(storage, grid))
}

'Grid works well.'


##############################################
# MC Sim over grid

##############################################
function_new1 <- function(simulation, parameters){
  grid <- create_grid(parameters)
  
  if(ncol(grid)==1){
    var1 <- c(unlist(grid))
    data <- map(var1, simulation)
  }
  
  if(ncol(grid)==2){
    var1 <- c(unlist(grid[,1]))
    var2 <- c(unlist(grid[,2]))
    data <- map2(var1, var2, simulation)
  } 
  
  
  if(ncol(grid)==3){
    var1 <- c(unlist(grid[,1]))
    var2 <- c(unlist(grid[,2]))
    var3 <- c(unlist(grid[,3]))
    list1 <- list(var1,var2,var3)
    data <- pmap(list1, .f=simulation)
  } 
  
  return(data)
}

################## Test #####################

#mit einer Variable
param_list1 <- list(c("n", 10, 50, 10))

create_grid(param_list1)
function_new1(simulation=rnorm, parameters=param_list1) #1 variablen

#mit zwei Variablen
param_list2 <- list(c("n", 10, 20, 10),
                    c("mean", 0, 1, 0.25))

test_grid2 <- create_grid(param_list2)
function_new1(simulation=rnorm, parameters=param_list2)

#mit drei Variablen
param_list3 <- list(c("n", 10, 20, 10),
                    c("mean", 0, 1, 0.25),
                    c("sd", 0, 0.3, 0.1))

test_grid3 <- create_grid(param_list0)
function_new1(simulation=rnorm, parameters=param_list0)






