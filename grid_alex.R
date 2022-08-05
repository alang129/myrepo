require(utils)
library(tidyverse)
library(purrr)



#######################################################
#######################################################
#Part 1: Grid 

'First step, built a function that creates a parameter grid with all permutations
of the given parameters'

####################
#test run with rnorm
create_grid <- function(parameters){
  input <- parameters
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

# TEST RUN GRID CREATION
param_list3 # 3 different parameters
create_grid(param_list3)

'Grid works well.'


##############################################
##############################################
# Part 2: Data generation over grid

'I decided to implement the data generation and summarisation seperatly, which
allows the user more flexibility, f.e. quickly changing the DGB while keeping the
summary statistics.

simulation = data generation function, f.e rnorm
parameters = list of parameters, f.e. param_list3 '

data_generation <- function(simulation, grid){
  #grid <- create_grid(parameters)
  
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

# TEST RUN DATE GENERATION
data_generation(simulation=rnorm, parameters=param_list3)
test_data <- data_generation(simulation=rnorm, parameters=param_list3)
'We see the data generation works as intenden and creates a list for each parameter
combination, which are available for further data analysis.'

##############################################
##############################################
# Part 3: Summary statistics

'Next we implement the summary function, that analyses the given data point.
sum_fun = the summary function we want to use on the data
data_input = the data points we want to analyse'

#summary function for one input
summary_function <- function(sum_fun, data_input){
  
  count <- length(data_input)
  summary_matrix <- matrix(nrow=count, ncol=1)
  
  for(i in 1:count){
    input <- list(data_input[[i]])
    output <- sapply(sum_fun, do.call, input)
    summary_matrix[i] <- output
  }
  #output <- as.data.frame(summary_matrix)
  #names(output) <- sum_fun
  colnames(summary_matrix) <- sum_fun
  return(summary_matrix)
}


# TEST RUN SUMMARY_FUNCTION
summary_function(sum_fun=list("mean"), data_input=test_data)

'Works like a charm. Next, we want to combine our results with the parameter grid and create
arrays, that structure our simulation input with the given results.'

##############################################
##############################################
# Part 5: Summary statistics

'Next, we want to store the summary statistics in an array, where you can easily read
the given parameter constellation'

create_array_function <- function(comb, parameters){
  input <- parameters
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
  
  
  
  comb_ordered <-  comb %>% arrange(comb[,2])  %>% arrange(comb[,3]) 

  
  seq1 <- c(unlist(storage[1]))
  seq2 <- c(unlist(storage[2]))
  seq3 <- c(unlist(storage[3]))
  

  row.names <- paste(name_vec[1],"=",seq1, sep = "")
  column.names <-  paste(name_vec[2],"=",seq2, sep = "")
  matrix.names <-  paste(name_vec[3],"=",seq3, sep = "")
  
  array1 <- array(comb_ordered[,4] #change to automatically adjust dim
                  , dim = c(length(seq1), length(seq2), length(seq3))
                  , dimnames=list(row.names, column.names, matrix.names)
                  )
  
  
  
  return(array1)
}

#namama <- paste("n =", seq(0, 0.5, 0.25))

test <- create_array_function(comb=test_data, parameters=param_list3)
test
class(test)

test_data
test_data %>% arrange(names(test_data)[3]) 
test_data %>% arrange(test_data[,2])  %>% arrange(test_data[,3]) 

# TEST RUN SUMMARY_FUNCTION

create_array_function(sum_fun=list("mean"), data_input=test_data)


##############################################
##############################################
##### main function part ###

main_function <-  function(parameters
                          , simulation
                          , sum_fun){
  
grid <- create_grid(parameters) #Step 1: create grid

raw_data <- data_generation(simulation, grid) #Step 2: simlate data

summary <- summary_function(sum_fun, data_input=raw_data) #Step 3: Summary statistics

comb <- cbind(grid, summary) #Step 4: Combine resuluts with parameters

#array_1 <- create_array_function(comb, parameters) #Step 5: Create array

return(comb)
  
}
test_data <- main_function(parameters=param_list3
              , simulation = rnorm
              , sum_fun="var")


mc_function <- function(grid_parameters, sum_fun_list, simulation_function){
  data <- data_generation(simulation=simulation_function, parameters=grid_parameters)
  summary <-summary_function(sum_fun = sum_fun_list, data_input = data)
  grid <- create_grid(input=grid_parameters)
  output <- cbind(grid, summary)
  return(output)
}





# use param_list3
mc_function(grid_parameters = param_list3
            , sum_fun_list=list("mean", "min", "max", "ttest", "ols")
            , simulation_function = rnorm)

mc_function(grid_parameters = param_list3
            , sum_fun_list=list("mean")
            , simulation_function = rnorm)



summary_function(sum_fun=list("mean", "min", "max", "ttest", "ols") #works
                  , data_input = test_data)


sim <- mc_function(grid_parameters = param_listx
            , sum_fun_list=list("var")
            , simulation_function = rnorm)





'Es werden immer zuerst die Spalten einer Matrix befüllt'

column.names <- c("mean=0", "mean=0.25", "mean=0.5")
#namama <- paste("n =", seq(0, 0.5, 0.25))
row.names <- c("n=10", "n=20")
matrix.names <- c("sd=1", "sd=2")


array1 <- array(sim1[,4]
                , dim = c(2, 3, 2)
                , dimnames=list(row.names, column.names, matrix.names))
array1

param_listx <-  list(c("n", 10, 20, 10),
                     c("mean", 0, 0.5, 0.25),
                     c("sd", 1, 2, 1))


array(c(vector1, vector2), dim = c(3, 3, 2))

# Create two vectors of different lengths.
vector1 <- c(5, 9, 3)
vector2 <- c(10, 11, 12, 13, 14, 15)

# Take these vectors as input to the array.
result <- array(c(vector1, vector2), dim = c(3, 3, 2))
print(result)







### t test
ttest<-function(data_input){ 
  
  # generate sample:
  #sample<-rnorm(n, loc, scale)
  
  # calculate test statistic:
  stat<-sqrt(length(data_input))*mean(data_input)/sd(data_input)
  
  # get test decision:
  decision<-abs(stat)>1.96
  
  # return result:
  return(decision)
  #return(list("decision"=decision))
}

dat <- test_data[[1]]
ttest(data_input=dat)



### OLS Function

first_part <- test_data[[1]]
lm(data=first_part, y)
x_vec <- 1:length(first_part)

ols <- function(data_input){
  y <- data_input
  myOLS <- lm(y ~ 1)
  sumOLS <- summary(myOLS)
  output <- sumOLS$sigma^2
  return(output)
}


ols(dat)





################## Test #####################

#mit einer Variable
param_list1 <- list(c("n", 10, 50, 10))

create_grid(param_list1)
data_generation(simulation=rnorm, parameters=param_list1) #1 variablen

#mit zwei Variablen
param_list2 <- list(c("n", 10, 20, 10),
                    c("mean", 0, 1, 0.25))

test_grid2 <- create_grid(param_list2)
data_generation(simulation=rnorm, parameters=param_list2)

#mit drei Variablen
param_list3 <- list(c("n", 10, 20, 10),
                    c("mean", 0, 1, 0.25),
                    c("sd", 0, 0.3, 0.1))

test_grid3 <- create_grid(param_list0)
data_generation(simulation=rnorm, parameters=param_list0)





