library(purrr)
library(furrr)
library(combinat)


'Furrr is a bridge between purrrs family of mapping functions and futures parallel processing capabilities. 
It attempts to make mapping in parallel as seamless as possible.'




'Function should accepts user-definied simulation functions along with parameter input,
f.e.

MC_function(x, y) with pramameter grid x=-10:10, y=0-1'

'Those grids should be generated automatically'


#Creates parameter grid
grid_function <- function(start, end, steps){
  output <- seq(from=start, to=end, by=steps)
  return(output)
}


grid_function(0, 10, 0.1)


'Supply the parameters as a list object?'


# define parameter grid:
n_grid <- c(50,100,250,500)
loc_grid <-seq(0,1,0.2)
scale_grid <-c(1,2)

# collect parameter grids in list:
param_list=list("n"=n_grid, "loc"=loc_grid, "scale"=scale_grid)

#Complete tidyMC function
tidyMC <-  function(simulation_function, parameters){
  grid_function(parameters1[0], parameters1[1], steps = 0.1)
  grid_function(parameters2[0], parameters2[1], steps = 0.1)
  simulation_function(n=1000, mean=)
  
}

param_vec1 <- c(0,10)
tidyMC(simulation_function=test_mc_function, parameters1=param_vec1)



test_mc_function <- function(n, mean, sd){
  output <- rnorm(n, mean, sd)
  return(output)
}



#Simple MC test function
#MC_test_2 <- function(x) x + 0.01
MC_test_3 <- function(x){
  rnorm(10, sd=x)
} 

#Simple case: Predefined parameter input for 1 variable
parameter_grid_func <- function(from, to, by) seq(from, to, by)

grid_x <- parameter_grid_func(0, 1, 0.1)

grid_x


#Next step (1): Use the grid and run the function with each parameter input
purrr::map_dbl(grid_x, MC_test_3)
MC_sim_test <- purrr::map_dbl(grid_x, MC_test_3) #this doesnt work, we need atomic output, we may need to pull the
#summary operation inside the function


#Next step (2): Find a way to apply a summary function onto the simulation (see functionals)
purrr::map_dbl(MC_sim_test, mean)

choose_summary <- function(f) f(MC_sim_test)
choose_summary(mean)
choose_summary(median)
choose_summary(max)

#Next step (3): Gernerate tidy output that prints nicely to the console
head(iris)
knitr::kable(iris)

'Include parallel computing (furrr) and maybe benchmark'
generate_data_function(n, parameter1, seed)

'Maybe compute a MC study where we compare OLS with GLS for data set with heteroscedasticity and show that
OLS is as efficient as GLS for large sample size'

#Homoscedastic dataset
x_vec <- 0:1000
y_vec <- x_vec + rnorm(1001, mean=0, sd=100)
plot(x_vec, y_vec)


#Heteroscedastic data
x_vec <- 0:1000
y_vec <- x_vec + rnorm(1001, mean=0, sd=x_vec*0.25) + rnorm(1001, mean=0, sd=20)
plot(x_vec, y_vec)

data_simulation <- as.data.frame(cbind(x_vec, y_vec))
colnames(data_simulation) <-  c("x", "y") #works

'We know beta_0 is zero and beta_1 is around 1'

model_ols <- lm(data=data_simulation, y ~  0 + x)
summary(model_ols)

