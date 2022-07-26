library(purrr)
library(furrr)
library(combinat)


'Furrr is a bridge between purrrs family of mapping functions and futures parallel processing capabilities. 
It attempts to make mapping in parallel as seamless as possible.'




'Function should accepts user-definied simulation functions along with parameter input,
f.e.'

'Those grids should be generated automatically'


MC_sim_fixed_alpha <- function(n){
  output <- MC_sim(n, alpha=0.5)
  return(output)
}


MC_sim(n=100, alpha = 0.5) #our monte carlo simulation function
MC_sim_fixed_alpha(n=100)


'Supply the parameters as a list object?'

'summary_functions can have vector or list as input, mc_sim needs to be list. 
summary functions need to be contain the string of the wanted functions!'


### original

mc_complete <- function(mc_fun, from, to, by, summary_functions, seed){
  set.seed(seed)
  parameter_grid <- seq(from, to, by)
  
  mc_sim <- purrr::map_dbl(parameter_grid, mc_fun)
  
  df_sim <- as.data.frame(cbind(parameter_grid, mc_sim))
  
  summary_output <- sapply(summary_functions, do.call, as.list(mc_sim))
  names(summary_output) <- summary_functions

  
  results <-  list(knitr::kable(df_sim)
                   , knitr::kable(summary_output))
  names(results) <- c("simulation", "Summary statistics")
  
  return(as.list(mc_sim))
}

###

mc_complete <- function(mc_fun, from, to, by, summary_functions, input_var, output_var,  seed){
  set.seed(seed)
  parameter_grid <- seq(from, to, by)
  
  mc_sim <- purrr::map_dfc(parameter_grid, mc_fun)
  
  df_sim <- as.data.frame(cbind(parameter_grid, t(mc_sim)))
  colnames(df_sim) <- c(input_var, output_var)
  
  
  dim_row <- dim(df_sim)[2]-1
  dim_col <- length(summary_functions)
  sum_matrix <- matrix(0, nrow=dim_row, ncol=dim_col)
  
  for(i in 2:dim(df_sim)[2]){
    input <- as.vector(unlist(df_sim[i]))
    sum_matrix[i-1,] <- sapply(summary_functions, do.call, as.list(input))
    
  }
  
  df_mat <-  as.data.frame(sum_matrix)
  names(df_mat) <- summary_functions
  
  df_mat_transposed <- t(df_mat)
  colnames(df_mat_transposed) <- output_var

  
  results <-  list(knitr::kable(df_sim)
                   , knitr::kable(df_mat_transposed))
  
  names(results) <- c("simulation", "Summary statistics")
  
  #return(results)
  return(results)
}


mc_complete(mc_fun = MC_sim_fixed_alpha
            , from=50, to=300, by=50
            , summary_functions = list("mean", "median", "min", "max")
            , input_var = "n"
            , output_var = c("OLS", "GLS", "Difference")
            , seed = 1234)



mat <- matrix(0, nrow=2, ncol=4)
df1 <- as.data.frame(mat)
names(df1) <- c("a", "b", "c", "d")

MC_sim_fixed_alpha(n=10)
grid_x <- seq(from=10, to=100, by=10)


test <- as.list(purrr::map_dfc(grid_x, MC_sim_fixed_alpha))
test[1]

test2 <- as.list(purrr::map_dbl(grid_x, MC_sim_fixed_alpha))
test2[1]

as.list(purrr::map_dbl(grid_x, MC_sim_fixed_alpha))

a <- as.vector(unlist(df[2]))
b <- as.list(a)
mean(b)
max(as.list(df[2]))

sapply(list("mean", "median", "min", "sd"), do.call, b)



sd(b)




length(grid_x)



MC_sim(n=10, alpha=0.5)
MC_sim_fixed_alpha(n=10)

test <- purrr::map_dfc(grid_x, MC_sim_fixed_alpha)

purrr::map_dbl(grid_x, MC_sim_fixed_alpha)
purrr::map_dfc(grid_x, MC_sim_fixed_alpha)
as.data.frame(test)


purrr::map_dbl(grid_x, MC_sim_fixed_alpha)

purrr::map_dfc(grid_x, MC_sim(alpha=0.5))


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

