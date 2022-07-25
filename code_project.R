library(purrr)
library(furrr)
library(combinat)


'Furrr is a bridge between purrrs family of mapping functions and futures parallel processing capabilities. 
It attempts to make mapping in parallel as seamless as possible.'




'Function should accepts user-definied simulation functions along with parameter input,
f.e.'

'Those grids should be generated automatically'

MC_sim(n=100, alpha = 0.5) #our monte carlo simulation function
MC_sim_fixed_alpha <- function(n){
  output <- MC_sim(n, alpha=0.5)
  return(output)
}
MC_sim_fixed_alpha(n=100)


'Supply the parameters as a list object?'



mc_complete <- function(mc_fun, from, till, by, summary_fun1, summary_fun2, summary_fun3){
  string1 <- deparse(substitute(summary_fun1)) #output string of summary-function name
  string2 <- deparse(substitute(summary_fun2)) #output string of summary-function name
  string3 <- deparse(substitute(summary_fun3)) #output string of summary-function name
  parameter_grid <- seq(from, till, by)
  
  mc_sim <- purrr::map_dbl(parameter_grid, mc_fun)
  df_sim <- as.data.frame(cbind(parameter_grid, mc_sim))
  
  a <- summary_fun1(mc_sim) 
  b <- summary_fun2(mc_sim)
  c <- summary_fun3(mc_sim)
  
  summary_output <- c(a, b, c)
  names(summary_output) <- c(string1, string2, string3)
  
  
  results <-  list(knitr::kable(df_sim)
                   , knitr::kable(summary_output))
  
  names(results) <- c("simulation", "Summary statistics")
  


  return(results)
}


mc_complete(mc_fun = MC_sim_fixed_alpha
            , from=50, till=250, by=50
            , summary_fun1 = mean
            , summary_fun2 = max
            , summary_fun3 = min)














choose_summary <- function(f) f(1:100)
choose_summary(mean)
choose_summary(median)
choose_summary(max)

sum_fun_list <- c("mean", "max", "median")
sum_fun_list <- c(mean, max, median)
choose_summary(sum_fun_list[1])

class(as.function(unlist(sum_fun_list)))
class(sum_fun_list)

choose_summary(as.function(sum_fun_list[1]))


str(unlist(sum_fun_list))
str(unlist(sum_fun_list[1]))

unlist(sum_fun_list[1])(1:10)

choose_summary(unlist(sum_fun_list[1]))




grid_x <- mc_complete(from=10, till=500, by=10)
length(grid_x)
purrr::map_dfc(grid_x, MC_sim_fixed_alpha)
purrr::map_dbl(grid_x, function(x) x+1)


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

