library(purrr)
library(furrr)
library(combinat)
library(parallel)


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

'mc_complete <- function(mc_fun, from, to, by, summary_functions, seed){
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
}'

###

mc_complete <- function(mc_fun, from, to, by, summary_functions, input_var,
                        output_var, seed = NULL, summarise = FALSE 
                        , Workers= NULL, parallel = FALSE, type = NULL ){
  
  max_cores <-  detectCores() #check number of cores
  parameter_grid <- seq(from, to, by) #create parameter grid
  
  if(!is.null(seed)) {#Reproducibility
    set.seed(seed)}#If seed provided then set.seed takes the number
  else {
    warning("No seed provided!", call. = FALSE)
    seed <- sample.int(10000, 1)#if its not provided then we generate random seed
    set.seed(seed)
    message("Random seed = ", seed, "\n")} 


  if(Workers > max_cores){# User cannot choose the number of cores more than maximum
    stop("Number of Cores cannot be bigger than total number of cores")
 }
  if( parallel == TRUE){
    if(type == "multisession"){
      plan(multisession,workers = Workers)
      mc_sim <- furrr::future_map_dfc(parameter_grid, mc_fun,.options = furrr_options(seed = TRUE))
    } else {
      plan(sequential)
      mc_sim <- furrr::future_map_dfc(parameter_grid, mc_fun,.options = furrr_options(seed = TRUE))
    }
  } else{
    mc_sim <- purrr::map_dfc(parameter_grid, mc_fun)
  }

  
  df_sim <- as.data.frame(cbind(parameter_grid, t(mc_sim)))
  colnames(df_sim) <- c(input_var, output_var)
  
  
  dim_row <- dim(df_sim)[2]-1
  
  if(summarise == TRUE){
    if(missing(summary_functions)){
      stop("Please provide a list of summary functions you want to use!")
    }
  dim_col <- length(summary_functions)
  sum_matrix <- matrix(0, nrow=dim_row, ncol=dim_col)
  
  for(i in 2:dim(df_sim)[2]){
    input <- as.vector(unlist(df_sim[i]))
    sum_matrix[i-1,] <- sapply(summary_functions, do.call, list(input))
    
  }
  
  df_mat <-  as.data.frame(sum_matrix)
  names(df_mat) <- summary_functions
  
  df_mat_transposed <- t(df_mat)
  colnames(df_mat_transposed) <- output_var
  
  
  results <-  list(knitr::kable(df_sim, row.names = FALSE)
                   , knitr::kable(df_mat_transposed))
  
  names(results) <- c("Monte Carlo simulation", "Summary statistics")
  
  return(results)
} else {
  result <- list(knitr::kable(df_sim, row.names = FALSE))
  names(result) <- "Monte Carlo simulation"
  return(result)
}
}





'mc_complete(mc_fun = MC_sim_fixed_alpha
            , from=50, to=300, by=50
            , summary_functions = list("mean", "median", "min", "max", "sd", "var")
            , input_var = "n"
            , output_var = c("OLS", "GLS", "Difference")
            , seed = NULL,summarise = FALSE
            , max_cores= detectCores()
            , Workers= 5,parallel = TRUE
            , type="multisession" )

'

# with seed
mc_complete(mc_fun = MC_sim_fixed_alpha
            , from=50, to=300, by=50
            , summarise = TRUE
            #, summary_functions = list("mean", "median", "min", "max", "sd", "var")
            , input_var = "n"
            , output_var = c("OLS", "GLS", "Difference")
            , seed = 1234
            , Workers= 4, parallel = TRUE
            , type="multisession" )



###################### 
####################### OOP approach

'I tried to outsource some functions out of the main code to make it less complicated'



mc_complete2 <- function(mc_fun, from, to, by, summary_functions, input_var,
                        output_var, seed = NULL, summarise = FALSE 
                        , Workers= NULL, parallel = FALSE, type_parall = NULL ){
  
  parameter_grid <- seq(from, to, by) #create parameter grid
  
  
  if(!is.null(seed)) {#Reproducibility
    set.seed(seed)}#If seed provided then set.seed takes the number
  else {
    #warning("No seed provided!", call. = FALSE)
    seed <- sample.int(10000, 1)#if its not provided then we generate random seed
    set.seed(seed)
    message("No seed provided! Randomly created seed = ", seed, "\n")} 
  
  
  if(parallel == TRUE){ #check condition for parallelisation
    mc_sim <- parallelisation_oop(Workers, type_parall, parameter_grid, mc_fun)
  } else {
    mc_sim <- suppressMessages(purrr::map_dfc(parameter_grid, mc_fun))
    }
    
  df_sim <- as.data.frame(cbind(parameter_grid, t(mc_sim))) #resuluts of MC simulation
  colnames(df_sim) <- c(input_var, output_var)



  if(summarise == TRUE){ #output with summary = TRUE
    return(summary_oop(summary_functions, df_sim, output_var))
  } 
  else { #output with summary = FALSE
    return(knitr::kable(df_sim, row.names = FALSE))
  }
}


#####################################
# Helper function for parallelisation
parallelisation_oop <- function(Workers, type_parall, parameter_grid, mc_fun){

  
  if(Workers > detectCores()) {stop("Number of Cores cannot be bigger than total number of cores")}


  if(length(type_parall)==0){
    plan(sequential)
    mc_sim <- suppressMessages(furrr::future_map_dfc(parameter_grid
                                                     , mc_fun
                                                     ,.options = furrr_options(seed = TRUE)))
    return(mc_sim)
    } else {
    if(type_parall == "multisession"){
              plan(multisession,workers = Workers)
              mc_sim <- suppressMessages(furrr::future_map_dfc(parameter_grid
                                                               , mc_fun
                                                               ,.options = furrr_options(seed = TRUE)))
        return(mc_sim)
              } else {stop("Invalid type!")}
    }
}


#############################
# Helper function for summary
summary_oop <- function(summary_functions, df_sim, output_var){
  if(missing(summary_functions)){
    stop("Please provide a list of summary functions you want to use!")
  }
  else{
    dim_row <- dim(df_sim)[2]-1
    dim_col <- length(summary_functions)
    sum_matrix <- matrix(0, nrow=dim_row, ncol=dim_col)
    
    for(i in 2:dim(df_sim)[2]){
      input <- as.vector(unlist(df_sim[i]))
      sum_matrix[i-1,] <- sapply(summary_functions, do.call, list(input))
    }
    
    df_mat <-  as.data.frame(sum_matrix)
    names(df_mat) <- summary_functions
    
    df_mat_transposed <- t(df_mat)
    colnames(df_mat_transposed) <- output_var
    
    results <-  list(knitr::kable(df_sim, row.names = FALSE)
                     , knitr::kable(df_mat_transposed))
    
    names(results) <- c("Monte Carlo simulation", "Summary statistics")
    
  }
  return(results)
}

#######################
# output command
mc_complete2(mc_fun = MC_sim_fixed_alpha
            , from=50, to=400, by=50
            , summarise = TRUE
            , summary_functions = list("mean", "median", "min", "max", "sd", "var")
            , input_var = "n"
            , output_var = c("OLS", "GLS", "Difference")
            , seed = 7162
            , Workers= 4
            , parallel = TRUE
            , type_parall="multisession")


'Invalid type works
No type works
multisession works
parallel = TRUE works'



