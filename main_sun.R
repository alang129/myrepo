


main_function <- function(nrep, cores, parameters, sum_fun, simulation) {


####################################################################################
  create_grid <- function(parameters, nrep){
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
                        , unlist(storage[5])
                        , c(1:nrep))

    names(grid) <- c(name_vec, "rep")

    return(grid)
  }



##############################################################################################
  data_generation <- function(simulation, grid){
    #this is for use inside the function

    if(ncol(grid)==2){
      var1 <- c(unlist(grid[,1]))
      data <- map(var1, simulation)
      #different purrr-functions depending on how many input variables we use
    }

    if(ncol(grid)==3){
      var1 <- c(unlist(grid[,1]))
      var2 <- c(unlist(grid[,2]))
      data <- map2(var1, var2, simulation)
    }

    if(ncol(grid)==4){
      var1 <- c(unlist(grid[,1]))
      var2 <- c(unlist(grid[,2]))
      var3 <- c(unlist(grid[,3]))
      list1 <- list(var1,var2,var3)
      data <- pmap(list1, .f=simulation)
    }

    return(data)
  }


####################################################################################



  array_1 <- function(comb, parameters, nrep){
    storage <- list()
    name_vec <- c()

    for(i in 1:length(parameters)){ #this creates the sequences of parameters
      a <- as.numeric(parameters[[i]][[2]])
      b <- as.numeric(parameters[[i]][[3]])
      c <- as.numeric(parameters[[i]][[4]])
      output <- seq(from=a, to=b, by=c)
      storage[[i]] <-  output
      name_vec[i] <- parameters[[i]][[1]] #this just stores the names of the variables
    }


    matrix.numeration <-  paste("rep","=", 1:nrep, sep = "")

    if(length(parameters)==1){
      comb_ordered <-  comb %>% arrange(comb[,2])
      seq1 <- c(unlist(storage[1]))

      row.names <- paste(name_vec[1],"=",seq1, sep = "")

      dimension_array <- c(length(seq1), nrep)
      dim_names_list <- list(row.names, matrix.numeration)
    }

    if(length(parameters)==2){
      comb_ordered <-  comb %>% arrange(comb[,2])  %>% arrange(comb[,3])
      seq1 <- c(unlist(storage[1]))
      seq2 <- c(unlist(storage[2]))

      row.names <- paste(name_vec[1],"=",seq1, sep = "")
      column.names <-  paste(name_vec[2],"=",seq2, sep = "")

      dimension_array <- c(length(seq1), length(seq2), nrep)
      dim_names_list <- list(row.names, column.names, matrix.numeration)
    }

    if(length(parameters)==3){
      comb_ordered <-  comb %>% arrange(comb[,2])  %>% arrange(comb[,3]) %>% arrange(comb[,4])
      seq1 <- c(unlist(storage[1]))
      seq2 <- c(unlist(storage[2]))
      seq3 <- c(unlist(storage[3]))

      row.names <- paste(name_vec[1],"=",seq1, sep = "")
      column.names <-  paste(name_vec[2],"=",seq2, sep = "")
      matrix.names1 <-  paste(name_vec[3],"=",seq3, sep = "")

      dimension_array <- c(length(seq1), length(seq2), length(seq3), nrep)
      dim_names_list <- list(row.names, column.names, matrix.names1, matrix.numeration)

    }


    array1 <- array(comb_ordered[,ncol(comb)] #change to automatically adjust dim
                    , dim = dimension_array
                    , dim_names_list)
    return(array1)

  }

################################################################################################

    main_function_array_test <-  function(parameters #list of parameters
                                          , nrep #number of repetitions
                                          , simulation #data genereation
                                          , sum_fun){ #summary statistics

      grid <- create_grid(parameters, nrep) #Step 1: create grid

      raw_data <- data_generation(simulation, grid) #Step 2: simlate data

      summary <- summary_function(sum_fun, data_input=raw_data) #Step 3: Summary statistics

      comb <- cbind(grid, summary) #Step 4: Combine resuluts with parameters

      array_1 <- create_array_function(comb, parameters, nrep) #Step 5: Create array

      return(comb)

    }




############################################################################################

    mylist <- list(nrep, cores, parameters, array_1, simulation)
    names(mylist) <- c("nrep", "cores", "parameters", "array_1", "simmulation")
    class(mylist) <- "Eco"

    mylist$results <- array_1 # need to change

#    if(cores>1){
#      parallel = "Multisession"
#    } else {
#      parallel = "Sequential"
#    }
############################################################################################

    output_function <- function(mylist) {

     if(class(mylist) == "Eco"){

       cat("\n",
           "Repetition(nrep)      : ",nrep,"\n\n",
           "Parallelization Type  : ",parallel,"\n\n",
           "Number of Cores Used in  Parallelization : ",cores," out of",detectCores(),"\n\n",
           "Input Parameters : ",paste(parameters),"\n\n",
           "Simulation Length :",length(array_1),"\n",
           "Minumum :",min(array_1),"\n",
           "Maximum :",max(array_1),"\n",
           "Mean    :", mean(array_1),"\n",
           "Median  :",median(array_1),"\n\n",
           "Quantiles :",names(quantile(dt$results)),"\n",
           "          ",round(quantile(dt$results),digits=1),"\n",
           #"Simulation Function : " ,print(simulation),"\n\n",
           "Execution Time of Monte Carlo Simulation",as.numeric(cpt),"secs \n",
           "Name of The Class :",class(mylist))
      }

      else {
        message("Object not of class Eco!")
      }



    }

    result <- print(output_function(mylist))
    return(result)

}

print(main_function(234567))


















test_par <- list(c("n", 10, 20, 10))
test_sum_fun <- list("mean")

test_main_function <- main_function(nrep = 2, cores = 1, parameters = test_par,
                          sum_fun = test_sum_fun, simulation = rnorm)
print(test_main_function)
