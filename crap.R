


function_new <- function(simulation, parameters){
  grid <- create_grid(parameters)
  
  if(ncol(grid)==1){
    #var1 <- c(grid[,1])
    var1 <- c(unlist(grid))
    data <- map(var1, simulation)
  } 
  
  else {
    
    if(ncol(grid)==2){
      var1 <- c((grid[,1]))
      var2 <- c((grid[,2]))
      data <- map2(var1, var2, .f=simulation)
    } 
    
    if(ncol(grid)>2){
      var1 <- list(grid[,1], grid[ ,2], grid[ ,3])
      data <- pmap(var1, .f=simulation)
    } }
  
  
  
  
  #names(data) <- "MC simulation"
  #comb <- cbind(grid, data)
  #return(data)
  return(data)
}































