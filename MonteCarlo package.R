library(MonteCarlo)


MonteCarlo() 
MakeTable()

'loc is just the mean'

ttest<-function(n,loc,scale){ 
  
  # generate sample:
  sample<-rnorm(n, loc, scale)
  
  # calculate test statistic:
  stat<-sqrt(n)*mean(sample)/sd(sample)
  
  # get test decision:
  decision<-abs(stat)>1.96
  
  # return result:
  return(list("decision"=decision))
}

ttest(100,1,1)


# define parameter grid:

n_grid<-c(50,100,250,500)
loc_grid<-seq(0,1,0.2)
scale_grid<-c(1,2)

# collect parameter grids in list:
param_list=list("n"=n_grid, "loc"=loc_grid, "scale"=scale_grid)

MC_result<-MonteCarlo(func=ttest, nrep=1000, param_list=param_list)

summary(MC_result)

MakeTable(output=MC_result, rows="n", cols=c("loc","scale"), digits=2, include_meta=FALSE)

