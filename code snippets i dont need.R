#How to get functionals to work

choose_summary <- function(f) f(1:100)
choose_summary(mean)
choose_summary(median)
choose_summary(max)

sum_fun_list <- c(mean, max, median)
my_vec <- list(1:100)

sapply(sum_fun_list, do.call, my_vec)



