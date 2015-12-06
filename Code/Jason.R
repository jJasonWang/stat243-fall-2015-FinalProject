#Function generate z
zfun <- function(x, fun, fun_deriv){
  (fun(x[-1]) - fun(x[-k]) - x[-1]*fun_deriv(x[-1]) + x[-k]*fun_deriv(x[-k]))/(fun_deriv(x[-k]) - fun_deriv(x[-1]))
}

#Function generate u
u <- function(x1, x, z, fun, fun_deriv){
  #Construct upper and lower bound
  z <- c(-Inf, z, Inf)
  
  #Grouping
  group <- cut(x1, breaks=z, labels=1:length(x))
  #Check which group the x1 locate
  xj <- x[as.numeric(group)]
  
  #Compute value, slope and intercept
  value <- fun(xj) + (x1 - xj)*fun_deriv(xj)
  a <- fun_deriv(x1)
  b <- fun(x1) - a*x1
  
  out <- list(parameter=cbind(a, b), u=value)
  out
}

l <- function(x1, x, fun){
  #Grouping
  group <- cut(x1, breaks=x, labels=1:(length(x) - 1))
  #Check which group the x1 locate
  xj <- x[as.numeric(group)]
  xjplus <- x[as.numeric(group) + 1]
  
  #Compute value
  all <- ((xjplus - x1)*fun(xj) + (x1 - xj)*fun(xjplus))/(xjplus - xj)
  all[is.na(all)] <- -Inf
  
  all
}