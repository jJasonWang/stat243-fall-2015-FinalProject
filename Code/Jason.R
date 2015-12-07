#Function generate coefficient and breaks
generate_u <- function(x, fun, fun_deriv){
  z <- (fun(x[-1]) - fun(x[-k]) - x[-1]*fun_deriv(x[-1]) + x[-k]*fun_deriv(x[-k]))/(fun_deriv(x[-k]) - fun_deriv(x[-1]))
  
  #Construct upper and lower bound
  z <- c(-Inf, z, Inf)
  
  #Grouping
  group <- cut(x, breaks=z, labels=1:length(x))
  #Check which group the x1 locate
  xj <- x[as.numeric(group)]
  
  #Compute value, slope and intercept
  value <- fun(xj) + (x - xj)*fun_deriv(xj)
  a <- fun_deriv(x)
  b <- fun(x) - a*x
  
  out <- list(parameter=cbind(a, b), breaks=z)
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

area <- function(z, u){
  area <- rep(0, length(z) + 1)
  #Add upper and lower bound
  z <- c(-Inf, z, Inf)
  for(i in 1:length(area)){
    #Update Coefficient
    a <- u$parameter[i, 1]
    b <- u$parameter[i, 2]
    
    #Compute area under the exponential
    area[i] <- integrate(function(x, a, b) exp(a*x + b), z[i], z[i + 1], a=a, b=b)$value
  }
  
  area
}
