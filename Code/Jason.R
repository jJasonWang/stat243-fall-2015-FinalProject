x <- seq(-5, 5, 0.01)

plot(x, dnorm(x, log=TRUE), type="l")

logdnorm <- function(x){
  -x^2/2 - log(2*pi)/2
}

#Derivative
logddnorm <- function(x){
  -x
}

experiment <- function(n, k, density_fun, ddensity_fun){
  #Initital value
  x <- seq(-0.5, 0.5, length.out=k)
  
  #z
  z <- (logdnorm(x[-1]) - logdnorm(x[-k]) - x[-1]*logddnorm(x[-1]) + x[-k]*logddnorm(x[-k]))/(logddnorm(x[-k]) - logddnorm(x[-1]))
  
  #Compute u
  
  #Compute s
  
  #Compute l
  
  #Sampling step for x_star
  
  #Sampling step for w
  
  #Reject or not
  
  #Updating step
  
  #Output
  
}

#Compute derivative
log_deriv <- function(x, fun, h=1){
  (log(fun(x + h)) - log(fun(x - h)))/2*h
}

#Function generate z
z <- function(x, fun, fun_deriv){
  (fun(x[-1]) - fun(x[-k]) - x[-1]*fun_deriv(x[-1]) + x[-k]*fun_deriv(x[-k]))/(fun_deriv(x[-k]) - fun_deriv(x[-1]))
}

#Function generate u
u <- function(x1, z, fun, fun_deriv){
  #Construct upper and lower bound
  z <- c(-Inf, z, Inf)
  
  #Grouping
  group <- cut(x1, breaks=z)
  #Check which group the x1 locate
  xj <- x[which(group == levels(group))]
  
  #Compute value
  logdnorm(xj) + (x1 - xj)*logddnorm(xj)
}

l <- function(x1, x, fun_deriv){
  #Grouping
  group <- cut(x1, breaks=x)
  #Check which group the x1 locate
  xj <- x[which(group == levels(group))]
  xjplus <- x[which(group == levels(group)) + 1]
  
  #Compute value
  all <- ((xjplus - x1)*logdnorm(xj) + (x1 - xj)*logdnorm(xjplus))/(xjplus - xj)
  ifelse(identical(numeric(0), all),
         -Inf,
         all)
}

height <- exp(sapply(z, u, z))
area <- rep(0, length(height) - 1)
for(i in 1:(length(height) - 1)){
  area[i] <- (height[i] + height[i + 1])*(z[i + 1] - z[i])/2
}
