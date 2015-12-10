# Please run the function code first ""
setwd("~/ps-stat243/final-project/Code")

# Test function 1 - dnorm 
h <- function(x){
  log(dnorm(x))
}

h_deriv <- function(x){
  dx = 1e-8
  (h(x + dx) - h(x - dx))/(2*dx)
}

InitTk(h, h_deriv,-Inf,Inf)

InitTk(h, h_deriv,-50,Inf)


# Test function 2 - Modified Laplace distribution
# f(x) = exp(-|x^3|)/2
h <- function(x){
  return (-abs(x^3))
}

h_deriv <- function(x){
  dx = 1e-8
  (h(x + dx) - h(x - dx))/(2*dx)
}

InitTk(h,h_deriv,-Inf,25)

InitTk(h,h_deriv,-10,10)

# Test function 3 - t distribution
# Not log-concave
h <- function(x){
  log(dt(x,1))
}

h_deriv <- function(x){
  dx = 1e-8
  (h(x + dx) - h(x - dx))/(2*dx)
}

InitTk(h, h_deriv,-Inf,Inf)