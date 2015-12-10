# Please run the function code "main.R" first.

# Standard R built-in PDF
dnorm # [-Inf, Inf]
dlogis # [-Inf, Inf]

# Test function 1 
# f(x) = exp(-x^2) 
# [-Inf, Inf]
tf1 <- function(x){
  return (exp(-x^2))
}

# Test function 2 - Modified Laplace distribution
# f(x) = exp(-|x^3|)/2
# [-Inf, Inf]
tf2 <- function(x){
  return (exp(-abs(x^3))/2)
}

# Test function 3 - Beta distribution (alpha - 2, beta - 5)
# f(x) = x*(1-x)^4
# [0, 1]
tf3 <- function(x){
  return (x*(1-x)^4)
}