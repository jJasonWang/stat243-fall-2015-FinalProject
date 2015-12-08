#It will be the function which user gives us
fun <- function(x){
  log(dnorm(x))
}
#Central difference
h <- 1e-8
fun_deriv <- function(x){
  (fun(x + h) - fun(x - h))/(2*h)
}

fun_deriv(1:5)
## [1] -1 -2 -3 -4 -5

#5 abscissaes
k <- 5
x <- seq(-0.5, 0.5, length.out=k)

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

generate_u(x, fun, fun_deriv)

## $parameter
##          a          b
## [1,]  0.50 -0.7939385
## [2,]  0.25 -0.8876885
## [3,]  0.00 -0.9189385
## [4,] -0.25 -0.8876885
## [5,] -0.50 -0.7939385## 

## $breaks
## [1]   -Inf -0.375 -0.125  0.125  0.375    Inf

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

#x1 is the x-value which you want to evaluate l
x1 <- seq(-1, 1, by=0.1)
l(x1, x, fun)
## [1]       -Inf       -Inf       -Inf       -Inf       -Inf
## [6]       -Inf -1.0064385 -0.9689385 -0.9439385 -0.9314385
## [11] -0.9189385 -0.9314385 -0.9439385 -0.9689385 -1.0064385
## [16] -1.0439385       -Inf       -Inf       -Inf       -Inf
## [21]       -Inf