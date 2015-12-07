# Project_example
Chih-Hui(Jason) Wang  
November 21, 2015  



# Standard normal distribution as example


```r
#Standard normal(log scale)
logdnorm <- function(x){
  -x^2/2 - log(2*pi)/2
}

#Derivative
logddnorm <- function(x){
  -x
}
```

# Main function pseudo code

```r
experiment <- function(n, k, density_fun, ddensity_fun){
  #Initital value

  #z
  
  #Compute u
  
  #Compute s
  
  #Compute l
  
  #Sampling step for x_star
  
  #Sampling step for w
  
  #Reject or not
  
  #Updating step
  
  #Output
  
}
```

# u and l function

```r
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
```


```r
#Initial point -0.5, 0.5
#5 abscissaes
k <- 5
x <- seq(-0.5, 0.5, length.out=k)

generate_u(x, logdnorm, logddnorm)
```

```
$parameter
         a          b
[1,]  0.50 -0.7939385
[2,]  0.25 -0.8876885
[3,]  0.00 -0.9189385
[4,] -0.25 -0.8876885
[5,] -0.50 -0.7939385

$breaks
[1]   -Inf -0.375 -0.125  0.125  0.375    Inf
```

### Intersection, upper and lower



### Upper function



