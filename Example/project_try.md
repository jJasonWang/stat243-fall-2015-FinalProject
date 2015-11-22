# Project_example
Chih-Hui(Jason) Wang  
November 21, 2015  



# Standard normal distribution as example


```r
#Standard normal(log scale)
mydnorm <- function(x){
  -x^2/2 - log(2*pi)/2
}

#Derivative
myddnorm <- function(x){
  -x
}

x <- seq(-5, 5, 0.01)
plot(x, dnorm(x, log=TRUE), type="l")
```

<img src="project_try_files/figure-html/unnamed-chunk-1-1.png" title="" alt="" style="display: block; margin: auto;" />

# Main function pseudo code

```r
experiment <- function(n, k, density_fun, ddensity_fun){
  #Initital value
  x <- seq(-0.5, 0.5, length.out=k)
  
  #z
  z <- (mydnorm(x[-1]) - mydnorm(x[-k]) - x[-1]*myddnorm(x[-1]) + x[-k]*myddnorm(x[-k]))/(myddnorm(x[-k]) - myddnorm(x[-1]))
  
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
u <- function(x1, z){
  z <- c(-Inf, z, Inf)
  group <- cut(x1, breaks=z)
  xj <- x[which(group == levels(group))]
  mydnorm(xj) + (x1 - xj)*myddnorm(xj)
}

l <- function(x1, x){
  xlower <- x[which(x >= x1)[1] - 1]
  xupper <- x[which(x >= x1)[1]]
  ifelse(all(x1 > x) | all(x1 < x), -Inf, ((xupper - x1)*mydnorm(xlower) + (x1 - xlower)*mydnorm(xupper))/(xupper - xlower))
}
```

# Visualization


```r
#Initial point -0.5, 0.5
#5 abscissaes
k <- 5
x <- seq(-0.5, 0.5, length.out=k)
#Compute z
z <- (mydnorm(x[-1]) - mydnorm(x[-k]) - x[-1]*myddnorm(x[-1]) + x[-k]*myddnorm(x[-k]))/(myddnorm(x[-k]) - myddnorm(x[-1]))
```


```r
#function to plot tangent line and intersection
plot.z <- function(x){
  slope <- myddnorm(x)
  intercept <- -(slope*x - mydnorm(x))
  
  for(i in 1:length(slope)){
    abline(a=intercept[i], b=slope[i], lty=3, col="red")
  }

  z <- (mydnorm(x[-1]) - mydnorm(x[-k]) - x[-1]*myddnorm(x[-1]) + x[-k]*myddnorm(x[-k]))/(myddnorm(x[-k]) - myddnorm(x[-1]))
  
  abline(v=z, col="blue", lty=4)
}
```

### All


```r
#Density
plot(seq(-1, 1, by=0.01), mydnorm(seq(-1, 1, by=0.01)), type="l")

#Abscissae
abline(v=x, lty=2)

#
plot.z(x)

#Add upper and lower
x1 <- seq(-1, 1, by=0.01)
lines(x1, sapply(x1, u, z), col="green")
lines(x1, sapply(x1, l, x), col="orange")
```

<img src="project_try_files/figure-html/unnamed-chunk-6-1.png" title="" alt="" style="display: block; margin: auto;" />

### Zoom in : intersection, upper and lower


```r
#Density
plot(seq(-1, 1, by=0.01), mydnorm(seq(-1, 1, by=0.01)), type="l", ylim=c(-1.1, -0.9))

#Abscissae
abline(v=x, lty=2)
#Intersect
abline(v=z, lty=2, col="blue")

#Add upper and lower
x1 <- seq(-1, 1, by=0.01)
lines(x1, sapply(x1, u, z), col="green")
lines(x1, sapply(x1, l, x), col="orange")

legend("topright", lty=c(2, 1, 1), col=c("blue","green","orange"), legend=c("intersection", "upper", "lower"))
```

<img src="project_try_files/figure-html/unnamed-chunk-7-1.png" title="" alt="" style="display: block; margin: auto;" />

