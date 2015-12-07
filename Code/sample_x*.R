
### sample a vector of x* from s(x), mu gives the parameters and breaking points 
### for mu(x) and y is a vector of random generated numbers from 0 to 1
# note: length of breaking points = length of parameters+1

#example:

# mu
# $parameters  (a,b) matrix
# [,1] [,2]
# [1,]    1    3
# [2,]   -1    8
# [3,]   -2   20
# 
# $break_points
# [1] -Inf    1    2    5

# y
# y <- runif(10)
# [1] 0.40231178 0.85938826 0.73720688 0.34161100 0.03901704 0.21471790 0.27236196
# [8] 0.40032105 0.61403713 0.05187816

sample_x <- function(mu,y){
  ## calculate the list of areas ##
  z <- mu$breaks
  area <- rep(0, length(z)-1)
  for (i in 1:length(area)){
    # get parameters
    a <- mu$parameter[i, 1]
    b <- mu$parameter[i, 2]
    # compute area
    area[i] <- integrate(function(x, a, b) exp(a*x + b), z[i], z[i + 1], a=a, b=b)$value
  }
  
  ## determine which interval y falls in ##
  chosen_area <- y*sum(area)
  cum_area <- c(0,cumsum(area))
  index <- findInterval(chosen_area,cum_area)
  sub_area <- chosen_area-cum_area[index]
  
  ## find the corresponding x values ##
  x_star <- rep(0,length(y))
  for (j in 1:length(index)){
    i <- index[j]
    a <- mu$parameter[i, 1]
    b <- mu$parameter[i, 2]
    z_star <- z[i]
    area_star <- sub_area[j]
    xx_star <- (log(a*area_star+exp(a*z_star+b))-b)/a
    x_star[j] <- xx_star
  }
  return(x_star)
}

## test ##
# sample_x(mu,y)
# [1] 2.256425 2.973276 2.664646 2.208253 2.019765 2.120433 2.158428 2.254770
# [9] 2.473955 2.026484







