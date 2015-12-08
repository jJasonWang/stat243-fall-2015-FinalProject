evalu <- function(x.temp,u){
  # evalu: evaluate u(x) at temp.x
  # Input:
  #   temp.x, given x value
  #   u, u function parameters
  # Output:
  #   u.val, u value at temp.x
  
  # Check if temp.x is out of bounds.
  if (x.temp < head(u$breaks,n=1) || x.temp > tail(u$breaks,n=1)){
    stop("x.temp is out of bound.")
  }
  
  ind <- sum(u$breaks <= x.temp)
  u.val <- u$parameter[ind,1] * x.temp + u$parameter[ind,2]
  return(as.numeric(u.val))
}

## Test examples:
a <- c(2,0,-1)
b <- c(4,2,3)
z <- c(-2,-1,1,2)
u <- list(parameter=cbind(a, b), breaks=z)

u.val <- evalu(-1.5,u)
u.val
u.val <- evalu(-0.8,u)
u.val
u.val <- evalu(1.5,u)
u.val
u.val <- evalu(2.5,u)
