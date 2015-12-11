# Input:
#    hfun: h(x)
#    hfun_deriv: h'(x)
#     start: lower bound
#     end: upper bound
# Output:
#		 matrix (Tk, h(Tk), h'(Tk))

initTk <- function(hfun, hfun_deriv, start, end){
  
  # Construct c(x, hfun, hfun_deriv)
  xhfunp <- function(x){
    return(c(x, hfun(x), hfun_deriv(x)))
  }
  
  # Iteration parameters
  m <- 1
  m.max <- 50
  
  # Four different scenario regarding x intervals are considered:
  # 1 - [-Inf, Inf]
  # 2 - [-Inf, b]
  # 3 - [a, Inf]
  # 4 - [a, b]
  if ((start == -Inf) && (end == Inf)) {
    while (1){
      mat.temp <- xhfunp(rnorm(1,sd=10))
      if ((mat.temp[2] != -Inf) && (mat.temp[3] != 0) && (mat.temp[3] != -Inf)){
        break
      }
    }
    mat.temp.pre <- mat.temp
    if (mat.temp[3] < 0){
      mat <- mat.temp
      while (mat.temp[3] <= 0 && m < m.max) {
        mat.temp <- xhfunp(mat.temp[1]-(2^m)*runif(1))
        mat <- rbind(mat, mat.temp)
        mat.temp.pre <- mat.temp
        m <- m + 1
      }
    } else if (mat.temp[3] > 0) {
      mat <- mat.temp
      while (mat.temp[3] >= 0 && m < m.max) {
        mat.temp <- xhfunp(mat.temp[1]+(2^m)*runif(1))
        mat <- rbind(mat, mat.temp)
        mat.temp.pre <- mat.temp
        m <- m + 1
      }          
    }    
  }  else if ((start == -Inf) && (end != Inf)) {
    while (1){
      mat.temp <- xhfunp(end - rexp(1, rate=0.1))
      if ((mat.temp[2] != -Inf) && (mat.temp[3] != 0) && (mat.temp[3] != -Inf)){
        break
      }
    }
    mat <- mat.temp
    while (1){
      mat.temp <- xhfunp(end - rexp(1, rate=0.1))
      if ((mat.temp[2] != -Inf) && (mat.temp[3] != 0) && (mat.temp[3] != -Inf)){
        mat <- rbind(mat, mat.temp)
        break
      }
    }
    mat.temp.pre <- mat.temp
    while (mat.temp[3] <= 0 && m < m.max) {
      mat.temp <- xhfunp(mat.temp[1] - (2^m)*runif(1))
      mat <- rbind(mat, mat.temp)
      mat.temp.pre <- mat.temp
      m <- m + 1
    }
  } else if ((start != -Inf) && (end == Inf)) {
    while (1){
      mat.temp <- xhfunp(start + rexp(1, rate=0.01))
      if ((mat.temp[2] != -Inf) && (mat.temp[3] != 0) && (mat.temp[3] != -Inf)){
        break
      }
    }
    mat <- mat.temp
    while (1){
      mat.temp <- xhfunp(start + rexp(1, rate=0.01))
      if ((mat.temp[2] != -Inf) && (mat.temp[3] != 0) && (mat.temp[3] != -Inf)){
        mat <- rbind(mat, mat.temp)
        break
      }
    }
    mat.temp.pre <- mat.temp
    while (mat.temp[3] >= 0 && m < m.max) {
      mat.temp <- xhfunp(mat.temp[1] + (2^m)*runif(1))
      mat <- rbind(mat, mat.temp)
      mat.temp.pre <- mat.temp
      m <- m + 1
    }
  }  else {
    a <- start + (end-start)/3
    b <- end - (end-start)/3
    mat <- rbind(xhfunp(a), xhfunp(b))
  }
  if (m == m.max){
    stop("Failed to find initial abscissae!
         1, The input function might have extremely large standard derivation.
         Reduce your standard derivation and try agian if applicable!
         2, The input function might be a modified exponential distribution.
         Use R built-in function dexp to sample if applicable!")
  }
  mat <- mat[order(mat[, 1]), ]
  return(mat)
  }

# genu： generate the parameters and breaking points used in the u(x) function
# Input:
#    Tk: a vector of x in Tk
#    hfun.x: values of h(x)
#		 hfun_deriv.x: deravative values of h(x) 
#		 start: lower bound
#		 end: upper bound
# Output:
#		 parameters and breaking points used in the u(x) function

genu <- function(mat, start, end){
  
  # Calculate z vector
  Tk <- mat[, 1]
  hfun.x <- mat[, 2]
  hfun_deriv.x <- mat[, 3]
  k <- length(Tk)
  
  z <- (hfun.x[-1] - hfun.x[-k] - Tk[-1] * hfun_deriv.x[-1] + Tk[-k] * hfun_deriv.x[-k])/(hfun_deriv.x[-k] - hfun_deriv.x[-1])
  if (sum(hfun_deriv.x[-k] - hfun_deriv.x[-1] == 0) > 0){
    stop("Failed to calculate z values as two adjacent points have identical derivatives!
         1, The input function might have extremely large standard derivation.
         Reduce your standard derivation and try agian if applicable!
         2, The input function might be a modified exponential distribution.
         Use R built-in function dexp to sample if applicable!")
  }
  
  # Construct upper and lower bound
  z <- c(start, z, end)
  
  # Compute slope and intercept
  a <- hfun_deriv.x
  b <- hfun.x - a * Tk
  
  res <- list(parameter=cbind(a, b), breaks = z)
  return(res)
  }

# samplex： generate a value of x* from s(x)
# Input:
#    u: the output of the generate_u() function, i.e. a list of the parameters and breaking points
#    y: a random value from 0 to 1, can also be a vector
# Output:
#		 a vector of sampled x* with the same length of y

samplex <- function(u, y){
  
  # Inverse CDF method is used.
  # Calculate the list of areas
  z <- u$breaks
  area <- rep(0, length(z)-1)
  a <- u$parameter[, 1]
  b <- u$parameter[, 2]
  area <- (exp(a*z[-1]+b)-exp(a*z[-length(z)]+b))/a
  
  # Determine which interval y falls in
  chosen.area <- y*sum(area)
  cum.area <- c(0,cumsum(area))
  index <- findInterval(chosen.area,cum.area)
  area.star <- chosen.area-cum.area[index]
  
  # Find the corresponding x values
  x.star <- rep(0, length(y))
  z.star <- z[index]
  a.idx <- a[index]
  b.idx <- b[index]
  x.star <- (log(a.idx * area.star + exp( a.idx * z.star + b.idx))-b.idx)/a.idx
  return(x.star)
}

# evalu： get the value of the function u(x) at x.temp
# Input:
#    x.temp: the point at which we want to evaluate u(x)
#    u: the output of the generate_u() function, i.e. a list of the parameters and breaking points
# Output:
#		 a value of u(x)

evalu <- function(x.temp, u){
  
  # Check if x.temp is out of bounds.
  # if (x.temp < head(u$breaks,n=1) || x.temp > tail(u$breaks,n=1)){
  #   stop("x.temp is out of bound.")
  # }
  
  ind <- sum(u$breaks <= x.temp)
  u.val <- u$parameter[ind,1] * x.temp + u$parameter[ind,2]
  return(as.numeric(u.val))
}

# evall： get the value of the function l(x) at x1
# Input:
#    x1: the point at which we want to evaluate l(x)
#    x: the breaking points of l(x), i.e. the values of x in Tk
#		 fun: h(x)
# Output:
#		 a value of l(x1)

evall <- function(x1, Tk, hfun.x){
  
  k <- length(Tk)
  # Grouping
  group <- cut(x1, breaks = Tk, labels=1:(k-1))
  # Check which group the x1 locate
  
  xj.index <- as.numeric(group)
  xj <- Tk[xj.index]
  xjplus <- Tk[xj.index + 1]
  
  # Compute value
  res <- ((xjplus - x1)*hfun.x[xj.index] + (x1 - xj)*hfun.x[xj.index+1])/(xjplus - xj)
  res[is.na(res)] <- -Inf
  return(res)
}


# ars： main function
# Input:
#    n: the number of points we want to sample
#    func: the density function of interest, not need to be normalized
#		 start: lower bound (optional)
# 	 end: upper bound (optional)
#    fun_deriv: the deravative of the density function (optional)
# Output:
#		 a vector of sampled x of length n

ars <- function(n, func, start=-Inf, end=Inf, hfun_deriv=NULL, ...){
  
  # Construct h(x).
  # Check whether users provide "log" keywords;
  # It most likely happens when the input function is R built-in functions
  # with log = TRUE options.
  if(any(names(formals(func)) == "log")){
    hfun <- function(x){
      func(x, ..., log=TRUE)
    }
  }else{
    hfun <- function(x){
      log(func(x, ...))
    }
  }
  
  # Construct h'(x) by central difference.
  # Check whether users provide deriv function.
  if(is.null(hfun_deriv)){
    h <- 1e-8
    hfun_deriv <- function(x){
      (hfun(x + h) - hfun(x - h))/(2*h)
    }
  }
  
  # Parameters initialization
  result <- rep(0, n)  # Array of result
  size <- 1 # Count
  mat <- initTk(hfun, hfun_deriv, start, end) # Initial abscissae with corresponding h(x) and h'(x)
  u <- genu(mat, start, end) # Generate u(x) corresponding to initial abscissae
  
  while (size <= n){
    # Sample from s(x)
    y <- runif(min(size, n-size+1))
    x.temp.mat <- samplex(u, y)
    for (i in 1:length(x.temp.mat)){
      x.temp <- x.temp.mat[i]
      # Compute upper and lower value
      u.x <- evalu(x.temp, u)
      l.x <- evall(x.temp, mat[, 1], mat[, 2])
      
      # Uniform random number to decide wether accept or reject
      w = runif(1)
      if(w <= exp(l.x - u.x)){
        # Accept
        result[size] <- x.temp
        size <- size + 1
      } else {
        hfun.temp <- hfun(x.temp)
        hfun_deriv.temp <- hfun_deriv(x.temp)
        if(w <= exp(hfun.temp - u.x)){
          # Accept
          result[size] <- x.temp
          size <- size + 1
        }
        # If the density of the point is too small, sample again
        
        if((is.finite(hfun.temp)) && (is.finite(hfun_deriv.temp)) 
           && (hfun_deriv.temp != 0) && (sum(mat[, 3] == hfun_deriv.temp) == 0)){
          # Reject, Update point
          mat <- rbind(mat, c(x.temp, hfun.temp, hfun_deriv.temp))
          mat <- mat[order(mat[, 1]), ]
          u <- genu(mat, start, end)
          
          # Numerical check for concavity
          h.dev <- mat[, 3]
          if (all(diff(h.dev)<=0)==FALSE){
            stop("The input function is NOT Log-concave!")
          }
        }
      }
    }
  }
  return(result)
}