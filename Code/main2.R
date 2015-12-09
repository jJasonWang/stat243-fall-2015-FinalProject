#Function generate coefficient and breaks
InitTk <- function(f,a = -Inf,b = Inf){
  # InitTk: generate the initial Tk set before sampling.
  # Input:
  #   f, sampling function
  #   a, lower bound
  #   b, upper bound
  #   k, length of Tk
  # Output:
  #   Tk, a set of x values
  # TODO: check the existance of multiple minima? equivalent to log-concave check.
  # TODO: log(0) situation
  
  # Construct the minimization function in order to find Mode.
  mlogf <- function(x){
    y <- f(x)
    if (y == 0){
      stop("Probability density function f(x) of certain x values within the given bounds are zero.
           It can be caused if the given probability density function approximates very small values as 0.
           Please narrow the bound input or contact the corresponding authors of the input function.")
    }
    return(-log(y))
    }
  
  # Calculate Tk
  if ((a == -Inf) && (b == Inf)) {
    x.init <- 0
    opt.res <- optim(x.init, mlogf, method="BFGS")
    Tk <- c(opt.res$par - 1, opt.res$par + 1)
  }  else if ((a == -Inf) && (b != Inf)) {
    x.init <- b - abs(b)/2
    opt.res <- optim(x.init, mlogf, method="BFGS")
    Tk <- c(opt.res$par - 1, (opt.res$par + b)/2)
  }  else if ((a != -Inf) && (b == Inf)) {
    x.init <- a + abs(a)/2
    opt.res <- optim(x.init, mlogf, method="BFGS")
    Tk <- c((opt.res$par + a)/2, opt.res$par + 1)
  }  else {
    x.init <- (a + b)/2
    opt.res <- optim(x.init, mlogf, method="BFGS")
    Tk <- c((opt.res$par + a)/2, (opt.res$par + b)/2)
  }
  return(Tk)
  }

generate_u <- function(x, fun, fun_deriv){
  k <- length(x)
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

sample_x <- function(mu, y){
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

eval_u <- function(x.temp, u){
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

eval_l <- function(x1, x, fun){
  #Grouping
  group <- cut(x1, breaks=x, labels=1:(length(x) - 1))
  #Check which group the x1 locate
  xj <- x[as.numeric(group)]
  xjplus <- x[as.numeric(group) + 1]
  
  #Compute value
  all <- ((xjplus - x1)*fun(xj) + (x1 - xj)*fun(xjplus))/(xjplus - xj)
  all[is.na(all)] <- -Inf
  return(all)
}

ARS <- function(n, func, start=Inf, end=Inf, fun_deriv=NULL){ 
  #optional: derivative
  ## the func should be a function has the form e.g.func(x){return(x^2)}
  #check func is concave
  #normalize?
  fun <- function(x){
    log(func(x))
  }
  #get derivative function by Central difference
  if(is.null(fun_deriv)){
    h <- 1e-8
    fun_deriv <- function(x){
      (fun(x + h) - fun(x - h))/(2*h)
    }
  }
  Tk <- InitTk(func)  #sorted?
  result <- numeric(n) #the resulting sample x*'s
  size <- 1
  uk <- generate_u(Tk, fun, fun_deriv)
  while (size < n){
    y <- runif(1)
    x.temp <- sample_x(uk, y)
    u.x <- eval_u(x.temp, uk)
    l.x <- eval_l(x.temp, Tk, fun)
    w=runif(1)
    if(w <= exp(l.x - u.x)){
      result[size] <- x.temp
      size <- size + 1
    }else{
      if(w <= exp(func(x.temp)) - u.x){
        sample[size] <- x.temp
        size <- size + 1
      }
      Tk <- sort(c(Tk, x.temp))
      uk <- generate_u(Tk, fun, fun_deriv)
      ##generate h.x and h.dev for checking concavity
      h.x <- sapply(Tk, func)
      h.dev <- sapply(Tk, fun_deriv)
      #check concavity
    }
  }
  return(sample)
}

ARS(n=10, dnorm)