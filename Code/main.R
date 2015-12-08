## helper functions
#generate sample x.temp
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

eval_u <- function(x.temp,u){
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
  return all
}

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

## main function
ARS <- function(n,func,start=Inf,end=Inf,fun_deriv=NULL){ #optional: derivative
  ## the func should be a function has the form e.g.func(x){return(x^2)}
  #check func is concave
  #normalize?
  
  #get derivative function by Central difference
  if(is.null(fun_deriv)){
    fun_deriv <- function(x){
      h <- 1e-8
      (func(x + h) - func(x - h))/(2*h)
    }
  }
  Tk = intTk(func)  #sorted?
  sample=numeric(n) #the resulting sample x*'s
  size=1
  uk = genu(Tk,f)
  while (size < n){
    x.temp = sampleXstar(uk)
    u.x = eval_u(x.temp,uk)
    l.x = eval_l(Tk,x.temp,func)
    w=runif(0,1)
    if(w <= exp(l.x-u.x)){
      sample[size] = x.temp
      size = size+1
    }else if(w <= exp(func(x.temp))-u.x){
      sample[size] = x.temp
      size = size+1
      Tk = sort(c(Tk,x.temp))
      uk = genu(Tk,f)
      ##generate h.x and h.dev for checking concavity
      h.x = sapply(Tk, func)
      h.dev = sapply(Tk, fun_deriv)
      #check concavity
    }
  }
  return(sample)
}
