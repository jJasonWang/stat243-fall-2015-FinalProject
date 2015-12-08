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

## Test examples:
res <- InitTk(dnorm)
res
res <- InitTk(dnorm,-Inf,2)
res
res <- InitTk(dnorm,-2,Inf)
res
res <- InitTk(dnorm,-10,6)
res
res <- InitTk(dnorm,-10,100)