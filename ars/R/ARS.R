#' ars
#'
#' Adaptive rejection sampling
#'
#' @param n number of sample size.
#' @param func the density function of interest, not need to be normalized.
#' @param start lower bound. (optional)
#' @param end upper bound. (optional)
#' @param hfun_deriv the derivative of the density function. (optional)
#' @param ... further arguments passed to or from other methods.
#' @return a vector of sample from the density function with length n
#' @author Chao Mao, Xian Shi, Chih-Hui Wang, Luyun Zhao
#' @export
#' @examples
#'
#' #Sample from normal distribution
#' n <- 100
#' ARS(n, dnorm)

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
    y <- runif(1)
    x.temp <- samplex(u, y)

    # Compute upper and lower value
    u.x <- evalu(x.temp, u)
    l.x <- evall(x.temp, mat[, 1], mat[, 2])

    # Uniform random number to decide wether accept or reject
    w = runif(1)
    if(w <= exp(l.x - u.x)){
      # Accept
      result[size] <- x.temp
      size <- size + 1
    }else{
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
  return(result)
}
