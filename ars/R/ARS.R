#' ARS
#'
#' Adaptive rejection sampling
#'
#' @param n number of sample size.
#' @param fun the density function of interest, not need to be normalized.
#' @param fun_deriv the derivative of the density function. (optional)
#' @param start lower bound. (optional)
#' @param end upper bound. (optional)
#' @param ... further arguments passed to or from other methods.
#' @return a vector of sample from the density function with length n
#' @author Chao Mao, Xian Shi, Chih-Hui Wang, Luyun Zhao
#' @export
#' @examples
#'
#' #Sample from normal distribution
#' n <- 100
#' ARS(n, dnorm)

ARS <- function(n, func, start=-Inf, end=Inf, fun_deriv=NULL, ...){
  #####Need to do: check func is concave, normalize?#####

  #Check if the input function
  if(any(names(formals(func)) == "log")){
    fun <- function(x){
      func(x, ..., log=TRUE)
    }
  }else{
    fun <- function(x){
      log(func(x, ...))
    }
  }

  #Get derivative function by Central difference
  if(is.null(fun_deriv)){
    h <- 1e-8
    fun_deriv <- function(x){
      (fun(x + h) - fun(x - h))/(2*h)
    }
  }

  ######Question: sorted?#####
  #Initial Value
  mat <- InitTk(fun, fun_deriv, start, end)
  Tk <- mat[1,]

  #Set up
  result <- rep(0, n)
  size <- 1

  #Generate u
  uk <- generate_u(Tk, fun, fun_deriv, start, end)

  #Matrix used for checking concavity
  mat <- cbind(Tk, fun(Tk), fun_deriv(Tk))

  while (size <= n){
    #Sample from s(x)
    y <- runif(1)
    x.temp <- sample_x(uk, y)

    #Compute upper and lower value
    u.x <- eval_u(x.temp, uk)
    l.x <- eval_l(x.temp, Tk, fun)

    #Uniform random number to decide wether accept or reject
    w=runif(1)
    if(w <= exp(l.x - u.x)){
      #Accept
      result[size] <- x.temp
      size <- size + 1
    }else{
      if(w <= exp(fun(x.temp) - u.x)){
        #Accept
        result[size] <- x.temp
        size <- size + 1
      }
      #Reject, Update point
      mat <- rbind(mat, c(x.temp, fun(x.temp), fun_deriv(x.temp)))
      #Sort by Tk
      mat <- mat[order(mat[, 1]), ]
      Tk <- mat[, 1]

      uk <- generate_u(Tk, fun, fun_deriv, start, end)
      ##generate h.x and h.dev for checking concavity
      ######check concavity#####
      h.dev <- mat[,3]
      if (all(diff(h.dev)<=0)==FALSE){
        stop("Not log concave!")
      }
    }
  }
  return(result)
}
