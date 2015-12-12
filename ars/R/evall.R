#' evall
#'
#' Evaluate the lower bound value
#'
#' @param x1 points that want to evalue lower bound.
#' @param Tk abscissae.
#' @param hfun.x a vector which includes all values of the log density function at those points in Tk.
#' @return a vector. Lower bound value of the log density function for squeezing test.
#' @author Chao Mao, Xian Shi, Chih-Hui Wang, Luyun Zhao
#' @export
#' @examples
#'
#' #standard normal distribution
#' hfun <- function(x) dnorm(x, log=TRUE)
#' h <- 1e-8
#' hfun_deriv <- function(x) (hfun(x + h) - hfun(x - h))/(2*h)
#'
#' #Construct mat
#' Tk <- c(-1, 1)
#' hfun.x <- hfun(Tk)
#'
#' #evaluate lower bound for -0.5
#' evall(-0.5, Tk, hfun.x)

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
