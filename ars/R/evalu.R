#' evalu
#'
#' Evaluate the upper bound value
#'
#' @param x.temp points that want to evalue upper bound.
#' @param u the output from \code{genu} function. A list which first element is the intercept and slope of the tangent lines of log density function and second element is the intersection of these tangent lines.
#' @return a vector. Upper bound value of the log density function for squeezing test.
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
#' mat <- cbind(Tk, hfun(Tk), hfun_deriv(Tk))
#' mat
#'
#' #Generate intercept, slope and breaks
#' u <- genu(mat, start=-Inf, end=Inf)
#'
#' #evaluate upper bound for -0.5
#' evalu(-0.5, u)

evalu <- function(x.temp, u){

  # Check if x.temp is out of bounds.
  # if (x.temp < head(u$breaks,n=1) || x.temp > tail(u$breaks,n=1)){
  #   stop("x.temp is out of bound.")
  # }

  ind <- sum(u$breaks <= x.temp)
  u.val <- u$parameter[ind,1] * x.temp + u$parameter[ind,2]
  return(as.numeric(u.val))
}
