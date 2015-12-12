#' samplex
#'
#' Sample the x value from S(x)
#'
#' @param u the output from \code{genu} function. A list which first element is the intercept and slope of the tangent lines of log density function and second element is the intersection of these tangent lines.
#' @param y numbers between 0 and 1. To decide where the point should sample from.
#' @return a vector contains sample that pass the squeezing test.
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
#' #random number from uniform(0, 1)
#' y <- runif(1)
#' samplex(u, y)

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

