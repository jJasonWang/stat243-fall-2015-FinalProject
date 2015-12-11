#' genrate_u
#'
#' Generate coefficients and breaks
#' @param x initial points
#' @param hfun.x log function of initial points
#' @param hfun_deriv.x derivative of log function of initial points
#' @param start lower bound
#' @param end upper bound
#' @author Chao Mao, Xian Shi, Chih-Hui Wang, Luyun Zhao
#' @return list contains coefficients and breaks
#' @export
#' @examples
#'

generate_u <- function(x, hfun.x, hfun_deriv.x, start, end){
  k <- length(x)
  z <- (hfun.x[-1] - hfun.x[-k] - x[-1]*hfun_deriv.x[-1] + x[-k]*hfun_deriv.x[-k])/(hfun_deriv.x[-k] - hfun_deriv.x[-1])

  #Construct upper and lower bound
  z <- c(start, z, end)

  #Compute slope and intercept
  a <- hfun_deriv.x
  b <- hfun.x - a*x

  out <- list(parameter=cbind(a, b), breaks=z)
  out
}
