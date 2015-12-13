#' genu
#'
#' Generate coefficients and breaks
#' @param mat the output from the function \ code {InitTk}. A k by 3 matrix which its first column are initial points, second column are log of the density function and third column are the derivatives of log density function. k is the total number of initial points.
#' @param lower bound of the density function.
#' @param upper bound of the density function.
#' @author Chao Mao, Xian Shi, Chih-Hui Wang, Luyun Zhao
#' @return a list which first element is the intercept(b) and slope(a) of the tangent lines of log density function and second element is the intersection of these tangent lines.
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
#' genu(mat, start=-Inf, end=Inf)


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
