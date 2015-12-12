#' initTk
#'
#' Generate the initial value for Adaptive rejection sampling
#'
#' @param hfun log of the density function.
#' @param hfun_deriv the derivatives of log density function.
#' @param start lower bound of the density function.
#' @param end upper bound of the density function.
#' @return a k by 3 matrix which its first column are initial points, second column are log of the density function and third column are the derivatives of log density function. k is the total number of initial points.
#' @author Chao Mao, Xian Shi, Chih-Hui Wang, Luyun Zhao
#' @export
#' @examples
#'
#' #Initial value for standard normal distribution
#' set.seed(0)
#' hfun <- function(x) dnorm(x, log=TRUE)
#' h <- 1e-8
#' hfun_deriv <- function(x) (hfun(x + h) - hfun(x - h))/(2*h)
#' InitTk(hfun, hfun_deriv, -Inf, Inf)

initTk <- function(hfun, hfun_deriv, start, end){
  # Construct c(x, hfun, hfun_deriv)
  xhfunp <- function(x){
    return(c(x, hfun(x), hfun_deriv(x)))
  }

  # Iteration parameters
  m <- 1
  m.max <- 50

  # Four different scenario regarding x intervals are considered:
  # 1 - [-Inf, Inf]
  # 2 - [-Inf, b]
  # 3 - [a, Inf]
  # 4 - [a, b]
  if ((start == -Inf) && (end == Inf)) {
    while (1){
      mat.temp <- xhfunp(rnorm(1,sd=10))
      if ((mat.temp[2] != -Inf) && (mat.temp[3] != 0) && (mat.temp[3] != -Inf)){
        break
      }
    }
    mat.temp.pre <- mat.temp
    if (mat.temp[3] < 0){
      mat <- mat.temp
      while (mat.temp[3] <= 0 && m < m.max) {
        mat.temp <- xhfunp(mat.temp[1]-(2^m)*runif(1))
        mat <- rbind(mat, mat.temp)
        mat.temp.pre <- mat.temp
        m <- m + 1
      }
    } else if (mat.temp[3] > 0) {
      mat <- mat.temp
      while (mat.temp[3] >= 0 && m < m.max) {
        mat.temp <- xhfunp(mat.temp[1]+(2^m)*runif(1))
        mat <- rbind(mat, mat.temp)
        mat.temp.pre <- mat.temp
        m <- m + 1
      }
    }
  }  else if ((start == -Inf) && (end != Inf)) {
    while (1){
      mat.temp <- xhfunp(end - rexp(1, rate=0.1))
      if ((mat.temp[2] != -Inf) && (mat.temp[3] != 0) && (mat.temp[3] != -Inf)){
        break
      }
    }
    mat <- mat.temp
    while (1){
      mat.temp <- xhfunp(end - rexp(1, rate=0.1))
      if ((mat.temp[2] != -Inf) && (mat.temp[3] != 0) && (mat.temp[3] != -Inf)){
        mat <- rbind(mat, mat.temp)
        break
      }
    }
    mat.temp.pre <- mat.temp
    while (mat.temp[3] <= 0 && m < m.max) {
      mat.temp <- xhfunp(mat.temp[1] - (2^m)*runif(1))
      mat <- rbind(mat, mat.temp)
      mat.temp.pre <- mat.temp
      m <- m + 1
    }
  } else if ((start != -Inf) && (end == Inf)) {
    while (1){
      mat.temp <- xhfunp(start + rexp(1, rate=0.01))
      if ((mat.temp[2] != -Inf) && (mat.temp[3] != 0) && (mat.temp[3] != -Inf)){
        break
      }
    }
    mat <- mat.temp
    while (1){
      mat.temp <- xhfunp(start + rexp(1, rate=0.01))
      if ((mat.temp[2] != -Inf) && (mat.temp[3] != 0) && (mat.temp[3] != -Inf)){
        mat <- rbind(mat, mat.temp)
        break
      }
    }
    mat.temp.pre <- mat.temp
    while (mat.temp[3] >= 0 && m < m.max) {
      mat.temp <- xhfunp(mat.temp[1] + (2^m)*runif(1))
      mat <- rbind(mat, mat.temp)
      mat.temp.pre <- mat.temp
      m <- m + 1
    }
  }  else {
    a <- start + (end-start)/3
    b <- end - (end-start)/3
    mat <- rbind(xhfunp(a), xhfunp(b))
  }
  if (m == m.max){
    stop("Failed to find initial abscissae!
         1, The input function might have extremely large standard derivation.
         Reduce your standard derivation and try agian if applicable!
         2, The input function might be a modified exponential distribution.
         Use R built-in function dexp to sample if applicable!")
  }
  mat <- mat[order(mat[, 1]), ]
  return(mat)
}
