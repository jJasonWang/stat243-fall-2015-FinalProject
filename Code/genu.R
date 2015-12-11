#' genu
#'
#' Generate coefficients and breaks
#' @param mat A k by 3 matrix. First column is initial value. Second column is the log function value of initial value. Thrid column is the derivative of the log function of initial value.
#' @param start lower bound
#' @param end upper bound
#' @author Chao Mao, Xian Shi, Chih-Hui Wang, Luyun Zhao
#' @return list contains coefficients and breaks
#' @export
#' @examples
#'

genu <- function(mat, start, end){
  # Calculate z vector
  Tk <- mat[, 1]
  hfun.x <- mat[, 2]
  hfun_deriv.x <- mat[, 3]
  k <- length(Tk)

  z <- (hfun.x[-1] - hfun.x[-k] - Tk[-1] * hfun_deriv.x[-1] + Tk[-k] * hfun_deriv.x[-k])/(hfun_deriv.x[-k] - hfun_deriv.x[-1])
  if (sum(hfun_deriv.x[-k] - hfun_deriv.x[-1] == 0) > 0){
    stop("Failed to calculate z values as two adjacent points have identical derivatives! It might result from extremely large standard derivation. Reduce your standard derivation and try agian if applicable!")
  }

  # Construct upper and lower bound
  z <- c(start, z, end)

  # Compute slope and intercept
  a <- hfun_deriv.x
  b <- hfun.x - a * Tk

  res <- list(parameter=cbind(a, b), breaks = z)
  return(res)
}
