#' eval_l
#'
#' Evaluate the lower bound value
#'
#' @param x1 points that want to evalue lower bound
#' @param x abscissae
#' @param hfun log density function of interest, not need to be normalized.
#' @return lower bound value
#' @author Chao Mao, Xian Shi, Chih-Hui Wang, Luyun Zhao
#' @export
#' @examples
#'

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
