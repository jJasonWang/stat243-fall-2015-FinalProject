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

eval_l <- function(x1, x, hfun){
  k <- length(x)

  #Grouping
  group <- cut(x1, breaks=x, labels=1:(k - 1))
  #Check which group the x1 locate
  xj <- x[as.numeric(group)]
  xjplus <- x[as.numeric(group) + 1]

  #Compute value
  all <- ((xjplus - x1)*hfun(xj) + (x1 - xj)*hfun(xjplus))/(xjplus - xj)
  all[is.na(all)] <- -Inf
  return(all)
}
