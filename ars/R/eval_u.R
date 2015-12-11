#' eval_u
#'
#' Evaluate the upper bound value
#'
#' @param x.temp points that want to evalue upper bound
#' @param u list contains coefficients and breaks
#' @return upper bound value
#' @author Chao Mao, Xian Shi, Chih-Hui Wang, Luyun Zhao
#' @export
#' @examples
#'

evalu <- function(x.temp, u){
  # Check if x.temp is out of bounds.
  # if (x.temp < head(u$breaks,n=1) || x.temp > tail(u$breaks,n=1)){
  #   stop("x.temp is out of bound.")
  # }

  ind <- sum(u$breaks <= x.temp)
  u.val <- u$parameter[ind,1] * x.temp + u$parameter[ind,2]
  return(as.numeric(u.val))
}
