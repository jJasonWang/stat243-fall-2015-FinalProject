#' sample_x
#'
#' Sample the x value from S(x)
#'
#' @param u list contains coefficients and breaks
#' @param y uniform random number
#' @return x value
#' @author Chao Mao, Xian Shi, Chih-Hui Wang, Luyun Zhao
#' @export
#' @examples
#'


samplex <- function(u, y){

  # Inverse CDF method is used.
  # Calculate the list of areas
  z <- u$breaks
  area <- rep(0, length(z)-1)
  for (i in 1:length(area)){
    # Get parameters in u functions
    a <- u$parameter[i, 1]
    b <- u$parameter[i, 2]
    # Compute area [z_i, z_{i+1}]
    area[i] <- (exp(a*z[i+1]+b)-exp(a*z[i]+b))/a
  }

  # Determine which interval y falls in
  chosen.area <- y*sum(area)
  cum.area <- c(0,cumsum(area))
  index <- findInterval(chosen.area,cum.area)
  sub.area <- chosen.area-cum.area[index]

  # Find the corresponding x values
  x.star <- rep(0, length(y))
  for (j in 1:length(index)){
    i <- index[j]
    a <- u$parameter[i, 1]
    b <- u$parameter[i, 2]
    z.star <- z[i]
    area.star <- sub.area[j]
    xx.star <- (log(a*area.star+exp(a*z.star+b))-b)/a
    x.star[j] <- xx.star
  }
  return(x.star)
}
