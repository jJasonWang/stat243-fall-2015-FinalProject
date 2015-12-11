#' sample_x
#'
#' Sample the x value from S(x)
#'
#' @param mu list contains coefficients and breaks
#' @param y uniform random number
#' @return x value
#' @author Chao Mao, Xian Shi, Chih-Hui Wang, Luyun Zhao
#' @export
#' @examples
#'


sample_x <- function(mu, y){
  ## calculate the list of areas ##
  z <- mu$breaks
  area <- rep(0, length(z)-1)
  for (i in 1:length(area)){
    # get parameters
    a <- mu$parameter[i, 1]
    b <- mu$parameter[i, 2]
    # compute area
    area[i] <- (exp(a*z[i+1]+b)-exp(a*z[i]+b))/a
  }

  ## determine which interval y falls in ##
  chosen_area <- y*sum(area)
  cum_area <- c(0,cumsum(area))
  index <- findInterval(chosen_area,cum_area)
  sub_area <- chosen_area-cum_area[index]

  ## find the corresponding x values ##
  x_star <- rep(0,length(y))
  for (j in 1:length(index)){
    i <- index[j]
    a <- mu$parameter[i, 1]
    b <- mu$parameter[i, 2]
    z_star <- z[i]
    area_star <- sub_area[j]
    xx_star <- (log(a*area_star+exp(a*z_star+b))-b)/a
    x_star[j] <- xx_star
  }
  return(x_star)
}
