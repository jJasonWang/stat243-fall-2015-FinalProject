# InitTk： generate the initial set of abscissae.
# Input:
#    hfun: h(x)
#    hfun_deriv: h'(x)
#  	 start: lower bound
#		 end: upper bound
# Output:
#		 matrix (Tk, h(Tk), h'(Tk))

InitTk <- function(hfun, hfun_deriv, start, end){
  xhfunp <- function(x){
    return(c(x, hfun(x), hfun_deriv(x)))
  }

  m <- 1
  m.max <- 500
  if ((start == -Inf) && (end == Inf)) {
    while (1){
      mat.temp <- xhfunp(rnorm(1,sd=10))
      if (mat.temp[2] != -Inf){
        break
      }
    }
    mat.temp.pre <- mat.temp
    if (mat.temp[3] < 0){
      mat <- mat.temp
      while (mat.temp[3] < 0 && m < m.max) {
        mat.temp <- xhfunp(mat.temp[1]-2^m)
        if (mat.temp[3] < mat.temp.pre[3]){
          stop("The input function is NOT Log-concave!")
        } else {
          mat <- rbind(mat, mat.temp)
          mat.temp.pre <- mat.temp
          m <- m + 1
        }
      }
    } else if (mat.temp[3] > 0) {
      mat <- mat.temp
      while (mat.temp[3] > 0 && m < m.max) {
        mat.temp <- xhfunp(mat.temp[1]+2^m)
        if (mat.temp[3] > mat.temp.pre[3]){
          stop("The input function is NOT Log-concave!")
        } else {
          mat <- rbind(mat, mat.temp)
          mat.temp.pre <- mat.temp
          m <- m + 1
        }
      }          
    }    
  }  else if ((start == -Inf) && (end != Inf)) {
    while (1){
      mat.temp <- xhfunp(end - rexp(1, rate=0.1))
      if (mat.temp[2] != -Inf){
        break
      }
    }
    mat <- mat.temp
    while (1){
      mat.temp <- xhfunp(end - rexp(1, rate=0.1))
      if (mat.temp[2] != -Inf){
        mat <- rbind(mat, mat.temp)
        break
      }
    }
    mat.temp.pre <- mat.temp
    while (mat.temp[3] < 0 && m < m.max) {
      mat.temp <- xhfunp(mat.temp[1]-2^m)
      if (mat.temp[3] < mat.temp.pre[3]){
        stop("The input function is NOT Log-concave!")
      } else {
        mat <- rbind(mat, mat.temp)
        mat.temp.pre <- mat.temp
        m <- m + 1
      }
    }
  } else if ((start != -Inf) && (end == Inf)) {
    while (1){
      mat.temp <- xhfunp(start + rexp(1, rate=0.01))
      if (mat.temp[2] != -Inf){
        break
      }
    }
    mat <- mat.temp
    while (1){
      mat.temp <- xhfunp(start + rexp(1, rate=0.01))
      if (mat.temp[2] != -Inf){
        mat <- rbind(mat, mat.temp)
        break
      }
    }
    mat.temp.pre <- mat.temp
    while (mat.temp[3] > 0 && m < m.max) {
      mat.temp <- xhfunp(mat.temp[1] + 2^m)
      if (mat.temp[3] > mat.temp.pre[3]){
        stop("The input function is NOT Log-concave!")
      } else {
        mat <- rbind(mat, mat.temp)
        mat.temp.pre <- mat.temp
        m <- m + 1
      }
    }
  }  else {
    a <- start + (end-start)/3
    b <- end - (end-start)/3
    mat <- rbind(xhfunp(a), xhfunp(b))
  }
  mat <- mat[order(mat[, 1]), ]
  return(mat)
}
