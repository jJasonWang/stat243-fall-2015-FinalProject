InitTk <- function(fun, fun_deriv, start, end){
  xfunp <- function(x){
    return(c(x, fun(x), fun_deriv(x)))
  }

  m <- 1
  m.max <- 500
  if ((start == -Inf) && (end == Inf)) {
    while (1){
      mat.temp <- xfunp(dnorm(1))
      if (mat.temp[2] != -Inf){
        break
      }
    }
    mat.temp.pre <- mat.temp
    if (mat.temp[3] < 0){
      mat <- mat.temp
      while (mat.temp[3] < 0 && m < m.max) {
        mat.temp <- xfunp(mat.temp[1]-2^m)
        if (mat.temp[3] < mat.temp.pre[3]){
          stop("NOT Log-concave!!")
        } else {
          mat <- rbind(mat, mat.temp)
          mat.temp.pre <- mat.temp
          m <- m + 1
        }
      }
    } else if (mat.temp[3] > 0) {
      mat <- mat.temp
      while (mat.temp[3] > 0 && m < m.max) {
        mat.temp <- xfunp(mat.temp[1]+2^m)
        if (mat.temp[3] > mat.temp.pre[3]){
          stop("NOT Log-concave!!")
        } else {
          mat <- rbind(mat, mat.temp)
          mat.temp.pre <- mat.temp
          m <- m + 1
        }
      }          
    }    
  }  else if ((start == -Inf) && (end != Inf)) {
    while (1){
      mat.temp <- xfunp(end - dexp(1))
      if (mat.temp[2] != -Inf){
        break
      }
    }
    mat <- mat.temp
    mat.temp.pre <- mat.temp
    while (mat.temp[3] < 0 && m < m.max) {
      mat.temp <- xfunp(mat.temp[1]-2^m)
      if (mat.temp[3] < mat.temp.pre[3]){
        stop("NOT Log-concave!!")
      } else {
        mat <- rbind(mat, mat.temp)
        mat.temp.pre <- mat.temp
        m <- m + 1
      }
    }
  } else if ((start != -Inf) && (end == Inf)) {
    while (1){
      mat.temp <- xfunp(start + dexp(1))
      if (mat.temp[2] != -Inf){
        break
      }
    }
    mat <- mat.temp
    mat.temp.pre <- mat.temp
    while (mat.temp[3] > 0 && m < m.max) {
      mat.temp <- xfunp(mat.temp[1] + 2^m)
      if (mat.temp[3] > mat.temp.pre[3]){
        stop("NOT Log-concave!!")
      } else {
        mat <- rbind(mat, mat.temp)
        mat.temp.pre <- mat.temp
        m <- m + 1
      }
    }
  }  else {
    a <- start + (end-start)/3
    b <- end - (end-start)/3
    mat <- rbind(xfunp(a), xfunp(b))
  }
  mat <- mat[order(mat[, 1]), ]
  return(mat)
}
