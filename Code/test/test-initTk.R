# Please run the function code first ""
setwd("~/ps-stat243/final-project/Code")

# dnorm
fun <- function(x){
  log(dnorm(x))
}
fun_deriv <- function(x){
  h = 1e-8
  (fun(x + h) - fun(x - h))/(2*h)
}

InitTk(fun,fun_deriv,-Inf,Inf)

InitTk(fun,fun_deriv,-10,Inf)

InitTk(fun,fun_deriv,-Inf,10)

InitTk(fun,fun_deriv,-10,10)