library(ars)
context("evaluate envelope function with given x value")

test_that("eavluate envelope function gives upper bound value",{
  Tk = c(-1,1)
  hfun = dnorm
  h <- 1e-8
  hfun_deriv <- function(x){
    (hfun(x+h) - hfun(x-h))/(2*h)
  }
  mat = cbind(Tk,hfun(Tk),hfun_deriv(Tk))
  u = genu(mat,-10,10)
  actual = evalu(x.temp=1,u)
  expect = 0.2419707245191433653275
  expect_equal(actual,expect)
})
