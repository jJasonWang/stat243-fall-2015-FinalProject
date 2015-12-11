library(ars)
context("generate envelope function u(x)")

test_that("generate envelope function gives right parameters and breaks",{
  Tk = c(-1,1)
  hfun = dnorm
  h <- 1e-8
  hfun_deriv <- function(x){
    (hfun(x+h) - hfun(x-h))/(2*h)
  }
  mat = cbind(Tk,hfun(Tk),hfun_deriv(Tk))
  actual= c(mean(genu(mat,-10,10)$parameter), genu(mat,-10,10)$breaks)
  expect= c(0.2419707228783466446131, c(-10.0000000,0,10.0000000))
  expect_equal(actual,expect)
})

#need to add
test_that("generate envelope function gives right error message",{
  expect_equal(1,1)
})
