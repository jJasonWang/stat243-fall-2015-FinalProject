#1. samplex()
library(ars)
context("Testing the sampling function samplex()")

test_that("test the sampling function with some given u and y",{
  a <- c(3,-1,-10)
  b <- c(5,6,20)
  u <- list(parameter=cbind(a,b),breaks=c(-Inf,0,9,Inf))
  actual <- samplex(u,c(0.1,0.5,0.7))
  actual <- round(actual,digits = 5)
  expect <- c(-0.02947,  0.57737,  1.08805)
  expect_equal(actual,expect)
})

#2.genu()
context("Testing the function genu() to generate right parameters and breaks")

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

#3.evalu()
context("Testing envelope function evalu() with given x value")

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

#evall()
context("Testing squeezing function evall() given x")

test_that("evaluate lower bound value with given x",{
  Tk=c(0,1)
  actual = evall(0.5,Tk,dnorm(Tk,log=TRUE))
  expect = -1.168938533204672669541
  expect_equal(actual,expect)
})

#initTk()
context("Testing the function initTk to intialize the abscissae  in Tk")

test_that("Initialize the abscissae in Tk",{
  h_deriv <- function(x){
    dx = 1e-8
    (h(x + dx) - h(x - dx))/(2*dx)
  }
  #testing the norm #1
  h <- function(x){
    log(dnorm(x))
  }
  set.seed(0)
  actual = unname(initTk(h, h_deriv,-Inf,Inf)[,1])
  expect = c(-0.2230484045281170280006,0.7215780604306480938703)
  expect_equal(actual,expect)
  #testing norm #2
  actual = unname(initTk(h, h_deriv,-50,Inf)[,1])
  expect = c(-0.01899085593053317921886, 0.15990535832450006314964)
  expect_equal(actual,expect)
  #testing Modified Laplace distribution f(x) = exp(-|x^3|)/2
  h <- function(x){
    return (-abs(x^3))
  }
  actual = unname(initTk(h,h_deriv,-Inf,25)[,1])
  expect =c(-11.93054597410273842684, 12.70437946585106558928)
  expect_equal(actual,expect)
  actual = unname(initTk(h,h_deriv,-10,10)[,1])
  expect = c(-3.333333333333333037274, 3.333333333333333037274)
  expect_equal(actual,expect)
  #testing not log concave t-distribution
  h <- function(x){
    return (log(dt(x,1)))
  }
  actual = unname(initTk(h, h_deriv,-Inf,Inf)[,1])
  expect = c(-0.05767172747536955523806, 1.01650885392192513734244)
  expect_equal(actual,expect)
})

# #check the error mssg
# h <- function(x){
#   log(dunif(x))
# }
# unname(initTk(h,h_deriv,0,1)[,1])
