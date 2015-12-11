library(ars)
context("Testing the sampling function samplex(u,y)")

test_that("test the sampling function with some given u and y",{
  a <- c(3,-1,-10)
  b <- c(5,6,20)
  u <- list(parameter=cbind(a,b),breaks=c(-Inf,0,9,Inf))
  actual <- samplex(u,c(0.1,0.5,0.7))
  actual <- round(actual,digits = 5)
  expect <- c(-0.02947,  0.57737,  1.08805)
  expect_equal(actual,expect)
})
