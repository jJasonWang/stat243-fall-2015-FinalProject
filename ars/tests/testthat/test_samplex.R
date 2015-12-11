library(ars)
context("Testing the sampling function samplex(u,y)")

test_that("test the sampling function with some given u and y",{
  a <- c(3,-1,-10)
  b <- c(5,6,20)
  u <- list(parameter=cbind(a,b),breaks=c(-Inf,0,9,Inf))
  actual <- samplex(u,c(1,5,7))
  expect <- c(-0.02947048, 0.57736628, 1.49332731)
  expect_equal(actual,expect)
})
