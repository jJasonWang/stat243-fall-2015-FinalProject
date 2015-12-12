library(ars)
context("Testing squeezing function evall() given x")

test_that("evaluate lower bound value with given x",{
  Tk=c(0,1)
  actual = evall(0.5,Tk,dnorm(Tk,log=TRUE))
  expect = -1.168938533204672669541
  expect_equal(actual,expect)
})
