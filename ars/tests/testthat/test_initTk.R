library(ars)
context("Testing the function initTk to intialize the abscissae  in Tk")

test_that(,{
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
  expect = c(-0.8986916204307355826586,2.3282192761685571724684,9.5938815961267671639234, 11.8852950495343527848036, 12.6295428488079330975324)
  expect_equal(actual,expect)
  #testing norm #2
  actual = unname(initTk(h, h_deriv,-50,Inf)[,1])
  expect = c(-24.177191220223903656006, -24.053618679288774728775,-23.229720379691570997238,-21.817266359459608793259,-10.824900812935084104538,-4.196987321750661692477,1.466418169904500246048)
  expect_equal(actual,expect)
  #testing Modified Laplace distribution f(x) = exp(-|x^3|)/2
  h <- function(x){
    return (-abs(x^3))
  }
  actual = unname(initTk(h,h_deriv,-Inf,25)[,1])
  expect =c(-3.056988547676116496632,5.494072851604908436229,7.502954386985987156322,12.716344515673011272838,13.564914600803643196514,15.434325063015460699489,19.603171600028872489929)
  expect_equal(actual,expect)
  actual = unname(initTk(h,h_deriv,-10,10)[,1])
  expect=c(-3.333333333333333037274,3.333333333333333037274)
  expect_equal(actual,expect)
})
