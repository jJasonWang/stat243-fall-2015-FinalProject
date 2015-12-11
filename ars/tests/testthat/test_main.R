library(ars)
context("Testing main function")

test_that("sampling density is same with given density",{
  set.seed(0)
  expect_equal(mean(ars(n=10,dnorm)),0.189488161838492) #norm
  expect_equal(mean(ars(n=10,function(x){exp(-abs(x^3))/2},start=0)),0.551939710611255)
  expect_equal(mean(ars(n=10,function(x){x*(1-x)^4},start=0,end=1)),0.291330316229308) #beta
})

test_that("sampling returns error for not log-concave functions",{
  expect_error(expect_warning(ars(n=10,function(x){-x^2}),"NaNs produced"), "missing value where TRUE/FALSE needed")
})

test_that("sampling returns error with large deviation",{
  expect_error(ars(n=10,dnorm,sd=10000),"Failed to calculate z values as two adjacent points have identical derivatives! It might result from extremely large standard derivation. Reduce your standard derivation and try agian if applicable!")
})
