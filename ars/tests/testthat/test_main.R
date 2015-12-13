library(ars)
context("Testing main function")

set.seed(0)

#Sample size is negative
test_that("negative sample size",{
  expect_error(ars(n=-1, dnorm), "Invalid sample size")
})


#lower bound bigger than upper bound
test_that("negative sample size",{
  expect_error(ars(n=100, dnorm, start=Inf, end=-Inf), "Invalid x interval")
})

#Not log-concave
test_that("sampling returns error for not log-concave functions",{
  f <- function(x) -x^2
  expect_error(expect_warning(ars(n=10, f),"NaNs produced"), "missing value where TRUE/FALSE needed")
})


#Standard normal Distribution
test_that("ks.test for sample from standard normal distribution", {
  n <- 1000
  data <- ars(n, dnorm, start=-Inf, end=Inf)

  expect_more_than(ks.test(data, "pnorm")$p.value, 0.05)
})

#Normal(10, 100)
test_that("ks.test for sample from normal distribution(10, 100)", {
  n <- 1000; mu <- 10; sd <- 10
  data <- ars(n, dnorm, start=-Inf, end=Inf, mean=mu, sd=sd)

  expect_more_than(ks.test(data, "pnorm", mean=mu, sd=sd)$p.value, 0.05)
})

#Normal(0, 10000)
test_that("ks.test for sample from normal distribution(0, 10000)", {
  n <- 1000; sd <- 100
  data <- ars(n, dnorm, start=-Inf, end=Inf, sd=sd)

  expect_more_than(ks.test(data, "pnorm", sd=sd)$p.value, 0.05)
})



#Beta Distribution
test_that("ks.test for sample from beta distribution(5, 5)", {
  n <- 1000; s1 <- 5; s2 <- 5
  data <- ars(n, dbeta, start=0, end=1, shape1=s1, shape2=s2)

  expect_more_than(ks.test(data, "pbeta", shape1=s1, shape=s2)$p.value, 0.05)
})

#Beta(5, 100)
test_that("ks.test for sample from beta distribution(5, 100)", {
  n <- 1000; s1 <- 5; s2 <- 100
  data <- ars(n, dbeta, start=0, end=1, shape1=s1, shape2=s2)

  expect_more_than(ks.test(data, "pbeta", shape1=s1, shape2=s2)$p.value, 0.05)
})

#Beta(100, 5)
test_that("ks.test for sample from beta distribution(5, 100)", {
  n <- 1000; s1 <- 100; s2 <- 5
  data <- ars(n, dbeta, start=0, end=1, shape1=s1, shape2=s2)

  expect_more_than(ks.test(data, "pbeta", shape1=s1, shape2=s2)$p.value, 0.05)
})



#gamma Distribution(5, 5)
test_that("ks.test for sample from gamma distribution(5, 5)", {
  n <- 1000; s <- 5; r <- 5
  data <- ars(n, dgamma, start=0, end=Inf, shape=s, rate=r)

  expect_more_than(ks.test(data, "pgamma", shape=s, rate=r)$p.value, 0.05)
})

#gamma(5, 100)
test_that("ks.test for sample from gamma distribution(5, 100)", {
  n <- 1000; s <- 5; r <- 100
  data <- ars(n, dgamma, start=0, end=Inf, shape=s, rate=r)

  expect_more_than(ks.test(data, "pgamma", shape=s, rate=r)$p.value, 0.05)
})

#gamma(100, 5)
test_that("ks.test for sample from gamma distribution(100, 5)", {
  n <- 1000; s <- 100; r <- 5
  data <- ars(n, dgamma, start=0, end=Inf, shape=s, rate=r)

  expect_more_than(ks.test(data, "pgamma", shape=s, rate=r)$p.value, 0.05)
})



#Logistic Distribution(0, 1)
test_that("ks.test for sample from Logistic Distribution(0, 1)", {
  n <- 1000
  data <- ars(n, dlogis, start=-Inf, end=Inf)

  expect_more_than(ks.test(data, "plogis")$p.value, 0.05)
})

#logistic(0, 10)
test_that("ks.test for sample from logistic(0, 10)", {
  n <- 1000; loc <- 0; scale <- 10
  data <- ars(n, dlogis, start=-Inf, end=Inf, location=loc, scale=scale)

  expect_more_than(ks.test(data, "plogis", location=loc, scale=scale)$p.value, 0.05)
})

#logistic(10, 1)
test_that("ks.test for sample from logistic(10, 1)", {
  n <- 1000; loc <- 10; scale <- 1
  data <- ars(n, dlogis, start=-Inf, end=Inf, location=loc, scale=scale)

  expect_more_than(ks.test(data, "plogis", location=loc, scale=scale)$p.value, 0.05)
})



#Weibull Distribution(1, 5)
test_that("ks.test for sample from Weibull Distribution(1, 5)", {
  n <- 1000; s <- 1; scale <- 5
  data <- ars(n, dweibull, start=0, end=Inf, shape=s, scale=scale)

  expect_more_than(ks.test(data, "pweibull", shape=s, scale=scale)$p.value, 0.05)
})

#Weibull Distribution(1, 100)
test_that("ks.test for sample from Weibull Distribution(1, 100)", {
  n <- 1000; s <- 1; scale <- 100
  data <- ars(n, dweibull, start=0, end=Inf, shape=s, scale=scale)

  expect_more_than(ks.test(data, "pweibull", shape=s, scale=scale)$p.value, 0.05)
})



#Uniform Distribution(0, 1)
test_that("ks.test for sample from Uniform Distribution(0, 1)", {
  n <- 1000
  data <- ars(n, dunif, min=0, max=1, start=0, end=1)

  expect_more_than(ks.test(data, "punif")$p.value, 0.05)
})

#Uniform Distribution(0, 10)
test_that("ks.test for sample from Uniform Distribution(0, 10)", {
  n <- 1000; minimum <- 0; maximum <- 10
  data <- ars(n, dunif, min=minimum, max=maximum, start=0, end=10)

  expect_more_than(ks.test(data, "punif", min=minimum, max=maximum)$p.value, 0.05)
})

#Uniform Distribution(10, 100)
test_that("ks.test for sample from Uniform Distribution(10, 100)", {
  n <- 1000; minimum <- 10; maximum <- 100
  data <- ars(n, dunif, min=minimum, max=maximum, start=10, end=100)

  expect_more_than(ks.test(data, "punif", min=minimum, max=maximum)$p.value, 0.05)
})


#Exponential Distribution(1)
test_that("ks.test for sample from Uniform Distribution(0, 1)", {
  n <- 1000
  data <- ars(n, dexp, start=0, end=Inf)

  expect_more_than(ks.test(data, "pexp")$p.value, 0.05)
})

#Exponential Distribution(100)
test_that("ks.test for sample from Uniform Distribution(0, 1)", {
  n <- 1000; rate <- 100
  data <- ars(n, dexp, rate=rate, start=0, end=Inf)

  expect_more_than(ks.test(data, "pexp", rate=rate)$p.value, 0.05)
})

library(pracma)

f <- function(x) exp(-x^2)
rf <- function(n) {
  x <- rep(0, n)
  i <- 0
  while(TRUE){
    xnew <- erfinv((2/sqrt(pi))*runif(1))
    if(is.na(xnew)){
      next
    }else{
      i <- i + 1
      x[i] <- xnew
      if(i == n) break
    }
  }
  x
}

test_that("ks.test for sample", {
  n <- 1000
  data <- ars(n, f, start=0, end=Inf)
  #Generate random number from f
  rn <- rf(n)

  expect_more_than(ks.test(data, rn)$p.value, 0.05)
})
