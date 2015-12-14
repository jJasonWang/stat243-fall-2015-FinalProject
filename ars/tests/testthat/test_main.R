library(ars)
context("Testing main function")

test_that("sampling returns error for not log-concave functions",{
  expect_error(expect_warning(ars(n=10,function(x){-x^2}),"NaNs produced"), "missing value where TRUE/FALSE needed")
})

test_that("sampling returns error with large deviation",{
  expect_error(ars(n=10,dnorm,sd=10000),"Failed to calculate z values as two adjacent points have identical derivatives! 1, The input function might have extremely large standard derivation. Reduce your standard derivation and try agian if applicable! 2, The input function might be a modified exponential distribution. Use R built-in function dexp to sample if applicable!")
})

#Standard normal Distribution
test_that("ks.test for sample from standard normal distribution", {
  n <- 1000
  data <- ars(n, dnorm, start=-Inf, end=Inf)
  #Generate normal random number from normal distribution
  normal <- rnorm(n)

  expect_more_than(ks.test(data, normal)$p.value, 0.05)
})

#Normal(10, 100)
test_that("ks.test for sample from normal distribution(10, 100)", {
  n <- 1000; mu <- 10; sd <- 10
  data <- ars(n, dnorm, start=-Inf, end=Inf, mean=mu, sd=sd)
  #Generate normal random number from normal distribution
  normal <- rnorm(n, mu, sd)

  expect_more_than(ks.test(data, normal)$p.value, 0.05)
})

#Normal(0, 10000)
test_that("ks.test for sample from normal distribution(0, 10000)", {
  n <- 1000; sd <- 100
  data <- ars(n, dnorm, start=-Inf, end=Inf, sd=sd)
  #Generate normal random number from normal distribution
  normal <- rnorm(n, sd=sd)

  expect_more_than(ks.test(data, normal)$p.value, 0.05)
})



#Beta Distribution
test_that("ks.test for sample from beta distribution(5, 5)", {
  n <- 1000; s1 <- 5; s2 <- 5
  data <- ars(n, dbeta, start=0, end=1, shape1=s1, shape2=s2)
  #Generate normal random number from normal distribution
  beta <- rbeta(n, s1, s2)

  expect_more_than(ks.test(data, beta)$p.value, 0.05)
})

#Beta(5, 100)
test_that("ks.test for sample from beta distribution(5, 100)", {
  n <- 1000; s1 <- 5; s2 <- 100
  data <- ars(n, dbeta, start=0, end=1, shape1=s1, shape2=s2)
  #Generate normal random number from normal distribution
  beta <- rbeta(n, s1, s2)

  expect_more_than(ks.test(data, beta)$p.value, 0.05)
})

#Beta(100, 5)
test_that("ks.test for sample from beta distribution(5, 100)", {
  n <- 1000; s1 <- 100; s2 <- 5
  data <- ars(n, dbeta, start=0, end=1, shape1=s1, shape2=s2)
  #Generate normal random number from normal distribution
  beta <- rbeta(n, s1, s2)

  expect_more_than(ks.test(data, beta)$p.value, 0.05)
})



#gamma Distribution(5, 5)
test_that("ks.test for sample from gamma distribution(5, 5)", {
  n <- 1000; s <- 5; r <- 5
  data <- ars(n, dgamma, start=0, end=Inf, shape=s, rate=r)
  #Generate normal random number from normal distribution
  gamma <- rgamma(n, s, r)

  expect_more_than(ks.test(data, gamma)$p.value, 0.05)
})

#gamma(5, 100)
test_that("ks.test for sample from gamma distribution(5, 100)", {
  n <- 1000; s <- 5; r <- 100
  data <- ars(n, dgamma, start=0, end=Inf, shape=s, rate=r)
  #Generate normal random number from normal distribution
  gamma <- rgamma(n, s, r)

  expect_more_than(ks.test(data, gamma)$p.value, 0.05)
})

#gamma(100, 5)
test_that("ks.test for sample from gamma distribution(100, 5)", {
  n <- 1000; s <- 100; r <- 5
  data <- ars(n, dgamma, start=0, end=Inf, shape=s, rate=r)
  #Generate normal random number from normal distribution
  gamma <- rgamma(n, s, r)

  expect_more_than(ks.test(data, gamma)$p.value, 0.05)
})



#Logistic Distribution(0, 1)
test_that("ks.test for sample from Logistic Distribution(0, 1)", {
  n <- 1000
  data <- ars(n, dlogis, start=-Inf, end=Inf)
  #Generate normal random number from normal distribution
  logis <- rlogis(n)

  expect_more_than(ks.test(data, logis)$p.value, 0.05)
})

#logistic(0, 100)
test_that("ks.test for sample from logistic(0, 100)", {
  n <- 1000; loc <- 0; scale <- 100
  data <- ars(n, dlogis, start=-Inf, end=Inf, location=loc, scale=scale)
  #Generate normal random number from normal distribution
  logis <- rlogis(n, loc, scale)

  expect_more_than(ks.test(data, logis)$p.value, 0.05)
})

#logistic(10, 1)
test_that("ks.test for sample from logistic(10, 1)", {
  n <- 1000; loc <- 10; scale <- 1
  data <- ars(n, dlogis, start=-Inf, end=Inf, location=loc, scale=scale)
  #Generate normal random number from normal distribution
  logis <- rlogis(n, loc, scale)

  expect_more_than(ks.test(data, logis)$p.value, 0.05)
})



#Weibull Distribution(1, 5)
test_that("ks.test for sample from Weibull Distribution(1, 5)", {
  n <- 1000; s <- 1; scale <- 5
  data <- ars(n, dweibull, start=0, end=Inf, shape=s, scale=scale)
  #Generate normal random number from normal distribution
  weibull <- rweibull(n, s, scale)

  expect_more_than(ks.test(data, weibull)$p.value, 0.05)
})

#Weibull Distribution(1, 100)
test_that("ks.test for sample from Weibull Distribution(1, 100)", {
  n <- 1000; s <- 1; scale <- 100
  data <- ars(n, dweibull, start=0, end=Inf, shape=s, scale=scale)
  #Generate normal random number from normal distribution
  weibull <- rweibull(n, s, scale)

  expect_more_than(ks.test(data, weibull)$p.value, 0.05)
})



#Uniform Distribution(0, 1)
test_that("ks.test for sample from Uniform Distribution(0, 1)", {
  n <- 1000
  data <- ars(n, dunif, min=0, max=1, start=0, end=1)
  #Generate normal random number from normal distribution
  unif <- runif(n)

  expect_more_than(ks.test(data, unif)$p.value, 0.05)
})

#Uniform Distribution(0, 10)
test_that("ks.test for sample from Uniform Distribution(0, 10)", {
  n <- 1000; minimum <- 0; maximum <- 10
  data <- ars(n, dunif, min=minimum, max=maximum, start=0, end=10)
  #Generate normal random number from normal distribution
  unif <- runif(n, minimum, maximum)

  expect_more_than(ks.test(data, unif)$p.value, 0.05)
})

#Uniform Distribution(10, 100)
test_that("ks.test for sample from Uniform Distribution(10, 100)", {
  n <- 1000; minimum <- 10; maximum <- 100
  data <- ars(n, dunif, min=minimum, max=maximum, start=10, end=100)
  #Generate normal random number from normal distribution
  unif <- runif(n, minimum, maximum)

  expect_more_than(ks.test(data, unif)$p.value, 0.05)
})


#Exponential Distribution(1)
test_that("ks.test for sample from Uniform Distribution(0, 1)", {
  n <- 1000
  data <- ars(n, dexp, start=0, end=Inf)
  #Generate normal random number from normal distribution
  expo <- rexp(n)

  expect_more_than(ks.test(data, expo)$p.value, 0.05)
})

#Exponential Distribution(100)
test_that("ks.test for sample from Uniform Distribution(0, 1)", {
  n <- 1000; rate <- 100
  data <- ars(n, dexp, rate=rate, start=0, end=Inf)
  #Generate normal random number from normal distribution
  expo <- rexp(n, rate)

  expect_more_than(ks.test(data, expo)$p.value, 0.05)
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
  #Generate normal random number from normal distribution
  rn <- rf(n)

  expect_more_than(ks.test(data, rn)$p.value, 0.05)
})
