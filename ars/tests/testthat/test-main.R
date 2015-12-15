library(ars)
context("Testing main function")
set.seed(3)

test_that("Negative Sample size",{
  cat('\nTesting error message for negative sample size...\n')
  expect_error(ars(n=-1, dnorm), "Invalid sample size")
  cat('..Pass\n')
})

test_that("Invalid x interval",{
  cat('Testing error message for Invalid x interval...\n')
  expect_error(ars(n=1, dnorm, start=Inf, end=-Inf), "Invalid x interval")
  cat('..Pass\n')
})

test_that("sampling returns error for not log-concave functions",{
  expect_error(expect_warning(ars(n=10,function(x){-x^2}),"NaNs produced"), "missing value where TRUE/FALSE needed")
})

cat('-----The following test will be KS test for several distribution-----\n')
cat('-----type I error rate(alpha) is 0.05-----\n')

#Standard normal Distribution
test_that("ks.test for sample from standard normal distribution", {
  cat('KS test for standard normal distribution...\n')
  n <- 1000
  data <- ars(n, dnorm, start=-Inf, end=Inf)
  #Generate normal random number from normal distribution
  normal <- rnorm(n)

  expect_more_than(ks.test(data, normal)$p.value, 0.05)
  cat('..Pass\n')
})

#Normal(10, 100)
test_that("ks.test for sample from normal distribution(10, 100)", {
  cat('KS test for normal(10, 100) distribution...\n')
  n <- 1000; mu <- 10; sd <- 10
  data <- ars(n, dnorm, start=-Inf, end=Inf, mean=mu, sd=sd)
  #Generate normal random number from normal distribution
  normal <- rnorm(n, mu, sd)
  cat('..Pass\n')

  expect_more_than(ks.test(data, normal)$p.value, 0.05)
})

#Normal(0, 10000)
test_that("ks.test for sample from normal distribution(0, 10000)", {
  cat('KS test for normal(0, 10000) distribution...\n')
  n <- 1000; sd <- 100
  data <- ars(n, dnorm, start=-Inf, end=Inf, sd=sd)
  #Generate normal random number from normal distribution
  normal <- rnorm(n, sd=sd)

  expect_more_than(ks.test(data, normal)$p.value, 0.05)
  cat('..Pass\n')
})



#Beta Distribution
test_that("ks.test for sample from beta distribution(5, 5)", {
  cat('KS test for beta(5, 5) distribution...\n')
  n <- 1000; s1 <- 5; s2 <- 5
  data <- ars(n, dbeta, start=0, end=1, shape1=s1, shape2=s2)
  #Generate normal random number from normal distribution
  beta <- rbeta(n, s1, s2)

  expect_more_than(ks.test(data, beta)$p.value, 0.05)
  cat('..Pass\n')
})

#Beta(5, 100)
test_that("ks.test for sample from beta distribution(5, 100)", {
  cat('KS test for beta(5, 100) distribution...\n')
  n <- 1000; s1 <- 5; s2 <- 100
  data <- ars(n, dbeta, start=0, end=1, shape1=s1, shape2=s2)
  #Generate normal random number from normal distribution
  beta <- rbeta(n, s1, s2)

  expect_more_than(ks.test(data, beta)$p.value, 0.05)
  cat('..Pass\n')
})

#Beta(100, 5)
test_that("ks.test for sample from beta distribution(5, 100)", {
  cat('KS test for beta(100, 5) distribution...\n')
  n <- 1000; s1 <- 100; s2 <- 5
  data <- ars(n, dbeta, start=0, end=1, shape1=s1, shape2=s2)
  #Generate normal random number from normal distribution
  beta <- rbeta(n, s1, s2)

  expect_more_than(ks.test(data, beta)$p.value, 0.05)
  cat('..Pass\n')
})



#gamma Distribution(5, 5)
test_that("ks.test for sample from gamma distribution(5, 5)", {
  cat('KS test for gamma(5, 5) distribution...\n')
  n <- 1000; s <- 5; r <- 5
  data <- ars(n, dgamma, start=0, end=Inf, shape=s, rate=r)
  #Generate normal random number from normal distribution
  gamma <- rgamma(n, s, r)

  expect_more_than(ks.test(data, gamma)$p.value, 0.05)
  cat('..Pass\n')
})

#gamma(5, 100)
test_that("ks.test for sample from gamma distribution(5, 100)", {
  cat('KS test for gamma(5, 100) distribution...\n')
  n <- 1000; s <- 5; r <- 100
  data <- ars(n, dgamma, start=0, end=Inf, shape=s, rate=r)
  #Generate normal random number from normal distribution
  gamma <- rgamma(n, s, r)

  expect_more_than(ks.test(data, gamma)$p.value, 0.05)
  cat('..Pass\n')
})

#gamma(100, 5)
test_that("ks.test for sample from gamma distribution(100, 5)", {
  cat('KS test for gamma(100, 5) distribution...\n')
  n <- 1000; s <- 100; r <- 5
  data <- ars(n, dgamma, start=0, end=Inf, shape=s, rate=r)
  #Generate normal random number from normal distribution
  gamma <- rgamma(n, s, r)

  expect_more_than(ks.test(data, gamma)$p.value, 0.05)
  cat('..Pass\n')
})



#Logistic Distribution(0, 1)
test_that("ks.test for sample from Logistic Distribution(0, 1)", {
  cat('KS test for logistic(0, 1) distribution...\n')
  n <- 1000
  data <- ars(n, dlogis, start=-Inf, end=Inf)
  #Generate normal random number from normal distribution
  logis <- rlogis(n)

  expect_more_than(ks.test(data, logis)$p.value, 0.05)
  cat('..Pass\n')
})

#logistic(0, 100)
test_that("ks.test for sample from logistic(0, 100)", {
  cat('KS test for logistic(0, 100) distribution...\n')
  n <- 1000; loc <- 0; scale <- 100
  data <- ars(n, dlogis, start=-Inf, end=Inf, location=loc, scale=scale)
  #Generate normal random number from normal distribution
  logis <- rlogis(n, loc, scale)

  expect_more_than(ks.test(data, logis)$p.value, 0.05)
  cat('..Pass\n')
})

#logistic(10, 1)
test_that("ks.test for sample from logistic(10, 1)", {
  cat('KS test for logistic(10, 1) distribution...\n')
  n <- 1000; loc <- 10; scale <- 1
  data <- ars(n, dlogis, start=-Inf, end=Inf, location=loc, scale=scale)
  #Generate normal random number from normal distribution
  logis <- rlogis(n, loc, scale)

  expect_more_than(ks.test(data, logis)$p.value, 0.05)
  cat('..Pass\n')
})



#Weibull Distribution(1, 5)
test_that("ks.test for sample from Weibull Distribution(1, 5)", {
  cat('KS test for weibull(1, 5) distribution...\n')
  n <- 1000; s <- 1; scale <- 5
  data <- ars(n, dweibull, start=0, end=Inf, shape=s, scale=scale)
  #Generate normal random number from normal distribution
  weibull <- rweibull(n, s, scale)

  expect_more_than(ks.test(data, weibull)$p.value, 0.05)
  cat('..Pass\n')
})

#Weibull Distribution(1, 100)
test_that("ks.test for sample from Weibull Distribution(1, 100)", {
  cat('KS test for weibull(1, 100) distribution...\n')
  n <- 1000; s <- 1; scale <- 100
  data <- ars(n, dweibull, start=0, end=Inf, shape=s, scale=scale)
  #Generate normal random number from normal distribution
  weibull <- rweibull(n, s, scale)

  expect_more_than(ks.test(data, weibull)$p.value, 0.05)
  cat('..Pass\n')
})



#Uniform Distribution(0, 1)
test_that("ks.test for sample from Uniform Distribution(0, 1)", {
  cat('KS test for uniform(0, 1) distribution...\n')
  n <- 1000
  data <- ars(n, dunif, min=0, max=1, start=0, end=1)
  #Generate normal random number from normal distribution
  unif <- runif(n)

  expect_more_than(ks.test(data, unif)$p.value, 0.05)
  cat('..Pass\n')
})

#Uniform Distribution(0, 10)
test_that("ks.test for sample from Uniform Distribution(0, 10)", {
  cat('KS test for uniform(0, 10) distribution...\n')
  n <- 1000; minimum <- 0; maximum <- 10
  data <- ars(n, dunif, min=minimum, max=maximum, start=0, end=10)
  #Generate normal random number from normal distribution
  unif <- runif(n, minimum, maximum)

  expect_more_than(ks.test(data, unif)$p.value, 0.05)
  cat('..Pass\n')
})

#Uniform Distribution(10, 100)
test_that("ks.test for sample from Uniform Distribution(10, 100)", {
  cat('KS test for uniform(10, 100) distribution...\n')
  n <- 1000; minimum <- 10; maximum <- 100
  data <- ars(n, dunif, min=minimum, max=maximum, start=10, end=100)
  #Generate normal random number from normal distribution
  unif <- runif(n, minimum, maximum)

  expect_more_than(ks.test(data, unif)$p.value, 0.05)
  cat('..Pass\n')
})


#Exponential Distribution(1)
test_that("ks.test for sample from Uniform Distribution(0, 1)", {
  cat('KS test for exponential(1) distribution...\n')
  n <- 1000
  data <- ars(n, dexp, start=0, end=Inf)
  #Generate normal random number from normal distribution
  expo <- rexp(n)

  expect_more_than(ks.test(data, expo)$p.value, 0.05)
  cat('..Pass\n')
})

#Exponential Distribution(100)
test_that("ks.test for sample from Uniform Distribution(0, 1)", {
  cat('KS test for exponential(100) distribution...\n')
  n <- 1000; rate <- 100
  data <- ars(n, dexp, rate=rate, start=0, end=Inf)
  #Generate normal random number from normal distribution
  expo <- rexp(n, rate)

  expect_more_than(ks.test(data, expo)$p.value, 0.05)
  cat('..Pass\n')
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
  cat('KS test for exp(-x^2)...\n')
  n <- 1000
  data <- ars(n, f, start=0, end=Inf)
  #Generate normal random number from exp(-x^2)
  rn <- rf(n)

  expect_more_than(ks.test(data, rn)$p.value, 0.05)
  cat('..Pass\n')
})
