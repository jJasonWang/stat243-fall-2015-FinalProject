library(ars)
context("Testing main function")

test_that("sampling returns error for not log-concave functions",{
  expect_error(expect_warning(ars(n=10,function(x){-x^2}),"NaNs produced"), "missing value where TRUE/FALSE needed")
})

test_that("sampling returns error with large deviation",{
  expect_error(ars(n=10,dnorm,sd=10000),"Failed to calculate z values as two adjacent points have identical derivatives! It might result from extremely large standard derivation. Reduce your standard derivation and try agian if applicable!")
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
  data <- ars(n, dnorm, start=-Inf, end=Inf, mean=mu, sd=sd)
  #Generate normal random number from normal distribution
  normal <- rnorm(n, mu, sd)

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
