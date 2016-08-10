# MCMC Scheme 1 prototype: LOGISTIC, TWO PARAMETER MODEL
# The scheme is comprised of 3 steps
# Step 1: Sample the person parameter: theta
# Step 2: Sample the item parameters: (a, b)
# Step 3: Sample the hyperparameters: (means and variances)
# anything for KSI must be multivariate normal!!!!

library(mvtnorm)

theta.proposal <- function(param){
  mu <- param[1]
  s <- sqrt(param[2])
  n <- 1
  
  proposal <- rnorm(n, mean = mu, sd = s)
  return(proposal)
}


data.likelihood <- function(param, data){
  x <- param[1]
  a <- param[2]
  b <- param[3]
  theta <- param[4]
  y <- data
  # this is already a log-likelihood for probit
  like <- sum(y*log(pnorm(a*theta-b)+(1-y)*log(1-pnorm(a*theta-b))))
  return(like)
}

prior.theta <- function(param){
  mu <- param[1]
  s <- sqrt(param[2])
  y <- parama[3]
  # this has to be a log-prior, so both values can be added and exponentiated
  # at the time of accepting the value
  prior <- dnorm(x = y, mean = mu, sd = s, log = T)
  return(prior)
}

ksi.proposal <- function(param){
  # the values for ksi originate from a bivariate normal distribution
  mu.a <- param[1]
  mu.b <- param[2]
  s.a <- sqrt(param[3])
  s.b <- sqrt(param[4])
  n <- 1
  proposal <- rmvnorm(n, mean = mu, sd = s)
  list(l1 = proposal[1], l2 = proposal[2])
}

# likelihood given ksi and next theta has been specified. 
# prior of ksi... is it a bivariate nd? this must be multivariate!!!!

ksi.prior <- function(param){
  mu <- param[1]
  sigma <- param[2]
  y <- param[3]
  prior <- dnorm(x = y, mean = mu, sigma = sigma, log = T)
  return(prior)
}


####################################################################
# real shitstorm: full conditionals
####################################################################

# sample mu

mu.full.c <- function(obs, param){
  n <- obs[1]
  no <- obs[2]
  mu0 <- param[1]
  theta.m <- param[2]
  s.theta <- param[3]
  
  mu <- (no/(n+no))*mu0 + (n/(n+no))*theta.m
  s <- sqrt(s.theta/(n+no))
  
  full.c <- rnorm(1, mean = mu, sd = s)
  return(full.c)
}


# sample sigma.squared: this might be pure bullshit
sigma.full.c <- function(obs, param, ){
  n <- obs[1]
  no <- obs[2]
  mu0 <- param[1]
  s0 <- param[2]
  theta.m <- paramπ[3]
  
  g1 <- 1
  g2 <- 1/2
  mu <- g1 + n/2
  sigma <- g2 + (n-1)*s0/2+((n*no)/2*(n+no))*((theta.m-m0)^2)
  s <- sigma
  
  full.c <- rinvgamma2(1, n0 = mu, var0 = s)
  return(full.c)
}


# full conditionals for mean vectors and variance/cov matrices of ksi.


