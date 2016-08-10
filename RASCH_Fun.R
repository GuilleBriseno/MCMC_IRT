# RASCH MCMC SAMPLER: Proposals, likelihood, and priors
library(MCMCpack)

# likelihood of the data
data.like <- function(param, data){
  theta <- param[1] # person ability
  b <- param[2] # item difficulty
  pi <- exp(theta-b)/(1+exp(theta-b)) # link
  y <- data # data
  
  # log likelihood
  like <- sum(y*log(pi)+(1-y)*log(1-pi)) 
  return(like)
}

# Ability prior
theta.prior <- function(param, val){
  mu <- param[1] # prior mean
  s <- sqrt(param[2]) # prior standard deviation
  y <- val # observed "x" value from prior
  
  prior <- dnorm(x = y, mean = mu, sd = s, log = T) 
  return(prior)
}

# Difficulty prior
b.prior <- function(param, val){
  mu <- param[1]
  s <- sqrt(param[2])
  y <- val
  
  prior <- dnorm(x = y, mean = mu, sd = s, log = T)
  return(prior)
} 

# hyperpriors, basically the prior of the parameters for the priors...
mu.t.prior <- function(var){
  s <- sqrt(var)
  hyprior <- rnorm(1, mean = 0, sd = s)
  return(hyprior)
}

mu.b.prior <- function(var){
  s <- sqrt(var)
  hyprior <- rnorm(1, mean = 0, sd = s)
  return(hyprior)
}

var.t.prior <- function(){ return(rinvgamma(1, shape = 0.5, scale = 0.5))}
var.b.prior <- rinvgamma(1, shape = 0.5, scale = 0.5)

# proposal, centered around the current value of the chain
prop.theta <- function(current){
  proposal <- rnorm(1, mean = current, sd = 1)
  return(proposal)
}

prop.b <- function(current){
  proposal <- rnorm(1, mean = current, sd = 1)
  return(proposal)
}
# DONE
