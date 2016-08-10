# Sample from weighted normal distributions, one-parameter MCMC

p <- 0.4
mu <- c(-1, 2)
sd <- c(.5, 2)
f <- function(x) {
  p     * dnorm(x, mu[1], sd[1]) +
  (1-p) * dnorm(x, mu[2], sd[2])
}

curve(f(x), col="red", -4, 8, n=301, las=1)

q <- function(x){ rnorm(1, x, 4)} # proposal distribution

step <- function(x, f, q) { #updating the chain
  ## Pick new point
  xp <- q(x)
  ## Acceptance probability:
  alpha <- min(1, f(xp) / f(x))
  ## Accept new point with probability alpha:
  if (runif(1) < alpha)
    x <- xp
  ## Returning the point:
  x
}

# this runs the chain for nsteps
run <- function(x, f, q, nsteps) {
  res <- matrix(NA, nsteps, length(x))
  for (i in seq_len(nsteps))
    res[i,] <- x <- step(x, f, q)
  drop(res)
}

res <- run(-10, f, q, 1000)

layout(matrix(c(1, 2), 1, 2), widths=c(4, 1))
par(mar=c(4.1, .5, .5, .5), oma=c(0, 4.1, 0, 0))
plot(res, type="s", xpd=NA, ylab="Parameter", xlab="Sample", las=1)
usr <- par("usr")
xx <- seq(usr[3], usr[4], length=301)
plot(f(xx), xx, type="l", yaxs="i", axes=FALSE, xlab="")


hist(res, 50, freq=FALSE, main="", ylim=c(0, .4), las=1,
     xlab="x", ylab="Probability density")
z <- integrate(f, -Inf, Inf)$value
curve(f(x) / z, add=TRUE, col="red", n=200)

# RUN LONGER
set.seed(1)
res.long <- run(-10, f, q, 50000)
hist(res.long, 100, freq=FALSE, main="", ylim=c(0, .4), las=1,
     xlab="x", ylab="Probability density", col="grey")
z <- integrate(f, -Inf, Inf)$value
curve(f(x) / z, add=TRUE, col="red", n=200)

# different proposals
res.fast <- run(-10, f, function(x) rnorm(1, x,  33), 1000)
res.slow <- run(-10, f, function(x) rnorm(1, x,  .3), 1000)

layout(matrix(c(1, 2), 1, 2), widths=c(4, 1))
par(mar=c(4.1, .5, .5, .5), oma=c(0, 4.1, 0, 0))
plot(res, type="s", xpd=NA, ylab="Parameter", xlab="Sample", las=1,
     col="grey")
lines(res.fast, col="red")
lines(res.slow, col="blue")
plot(f(xx), xx, type="l", yaxs="i", axes=FALSE)


par(mfrow=c(1, 3), mar=c(4, 2, 3.5, .5))
acf(res.slow, las=1, main="Small steps")
acf(res, las=1, main="Intermediate")
acf(res.fast, las=1, main="Large steps")


coda::effectiveSize(res)
coda::effectiveSize(res.fast)
coda::effectiveSize(res.slow)

n <- 10^(2:5)
samples <- lapply(n, function(n) run(-10, f, q, n))
xlim <- range(sapply(samples, range))
br <- seq(xlim[1], xlim[2], length=100)

hh <- lapply(samples, function(x) hist(x, br, plot=FALSE))
ylim <- c(0, max(f(xx)))

par(mfrow=c(2,2), mar=rep(.5, 4), oma=c(4, 4, 0, 0))
for (h in hh) {
  plot(h, main="", freq=FALSE, yaxt="n",
       ylim=range(h$density, ylim))
  curve(f(x), add=TRUE, col="red", n=300)
}
