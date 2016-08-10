# MCMC in two dimensions

make.mvn <- function(mean, vcv) {
  logdet <- as.numeric(determinant(vcv, TRUE)$modulus)
  tmp <- length(mean) * log(2 * pi) + logdet
  vcv.i <- solve(vcv)
  
  function(x) {
    dx <- x - mean
    exp(-(tmp + rowSums((dx %*% vcv.i) * dx))/2)
  }
}

mu1 <- c(-1, 1)
mu2 <- c(2, -2)
vcv1 <- matrix(c(1, .25, .25, 1.5), 2, 2)
vcv2 <- matrix(c(2, -.5, -.5, 2), 2, 2)
f1 <- make.mvn(mu1, vcv1)
f2 <- make.mvn(mu2, vcv2)
f <- function(x) {
  f1(x) + f2(x)}

x <- seq(-5, 6, length=71)
y <- seq(-7, 6, length=61)
xy <- expand.grid(x=x, y=y)
z <- matrix(apply(as.matrix(xy), 1, f), length(x), length(y))

image(x, y, z, las=1)
contour(x, y, z, add=TRUE)

# proposal from a uniform
q <- function(x, d=8)
  x + runif(length(x), -d/2, d/2)

x0 <- c(-4, -4)
set.seed(1)
samples <- run(x0, f, q, 1000)

image(x, y, z, xlim=range(x, samples[,1]), ylim=range(x, samples[,2]))
contour(x, y, z, add=TRUE)
lines(samples[,1], samples[,2], col="#00000088")

set.seed(1)
samples <- run(x0, f, q, 100000)
smoothScatter(samples)
contour(x, y, z, add=TRUE)

hist(samples[,1], freq=FALSE, main="", xlab="x",
     ylab="Probability density")

# 

m <- function(x1) {
  g <- Vectorize(function(x2) f(c(x1, x2)))
  integrate(g, -Inf, Inf)$value
}

xx <- seq(min(samples[,1]), max(samples[,1]), length=201)
yy <- sapply(xx, m)
z <- integrate(splinefun(xx, yy), min(xx), max(xx))$value

hist(samples[,1], freq=FALSE, main="", las=1, xlab="x",
     ylab="Probability density", ylim=c(0, 0.25))
lines(xx, yy/z, col="red")
