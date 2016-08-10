m <- 0
s <- 1

set.seed(1)
samples <- rnorm(10000, m, s)
mean(samples)

summary(replicate(1000, mean(rnorm(10000, m, s))))

cummean <- function(x){
  cumsum(x) / seq_along(x)
}

plot(cummean(samples), type="l", xlab="Sample", ylab="Cumulative mean",
     panel.first=abline(h=0, col="red"), las=1)

plot(cummean(samples), type="l", xlab="Sample", ylab="Cumulative mean",
     panel.first=abline(h=0, col="red"), las=1, log="x")
for (i in seq_len(30))
  lines(cummean(rnorm(10000, m, s)),
        col=rgb(runif(1), runif(1), runif(1), .5))

p <- 0.025
a.true <- qnorm(p, m, s)
a.true

f <- function(x){ dnorm(x, m, s)} # direct integration
g <- function(a){
  integrate(f, -Inf, a)$value}
a.int <- uniroot(function(x) g(x) - p, c(-10, 0))$root
a.int

a.mc <- unname(quantile(samples, p)) # monte carlo integration
a.mc
a.true - a.mc #error

# markov chain
P <- rbind(c(.5,  .25, .25),
           c(.2,  .1,  .7),
           c(.25, .25, .5))
P
rowSums(P)
colSums(P)

iterate.P <- function(x, P, n) {
  res <- matrix(NA, n+1, length(x))
  res[1,] <- x
  for (i in seq_len(n))
    res[i+1,] <- x <- x %*% P
  res
}

n <- 10
y1 <- iterate.P(c(1, 0, 0), P, n)
y2 <- iterate.P(c(0, 1, 0), P, n)
y3 <- iterate.P(c(0, 0, 1), P, n)


matplot(0:n, y1, type="l", lty=1, xlab="Step", ylab="y", las=1)
matlines(0:n, y2, lty=2)
matlines(0:n, y3, lty=3)

v <- eigen(t(P), FALSE)$vectors[,1]
v <- v/sum(v) # normalise eigenvector

matplot(0:n, y1, type="l", lty=1, xlab="Step", ylab="y", las=1)
matlines(0:n, y2, lty=2)
matlines(0:n, y3, lty=3)
points(rep(10, 3), v, col=1:3)
drop(v %*% P) - v



run <- function(i, P, n) {
  res <- integer(n)
  for (t in seq_len(n))
    res[[t]] <- i <- sample(nrow(P), 1, pr=P[i,])
  res
}

samples <- run(1, P, 100)
plot(samples, type="s", xlab="Step", ylab="State", las=1)


plot(cummean(samples == 1), type="l", ylim=c(0, 1),
     xlab="Step", ylab="y", las=1)
lines(cummean(samples == 2), col=2)
lines(cummean(samples == 3), col=3)

n <- 5000
set.seed(1)
samples <- run(1, P, n)
plot(cummean(samples == 1), type="l", ylim=c(0, 1), log="x",
     xlab="Step", ylab="y", las=1)
lines(cummean(samples == 2), col=2)
lines(cummean(samples == 3), col=3)
abline(h=v, lty=2, col=1:3)
