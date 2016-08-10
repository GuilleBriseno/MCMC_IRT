# MCMC example
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

set.seed(1)
plot(cummean(samples), type="l", xlab="Sample", ylab="Cumulative mean",
     panel.first=abline(h=0, col="red"), las=1, log="x")
for (i in seq_len(30))
  lines(cummean(rnorm(10000, m, s)),
        col=rgb(runif(1), runif(1), runif(1), .5))

