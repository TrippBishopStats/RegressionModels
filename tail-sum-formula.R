# implementation of the tail sum formula. Create an upper triangular matrix that
# contains all of the probabilities for values greater than 0.

rm(list=ls())
X <- 1:15 # the support of X
n <- length(X) # the number of elements in the support
p <- 0.15
# the probability for each element in the support
P <- dbinom(X, size=n, prob=p)

# next, create the upper triangular matrix
U <- matrix(0, nrow=n, ncol=n)

for(i in X) {
  U[i,i:n] <- P[i:n]
}

# now, we use a J vector that will be used in the sum for each row. We're taking
# the dot product of each row of probabilities with the J vector to create the
# cumulative probabitlity in each row.
J <- matrix(1, ncol=1, nrow=n)

# the product of the row probabilities and the J vector returns an nx1 vector
# where each element is the sum of the probabilities in each row. Sum them all
# up to get the expected value.
mu <- t(J)%*%U%*%J |> as.numeric() # convert to a scalar

# for a binomial distribution, the expected value is simply n*p, so we can
# compare our computed expectation with np to see if the algorithm works.
all.equal(mu, n*p)
