rm(list=ls())

X <- c(1,3,2,4)
Y <- c(2,-1,3,1)

cov(X,Y)

B <- cbind(X,Y)

t(B)%*%B

data <- data.frame(math = c(84, 82, 81, 89, 73, 94, 92, 70, 88, 95),
                   science = c(85, 82, 72, 77, 75, 89, 95, 84, 77, 94),
                   history = c(97, 94, 93, 95, 88, 82, 78, 84, 69, 78))

D <- as.matrix(data)
n <- nrow(data)

means <- rep(1, times=n)%*%t(colMeans(D))
# centre the observations
D <- D - means

# now compute the var/covar matrix
covar <- t(D)%*%D/(n-1)


x <- c(-3,0,-1)


t(x)%*%covar%*%x



X%*%t(X)

Y%*%t(Y)

# playing with the trace of matrices

A <- matrix(c(1,3,2,-1,4,6), byrow=TRUE, ncol=2)

B <- t(A)%*%A
C <- A%*%t(A)

C

A <- matrix(c(3,4,7,2), byrow=TRUE, ncol=2)

A%*%t(A)
sum(diag(A%*%t(A)))
sum(diag(t(A)%*%A))


B <- matrix(c(3,-2,4,7,1,0,2,3,5), byrow=TRUE, ncol=3)

B_c <- B - matrix(c(1,1,1), ncol=1)%*%colMeans(B)

t(B_c)%*%B_c/2


cov(B)

B

matrix(c(1,1,1), ncol=1)%*%matrix(c(2,-1,3),nrow=1)

# correlation is invariant under a linear transformation

cov(data$math, data$science)
cor(data$math, data$science)

X1 <- 0.5*data$math + 27
X2 <- 0.8*data$science - 11

cov(X1, X2)
cor(X1, X2)
