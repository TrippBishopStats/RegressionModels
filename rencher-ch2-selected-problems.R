# problem 2.15

A <- matrix(c(5,4,4,2,-3,1,3,7,2), byrow=TRUE, ncol=3)
B <- matrix(c(1,0,1,0,1,0,1,2,3), byrow=TRUE, ncol=3)

det(A)
det(B)

det(A%*%B)

all.equal(det(A)*det(B), det(A%*%B))

A + B


C <- matrix(c(3,4,2,3), byrow=TRUE, ncol=2)

det(C)
C

t(C[,1, drop=FALSE])%*%C[,2,drop=FALSE]

# problem 2.19
A <- matrix(c(1,1,-2,-1,2,1,0,1,-1), byrow=TRUE, ncol=3)

eigen(A)

X <- matrix(c(2,1,-2,-1,3,-1,0,1,-1), byrow=TRUE, ncol=3)
matlib::echelon(A)

# problem 2.20
A <- matrix(c(3,1,1,1,0,2,1,2,0), byrow=TRUE, ncol=3)

C <- eigen(A)$vectors

D <- t(C)%*%A%*%C # this gives us the eigenvalues of A, as expected.

C%*%D%*%t(C) # which is A, so the spectral decomposition has worked out as expected.


#problem 2.21
A <- matrix(c(2,-1,-1,2), byrow=TRUE, ncol=2)

eigen(A)

C <- eigen(A)$vectors
D <- diag(eigen(A)$values, ncol=2) |> sqrt()

(C%*%D%*%t(C))%*%(C%*%D%*%t(C))

C%*%D
1/sqrt(6)
1/2 - sqrt(3)/2


# problem 2.22
A <- matrix(c(3,6,-1,6,9,4,-1,4,3), byrow=TRUE, ncol=3)

eig_decomp_A <- eigen(A)

A2 <- A%*%A

eig_decomp_A2 <- eigen(A2)

D <- diag(eig_decomp_A$value, ncol=3)
D2 <- diag(eig_decomp_A2$value, ncol=3)

D^2
D2

# is CD^2C equal to just squaring A?
B <- eig_decomp_A$vectors%*%D^2%*%t(eig_decomp_A$vectors)

# yes, yes it is. And if A is large or we have to take higher powers, this
# spectral decomposition approach is increasing more efficient at this kind of
# operation.
all.equal(A2, B)

# problem 2.23

A <- matrix(c(4,-5,-1,7,-2,3,-1,4,-3,8,2,6), byrow=TRUE, ncol=3)

eig_decomp_U <- eigen(A%*%t(A))
eig_decomp_V <- eigen(t(A)%*%A)

# there are no zero valued eigenvalues for A'A, so all k=3 for U and V
U <- eig_decomp_U$vectors[,1:3, drop=FALSE]
V <- eig_decomp_V$vectors

D <- diag(sqrt(eig_decomp_V$values), ncol=length(eig_decomp_V$values))
D
B <- U%*%D%*%t(V)
all.equal(A,B)

sv_decomp <- svd(A)

all.equal(U, sv_decomp$u)
U

# the eigenvectors of U are all different to what is computed by the svd
# function by a factor of -1. The eigenvectors of V are mixed. columns 1 and 3
# are different by a factor of -1 but 2 is the same. If I modify the vectors
# computed by the eigen function, then B <- U%*%D%*%t(V) actually gives the
# correct result.

U <- -1*U

V <- cbind(-1*V[,1,drop=FALSE],V[,2,drop=FALSE],-1*V[,3,drop=FALSE])

sv_decomp$u

V
sv_decomp$v
all.equal(V, sv_decomp$v)

B
t(U)%*%U
t(V)%*%V


s1 <- U[,1, drop=FALSE]%*%t(V)[1,,drop=FALSE]
s2 <- U[,2, drop=FALSE]%*%t(V)[2,,drop=FALSE]
s3 <- U[,3, drop=FALSE]%*%t(V)[3,,drop=FALSE]

matlib::echelon(s1)
matlib::echelon(s1+s2)
matlib::echelon(s1+s2+s3)

sv_decomp$u

t(sv_decomp$u)%*%sv_decomp$u


sv_decomp$u%*%diag(sv_decomp$d,ncol=length(sv_decomp$d)) %*%t(sv_decomp$v)

# Problem 2.24

A <- matrix(c(1,1,-2,-1,2,1,0,1,-1), byrow=TRUE, ncol=3)

column_means <- function(A) {
  n <- nrow(A)
  j <- matrix(rep(1,times=n), nrow=1)
  as.vector(j%*%A/n)
}

row_means <- function(A) {
  n <- ncol(A)
  j <- matrix(rep(1,times=n), ncol=1)
  as.vector(A%*%j/n)
}

column_means(A)
colMeans(A)

row_means(A)
rowMeans(A)
