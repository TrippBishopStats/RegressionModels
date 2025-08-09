# Orthogonal matrix properties

# implement the Gram-Schmidt algorithm for producing an orthonormal matrix.
# ideal case error handling
# matrix is square with at least 2 columns
# det(A) != 0
gram_schmidt <- function (A, normalise=TRUE) {
  u1 <- matrix(A[,1], ncol=1)
  n <- ncol(A)
  
  if(normalise) {
    u1 <- u1/sqrt(as.numeric(t(u1)%*%u1))
  }
  orthogonal_basis <- cbind(u1)
  
  for(i in 2:n) {
    p <- ncol(orthogonal_basis)
    v <- matrix(A[,i], ncol=1)
    u <- v
    for(j in 1:p) {
      u_j <- matrix(orthogonal_basis[,j], ncol=1)
      a <- as.numeric((t(v)%*%u_j)/(t(u_j)%*%u_j))
      u <- u - a*u_j
    }
    if(normalise) {
      #normalise the new basis vector and store in the new basis matrix
      u <- u/sqrt(as.numeric(t(u)%*%u))
    }
    # update the orthogonal matrix with the latest column
    orthogonal_basis <- cbind(orthogonal_basis,u)
  }
  return(orthogonal_basis)
}

vec_norm <- function(x) {
  sqrt(as.numeric(t(x)%*%x))
}

A <- matrix(c(5,4,4,2,-3,1,3,7,9), byrow=TRUE, ncol=3)
B <- gram_schmidt(A)
C <- matlib::GramSchmidt(A, normalize=TRUE)

# compare my algorithm with the one from matlib
all.equal(B,C)

# orthogonal matrices preserve length under a rotation, so they are also called
# rotation matrices.

# given a vector x, compute its length and then transform it with the matrix B.
x <- c(4,5,1)
vec_norm(x)

z <- B%*%x
# check the length of the vector after the linear transformation.
vec_norm(z)

# notice how this vector is considerable longer after linear transformation by
# the original matrix A
w <- A%*%x
vec_norm(w)

# the determinant of an orthogonal matrix is either 1 or -1
det(B)
det(A)


eigs <- eigen(A)

C <- eigs$vectors
N <- gram_schmidt(C)
D <- diag(eigs$values, ncol=length(eigs$values))

G <- N%*%D%*%t(N)

all.equal(A,G)

t(N)%*%A%*%N


all.equal(t(C)%*%A%*%C, D)

df <- penguins[,3:5] |> na.omit()
A <- as.matrix(df)
A <- t(A)%*%A

eigs <- eigen(A)

C <- eigs$vectors

D <- t(C)%*%A%*%C
D

# the spectral decomposition makes it very easy to compute the determinant of
# the original matrix. For large matrices, this is probably very useful.
prod(eigs$values)

det(A)

A <- as.matrix(penguins[,3:6])

B <- t(A)%*%A

det(A)

