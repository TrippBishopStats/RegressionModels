library(palmerpenguins)
library(tidyverse)

rm(list=ls())

df <- penguins |> 
  select(ends_with("mm"), ends_with("g")) |> 
  na.omit()

# compute mean row matrix
col_means <- colMeans(df)
n <- nrow(df)
means <- rep(1,times=n)%*%t(col_means)

# create a mean centred version of data
B <- as.matrix(df - means)

# create the covariance matrix of the mean centred data
C <- t(B)%*%B

# determine the eigenvectors and eigenvalues of the covariance matrix
eigs <- eigen(C)

# Determine the principal components
P <- B%*%eigs$vectors


# determine the percentage of the variance accounted for by each principal 
# component.