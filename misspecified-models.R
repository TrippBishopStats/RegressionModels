# exploring the effect of a missing variable on parameter estimates and
# coefficients of determination.

# create two predictors and then correlate them to varying degrees.
rm(list=ls())
library(MASS)

set.seed(42)

# Sample and fit. Predictors are uncorrelated so there is no missing predictor
# bias.
X <- mvrnorm(n=100,
             mu=c(0, 0),
             Sigma=matrix(c(3,0,0,3), ncol=2),
             empirical=TRUE)

X <- scale(X, center=TRUE, scale=TRUE)

Y <- 1.15*X[,1] + 0.4*X[,2] + rnorm(100, sd=1.25)

df <- data.frame(X,Y)

mod.fit <- lm(Y ~ X2, data=df) |> summary()

# and if we include X1 in the model now, the coefficient estimate for the slope 
# parameter for X2 does not change
lm(Y ~ X1 + X2, data=df) |> summary()

# Predictors are now moderately correlated with r=0.5
set.seed(25)
X <- mvrnorm(n=100,
             mu=c(0, 0),
             Sigma=matrix(c(3,1.5,1.5,3), ncol=2),
             empirical=TRUE)

Y <- 1.15*X[,1] + 0.4*X[,2] + rnorm(100, sd=1.25)

df <- data.frame(X,Y)

# notice now that the coefficient estimate for X2 has increased quite a bit
lm(Y ~ X2, data=df) |> summary()

# now the reveal. Both predictors are now in the model and notice how much the
# the slope for X2 changes
lm(Y ~ X1 + X2, data=df) |> summary()

################################################################################
### Now lets try with the mtcars dataset rather than simulated data.
################################################################################

cor(mtcars$hp, mtcars$wt)
cor(mtcars$hp, mtcars$mpg)

# so here, hp and wt are pretty strongly positively correlated and hp and mpg
# are strongly negatively correlated. If we treat hp as our omitted variable, we
# can see that there is a negative bias in the slope parameter estimate for wt
# in the model where wt is the only predictor.

lm(mpg ~ wt, data=mtcars) |> summary()

lm(mpg ~ wt + hp, data=mtcars) |> summary()




