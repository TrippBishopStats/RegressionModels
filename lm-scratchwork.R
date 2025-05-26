### Basic script for playing with linear models. Will form the basis of a more
### sophisticated code base.
library(tidyverse)


X <- model.matrix(~ hp+wt, data = mtcars) |> unname()
Y <- mtcars$mpg
n <- length(Y)
k <- ncol(X)
I <- diag(nrow = n, ncol = n)
J <- matrix(rep(1, times=n^2), ncol = n)

# create useful matrices from the data that we will use repeatedly
X_t <- t(X)
X_t_X_inv <- solve(X_t%*%X)
Y_t <- t(Y)

# parameter estimates, distances, residuals
B <- X_t_X_inv%*%X_t%*%Y
H <- X%*%X_t_X_inv%*%X_t
e <- (I - H)%*%Y

## ANOVA metrics: SSTO, SSE, SSR

# The unsexy way of computing SSE, SSTO, SSR
SSE <- (t(Y)%*%Y - t(B)%*%X_t%*%Y)
SSTO <- Y_t%*%(I - 1/n*J)%*%Y
SSR <- SSTO - SSE

# all to the error terms are quadratic forms Y'AY, where A is some constant
# matrix.

SSE <- Y_t%*%(I - H)%*%Y
SSTO <- Y_t%*%(I - 1/n*J)%*%Y
SSR <- Y_t%*%(H - 1/n*J)%*%Y

# F statistics calcs
MSE <- as.numeric(SSE/(n-k))
MSR <- as.numeric(SSR/(k-1))

F_stat <- MSR/MSE
p_F <- pf(F_stat, k-1, n-k, lower.tail = FALSE)

# Distances with the Hat matrix


# Get a covariance matrix for the Betas
B_varcov <- MSE*X_t_X_inv

# get the standard errors for the estimates
SE_estimates <- sqrt(diag(B_varcov))

# Add in a homegrown VIF object

# Create R sqr and adjusted R sqr info
R_sqr <- SSR/SSTO
R_adj <- 1 - (n-1)/(n-k)*(SSE/SSTO)
AIC <- n*log(SSE) - n*log(n) + 2*k
BIC <- n*log(SSE) - n*log(n) + k*log(n)

# Conduct hypothesis tests on the slope parameters
t_values <- B/SE_estimates
p_values <- 2*pt(abs(t_values), df=n-k, lower.tail = FALSE)

df_param_est <- data.frame(
  estimate = as.vector(B),
  stderr = SE_estimates,
  t = t_values,
  p = p_values
)

model.fit <- list(
  B = B,
  MSE = MSE,
  B_varcov = MSE*X_t_X_inv
)

preds <- function(X, model, type="confidence") {
  
  if (type =="prediction") {
    se <-  MSE + diag(X%*%model$B_varcov%*%t(X))
  } else if (type =="confidence") {
    se <-diag(X%*%model$B_varcov%*%t(X))
  } else {
    se <- NULL
  }
  
  list(
    fitted.values = X%*%model$B,
    se = se
  )
}

mod.preds <- preds(X, model.fit, type = "confidence")

# check the math against the lm() function and its summary output.
mod.fit <- lm(mpg~hp+wt, data=mtcars)
mod.sum <- summary(mod.fit)

# create a plot using geom_smooth to verify that the confidence intervals around
# mean predictions are good.
tibble(
  x = X[,2],
  y = mod.preds$fitted.values,
  se.lower = mod.preds$fitted.values - 1.96*sqrt(mod.preds$se),
  se.upper = mod.preds$fitted.values + 1.96*sqrt(mod.preds$se)
) |> ggplot(aes(x,y)) + geom_point() + 
  geom_smooth(data=mtcars, aes(x=hp, y=mpg), method = "lm") +
  geom_line(aes(y=se.lower), linewidth=1, alpha=0.4, colour="red") +
  geom_line(aes(y=se.upper), linewidth=1, alpha=0.4, colour="red") + 
  theme_minimal()

            