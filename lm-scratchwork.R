### Basic script for playing with linear models. Will form the basis of a more
### sophisticated code base.

X <- model.matrix(~ hp + wt, data = mtcars) |> unname()
Y <- mtcars$mpg
n <- length(Y)
k <- ncol(X) - 1
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
MSE <- as.numeric(SSE/(n-k-1))
MSR <- as.numeric(SSR/k)

F_stat <- MSR/MSE
p_F <- pf(F_stat, k, n-k-1, lower.tail = FALSE)

# Distances with the Hat matrix


# Get a covariance matrix for the Betas
B_varcov <- MSE*X_t_X_inv

# get the standard errors for the estimates
SE_estimates <- sqrt(diag(B_varcov))

# Add in a homegrown VIF object

# Create R sqr and adjusted R sqr info


# Conduct hypothesis tests on the slope parameters
t_values <- B/SE_estimates
p_values <- 2*pt(abs(t_values), df=n-k-1, lower.tail = FALSE)

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
  } else {
    se <-diag(X%*%model$B_varcov%*%t(X))
  }
  
  list(
    fitted.values = X%*%model$B,
    se = se
  )
}

preds(X, model.fit, type = "prediction")

# check the math against the lm() function and its summary output.
mod.fit <- lm(mpg~hp+wt, data=mtcars)
mod.sum <- summary(mod.fit)
