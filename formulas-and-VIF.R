rm(list=ls())

library(formula.tools)
library(stringi)

f <- formula(mpg ~ hp + wt + disp)

rhs_vars <- formula.tools::rhs.vars(f)
n <- length(rhs_vars)
vifs <- numeric(length=n)
names(vifs) <- rhs_vars
for (i in 1:n) {
  pred <- rhs_vars[i]
  new_preds <- rhs_vars[rhs_vars!=pred]
  rhs <- paste(new_preds, collapse = "+")
  lm_formula <- as.formula(paste(pred, "~", rhs))
  mod.fit <- lm(lm_formula, data=mtcars)
  mod.sum <- summary(mod.fit)
  vifs[i] <- 1/(1-mod.sum$r.squared)
}

mod.fit <- lm(hp ~ wt+drat, data=mtcars)
mod.sum <- summary(mod.fit)

mod.sum$r.squared

1/(1-mod.sum$r.squared)

design_formula <- f
X <- model.matrix(f, data = mtcars)
cor(mtcars$hp, mtcars$wt)^2

