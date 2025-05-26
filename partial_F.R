rm(list=ls())
library(formula.tools)

# create two models, one with wt & hp, the other with just hp

X_full <- model.matrix(~hp+wt+disp, data = mtcars) |> unname()
X_reduced <- model.matrix(~hp+wt, data = mtcars) |> unname()

k_full <- ncol(X_full)
k_reduced <- ncol(X_reduced)

Y <- mtcars$mpg
n <- length(Y)
I <- diag(nrow=n, ncol=n)
J <- matrix(rep(1, times=(n*n)), nrow=n, ncol=n)

B_full <- solve(t(X_full)%*%X_full)%*%t(X_full)%*%Y
B_reduced <- solve(t(X_reduced)%*%X_reduced)%*%t(X_reduced)%*%Y

H_full <- X_full%*%solve(t(X_full)%*%X_full)%*%t(X_full)
H_reduced <- X_reduced%*%solve(t(X_reduced)%*%X_reduced)%*%t(X_reduced)

SSTO <- t(Y)%*%(I - 1/n*J)%*%Y

SSE_full <- t(Y)%*%(I - H_full)%*%Y
SSR_full <- SSTO - SSE_full

SSE_reduced <- t(Y)%*%(I - H_reduced)%*%Y
SSR_reduced <- SSTO - SSE_reduced

SSR_wt_hp <- SSR_full - SSR_reduced

# model comparison. Is the larger model worth it?
F_stat <- ((SSE_reduced - SSE_full)/((n-k_reduced)-(n-k_full)))/(SSE_full/(n-k_full))

AIC_full <- n*log(SSE_full) - n*log(n) + 2*k_full
AIC_reduced <- n*log(SSE_reduced) - n*log(n) + 2*k_reduced


library(latex2exp)
tibble(
  value = c(SSR_wt_hp,SSR_reduced,SSE_full),
  group = rep("SSTO", times=3),
  term = as_factor(c("SSR(X_2|X_1)", "SSR(X_1)", "SSE(X_1,X_2)"))
) |> ggplot(aes(x=group, y=value, fill=term)) + 
  geom_bar(stat = "identity", position = "fill") +
  scale_fill_discrete(labels=c(TeX("$SSR(X_2|X_1$)"), TeX("$SSR(X_1)$"), TeX("$SSE(X_1,X_2)$"))) +
  scale_fill_brewer(palette="Dark2") +
  scale_y_continuous(labels = scales::percent) +
  labs(
    fill = "Component"
  ) +
  theme_minimal()

mod.full.fit <- lm(mpg~hp+wt+disp, data=mtcars)
mod.reduced.fit <- lm(mpg~hp+wt, data=mtcars)

mod.aov <- anova(mod.reduced.fit, mod.full.fit)
