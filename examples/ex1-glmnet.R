rm(list = ls())

library(glmnet)
# Gaussian
x = matrix(rnorm(100 * 20), 100, 20)
y = rnorm(100)
fit1 = glmnet(x, y)

# multinomial
g4 = sample(1:4, 100, replace = TRUE)
fit3 = glmnet(x, g4, family = "multinomial", alpha = 0.5)
fit3a = glmnet(x, g4, family = "multinomial", type.multinomial = "grouped")


# Cox
set.seed(10101)
N = 1000
p = 30
nzc = p/3
x_cox = matrix(rnorm(N * p), N, p)
beta = rnorm(nzc)
fx = x_cox[, seq(nzc)] %*% beta/3
hx = exp(fx)
ty = rexp(N, hx)
tcens = rbinom(n = N, prob = 0.3, size = 1)  # censoring indicator
y_cox = cbind(time = ty, status = 1 - tcens)  # y=Surv(ty,1-tcens) with library(survival)
fit_cox = glmnet(x_cox, y_cox, family = "cox", alpha = 0.5)

myglance(fit1)  # coef by default
mytidy(fit1)
mytidy(fit1, what = "dev")

myglance(fit3)
mytidy(fit3)
mytidy(fit3, what = "dev")

myglance(fit3a)
mytidy(fit3a)
mytidy(fit3a, what = "dev")

myglance(fit_cox)
mytidy(fit_cox)
mytidy(fit_cox, what = "dev")




