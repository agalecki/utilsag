set.seed(1234)
library(glmnet)
# Gaussian
x = matrix(rnorm(100 * 20), 100, 20)
y = rnorm(100)
glmnet_fit1 = glmnet(x, y, alpha= 0.5)

# multinomial
g4 = sample(1:4, 100, replace = TRUE)
glmnet_fit3 = glmnet(x, g4, family = "multinomial", alpha = 0.5)
glmnet_fit3a = glmnet(x, g4, family = "multinomial", type.multinomial = "grouped")


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
tcens = rbinom(n = N, prob = 0.3, size = 1)   # censoring indicator
y_cox = cbind(time = ty, status = 1 - tcens)  # y=Surv(ty,1-tcens) with library(survival)
library(glmnet)
glmnet_fit_cox = glmnet(x_cox, y_cox, family = "cox", alpha = 0.5)
cvglmnet_fit_cox = cv.glmnet(x_cox, y_cox, family = "cox", alpha = 0.5)

newx_cox <- x_cox[1:10, ]
detach(package:glmnet)

library(utilsag)
myglance(glmnet_fit_cox)
mytidy(glmnet_fit_cox)

# glmnetUtils cva

library(glmnetUtils)

cvaglmnet_fit_cox <- cva.glmnet(x_cox, y_cox, family = "cox")

# penAFT
library(penAFT)
X <- x_cox
logY <- log(ty) 
delta <-  1 - tcens    # := status


#penAFT_object <- penAFT(X = X, logY = logY, delta = delta,
#                   nlambda = 50, lambda.ratio.min = 0.01,
#                   penalty = "EN",
#                   alpha = 0.5)

#penAFTcv_object <- penAFT.cv(X = X, logY = logY, delta = delta,
#                   nlambda = 20, lambda.ratio.min = 0.1,
#                   penalty = "EN", nfolds = 5,
#                   alpha = 0.5)
                   
#save(penAFT_object, penAFTcv_object, file = paste0(path, "/examples/penAFT_objects.Rdata"))

