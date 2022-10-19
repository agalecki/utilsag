
myglance(glmnet_fit1)  # coef by default
mytidy(glmnet_fit1)
mytidy(glmnet_fit1, what = "dev")

myglance(glmnet_fit3)
mytidy(glmnet_fit3)
mytidy(glmnet_fit3, what = "dev")

myglance(glmnet_fit3a)
mytidy(glmnet_fit3a)
mytidy(glmnet_fit3a, what = "dev")

myglance(glmnet_fit_cox)
mytidy(glmnet_fit_cox)
mytidy(glmnet_fit_cox, what = "dev")
save(glmnet_fit3a, file="ex1-glmnet.Rdata")



