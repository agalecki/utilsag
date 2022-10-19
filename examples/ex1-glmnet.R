
myglance(glmnet_fit1)  # coef by default
mytidy(glmnet_fit1)
mytidy(glmnet_fit1, component = "dev")

myglance(glmnet_fit3)
mytidy(glmnet_fit3)
mytidy(glmnet_fit3, component = "dev")

myglance(glmnet_fit3a)
mytidy(glmnet_fit3a)
mytidy(glmnet_fit3a, component = "dev")

tidy(glmnet_fit3a)

myglance(glmnet_fit_cox)
mytidy(glmnet_fit_cox)
mytidy(glmnet_fit_cox, component = "dev")
save(glmnet_fit3a, file="ex1-glmnet.Rdata")



