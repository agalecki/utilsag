
myglance(glmnet_fit1)  # coef by default
mytidy(glmnet_fit1)

myglance(glmnet_fit3)
mytidy(glmnet_fit3)

myglance(glmnet_fit3a)
mytidy(glmnet_fit3a)

tidy(glmnet_fit3a)

myglance(glmnet_fit_cox)
mytidy(glmnet_fit_cox)


(myg5 <- myglance(cvglmnet_fit_cox))
mytidy(cvglmnet_fit_cox) %>% print(n=60)

pred_cox <- predict(cvglmnet_fit_cox, newx = newx_cox, lambda = myg5[,"index_1se"], type = "response")
pred_cox

save(glmnet_fit3a, file="ex1-glmnet.Rdata")



