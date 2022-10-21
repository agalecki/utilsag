
myglance(glmnet_fit1)  # coef by default
sel <- c(2,10)

tidy(glmnet_fit1) %>% filter(step %in% sel) %>% arrange(step)
mytidy(glmnet_fit1) %>% filter(step %in% sel)
mytidy(glmnet_fit1) %>% filter(step %in% sel) %>% unnest(coefs)


myglance(glmnet_fit3)
sel <- 10
tidy(glmnet_fit3) %>% filter(step == sel) %>% arrange(step, class)

mytidy(glmnet_fit3) %>% filter(step == sel)
mytidy(glmnet_fit3) %>% filter(step == sel) %>% 
  unnest(coefs)


myglance(glmnet_fit3a)
tidy(glmnet_fit3a) %>% arrange(step)
tidy(glmnet_fit3a) %>% arrange(step) %>% filter(step == 3)
mytidy(glmnet_fit3a)
mytidy(glmnet_fit3a) %>% filter(step == 3) 


myglance(glmnet_fit_cox)
mytidy(glmnet_fit_cox)


(myg5 <- myglance(cvglmnet_fit_cox))
mytidy(cvglmnet_fit_cox) %>% print(n=60)

pred_cox <- predict(cvglmnet_fit_cox, newx = newx_cox, lambda = myg5[,"index_1se"], type = "response")
pred_cox

save(glmnet_fit3a, file="ex1-glmnet.Rdata")



