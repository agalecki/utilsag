### glmnet_fit1
str(glmnet_fit1)
myglance(glmnet_fit1)  # coef by default
sel <- c(2,10)

tidy(glmnet_fit1) %>% filter(step %in% sel) %>% arrange(step)
mytidy(glmnet_fit1) %>% filter(step %in% sel)
mytidy(glmnet_fit1) %>% filter(step %in% sel) %>% unnest(beta)

### glmnet_fit3
sel <- 5
myglance(glmnet_fit3)
tidy(glmnet_fit3) %>% filter(step == sel) %>% arrange(step, class)
mytidy(glmnet_fit3) %>% filter(step == sel)
mytidy(glmnet_fit3) %>% filter(step == sel) %>% 
   unnest(beta)

### glmnet_fit3a
sel <- 3
myglance(glmnet_fit3a)
tidy(glmnet_fit3a) %>% filter(step == sel) %>% arrange(step, class)
mytidy(glmnet_fit3a) %>% filter(step == sel) 
mytidy(glmnet_fit3a) %>% filter(step == sel) %>% 
   unnest(beta)

### glmnet_fit_cox

sel <- c(3,5)
myglance(glmnet_fit_cox)

tidy(glmnet_fit_cox)  %>% filter(step == sel) %>% arrange(step)
mytidy(glmnet_fit_cox) %>% filter(step == sel)

### cvglmnet_fit_cox
sel <- c(3,5)
(myg5 <- myglance(cvglmnet_fit_cox))
mytidy(cvglmnet_fit_cox) %>% filter(step == sel)

pred_cox <- predict(cvglmnet_fit_cox, newx = newx_cox, lambda = myg5[,"index_1se"], type = "response")
pred_cox

save(glmnet_fit3a, file="ex1-glmnet.Rdata")



