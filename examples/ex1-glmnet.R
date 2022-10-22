### glmnet_fit1
str(glmnet_fit1)
myglance(glmnet_fit1)  # coef by default

stepx <- c(2,10) # Select subset of `step` vector

tidy(glmnet_fit1) %>% filter(step %in% stepx) %>% arrange(step)
mytidy(glmnet_fit1) %>% filter(step %in% stepx)
mytidy(glmnet_fit1) %>% filter(step %in% stepx) %>% unnest(beta)

### glmnet_fit3
stepx <- 5
myglance(glmnet_fit3)
tidy(glmnet_fit3) %>% filter(step %in% stepx) %>% arrange(step, class)
mytidy(glmnet_fit3) %>% filter(step %in% stepx)
mytidy(glmnet_fit3) %>% filter(step %in% stepx) %>% 
   unnest(beta)

### glmnet_fit3a
stepx <- 3
myglance(glmnet_fit3a)
tidy(glmnet_fit3a) %>% filter(step %in% stepx) %>% arrange(step, class)
mytidy(glmnet_fit3a) %>% filter(step %in% stepx) 
mytidy(glmnet_fit3a) %>% filter(step %in% stepx) %>% 
   unnest(beta)

### glmnet_fit_cox

stepx <- c(3,5)
myglance(glmnet_fit_cox)

tidy(glmnet_fit_cox)  %>% filter(step %in% stepx) %>% arrange(step)
mytidy(glmnet_fit_cox) %>% filter(step %in% stepx)

### cvglmnet_fit_cox
stepx <- c(3,5)
(myg5 <- myglance(cvglmnet_fit_cox))

tidy(cvglmnet_fit_cox)
## tidy(cvglmnet_fit_cox)  %>% filter(step %in% stepx) %>% arrange(step)
mytidy(cvglmnet_fit_cox) %>% filter(step %in% stepx)

pred_cox <- predict(cvglmnet_fit_cox, newx = newx_cox, lambda = myg5[,"index_1se"], type = "response")
pred_cox

## save(glmnet_fit3a, file="ex1-glmnet.Rdata")



