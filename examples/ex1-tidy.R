library(broom)
library(tidyr)
library(dplyr)

### glmnet_fit1
message("--- glmnet_fit1 ---")
str(glmnet_fit1)
glance(glmnet_fit1)
stepx <- c(2,10) # Select subset of `step` vector
tidy(glmnet_fit1) %>% filter(step %in% stepx) %>% arrange(step)

### glmnet_fit3
message("--- glmnet_fit3 ---")
stepx <- 5
str(glmnet_fit3)
glance(glmnet_fit3)
tidy(glmnet_fit3) %>% filter(step %in% stepx) %>% arrange(step, class)

### glmnet_fit3a
message("--- glmnet_fit3a ---")
stepx <- 3
str(glmnet_fit3a)
glance(glmnet_fit3a)
tidy(glmnet_fit3a) %>% filter(step %in% stepx) %>% arrange(step, class)

### glmnet_fit_cox
message("--- glmnet_fit_cox ---")
stepx  <- c(3,5)
str(glmnet_fit_cox)
glance(glmnet_fit_cox)

tidy(glmnet_fit_cox)  %>% filter(step %in% stepx) %>% arrange(step)

### cvglmnet_fit_cox
message("--- cvglmnet_fit_cox ---")

stepx <- c(3,5)
str(cvglmnet_fit_cox)

(g5 <- glance(cvglmnet_fit_cox))

# tidy(cvglmnet_fit_cox) 
tidy(cvglmnet_fit_cox) %>% mutate(row_id = row_number()) %>% slice(stepx) 


detach(package:dplyr)
detach(package:tidyr)
detach(package:broom)
