tidyx.cva.glmnet <- function(x, extract = c("cva", "mod", "cva.summ", "mod.summ"), return_zeros = FALSE, ...){
  # estimate := cvm
  # 
  extract <- match.arg(extract)
  alpha   <- x$alpha   # vector
  n <- length(alpha)  
  modlist <- x$modlist
  dtlist <- vector("list", length = n)
   for (i in 1:n) {
       modi <- modlist[[i]]      # cv.glmnet object
       dti <- switch (extract,
                 cva  = tidy(modi),        # tidy cv.glmnet object
                 mod = tidy(modi$glmnet.fit, return_zeros = return_zeros),  ## tidy glmnet object
                 cva.summ = glance(modi),
                 mod.summ = glance(modi$glmnet.fit)
                 )
       dti$alpha  <- alpha[i] 
       if (extract == "cva") dti <- dti %>% mutate(step = 1:nrow(dti)) 
       dtlist[[i]] <- dti
     }
     
     ret <- dplyr::bind_rows(dtlist) %>% relocate (alpha, lambda) %>% 
        relocate(step, lambda, .after = alpha) %>% arrange(alpha, step)                    
 return(as_tibble(ret))
}
