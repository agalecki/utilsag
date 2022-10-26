#' 
#' @method myglance cva.glmnet
#' @export
myglance.cva.glmnet <- function(x){
 xcall <- as.list(x$call)
 mod1 <- x$modlist[[1]]
 modfit1 <- mod1$glmnet.fit
 ret <- tibble::tibble(
    n_alpha = length(x$alpha),
    n_folds = x$nfolds,
    nulldev = modfit1$nulldev,
    family  = xcall$family,
    n_colx  = modfit1$dim[1],
    nobs    = modfit1$nobs
    )
return(ret)
}

#' 
#' @method mytidy cva.glmnet
#' @export
mytidy.cva.glmnet <- function(x, return_zeros = FALSE,  unnest=FALSE, ...){
 print("---- mytidy.cva.glmnet starts")
 xalpha <- x$alpha
 modlist <- x$modlist
 #print("---- mytidy.cva.glmnet 5")
 alphas <- as.list(1:length(xalpha))
 #print("---- mytidy.cva.glmnet 7")
 #- ret1 <-  modlist %>%  map_dfr(myglance) # `myglance` applied to  `cv.glmnet` class
 funi <- function(i){
    modi <- modlist[[i]]
    fiti <- modi$glmnet.fit
    #print(paste0("i=", i, xalpha[i]))
    #print(paste0( ":", myglance(fiti)))
    tbl1 <- tibble(alpha_idx =i, alpha = xalpha[i], myglance(fiti)) %>%
           select(-c(family, nobs, n_colx, nulldev)) 
    tbl2 <- tibble(alpha_idx = i, mytidy(fiti, return_zeros = return_zeros, unnest = TRUE, ...))  
    if (i==1) {
     print(colnames(tbl1))
     print(colnames(tbl2))
    }
    # tbl2 <- tbl2 %>% group_by(alpha_idx) %>% nest(steps = c(step, lambda))
    if (i ==1)  print(colnames(tbl2))
    rturn(left_join(tbl1, tbl2, by = "alpha_idx")
    }
 ret <- alphas %>% map_dfr(funi)          
 # print("---- mytidy.cva.glmnet 11")
 # glmnetfit <- lapply(modlist, FUN = function(mod) mod$glmnet.fit)
 # print("---- mytidy.cva.glmnet 15")

 # gfit <- glmnetfit %>%  map_dfr(mytidy)
 
 
 
 print("---- mytidy.cva.glmnet ends")
 return(ret)
}
