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
mytidy.cva.glmnet <- function(x, return_zeros = FALSE,  unnest = character(1), ...){
 #print("---- mytidy.cva.glmnet starts")
 xalpha <- x$alpha
 modlist <- x$modlist
 alphas <- as.list(1:length(xalpha))
  funi <- function(i){
    modi <- modlist[[i]]       # cv.glmnet
    fiti <- modi$glmnet.fit    # "coxnet" "glmnet"
  
    tbl1 <- tibble(alpha_idx =i, alpha = xalpha[i], myglance(fiti)) %>% 
              select(-c(family, nobs, n_colx, nulldev)) # columns not needed included in myglance
    tbl2 <- tibble(alpha_idx = i, mytidy(modi))
    tbl3 <- tibble(alpha_idx = i, mytidy(fiti, return_zeros = return_zeros, unnest = "beta", ...)) %>%
              rename(beta_hat = estimate)
    
    ret <- left_join(tbl1, tbl2, tbl3, by = "alpha_idx")
    ret
 }
 ret <- alphas %>% map_dfr(funi)          
    
 #print("---- mytidy.cva.glmnet ends")
 return(ret)
}
