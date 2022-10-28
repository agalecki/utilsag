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
mytidy.cva.glmnet <- function(x, return_zeros = FALSE, unnest = FALSE, alpha_info = FALSE,...){
 #print("---- mytidy.cva.glmnet starts")
 xalpha <- x$alpha
 modlist <- x$modlist
 alphas <- as.list(1:length(xalpha))
  funi <- function(i){
    modi <- modlist[[i]]       # cv.glmnet
    fiti <- modi$glmnet.fit    # "coxnet" "glmnet"
  
    # tbl1 contains one row per alpha (indexed by a_idx)
    tbl1x <- tibble(a_idx =i, alpha = xalpha[i], myglance(fiti)) %>% 
              select(-c(family, nobs, n_colx, nulldev)) # columns not needed included in myglance
              
    tbl1_cv <- tibble(a_idx =i, alpha = xalpha[i], myglance(modi)) # %>% 
                 # select(-c(family, nobs, n_colx, nulldev)) # columns not needed included in myglance
    colnames(tbl1x)
    colnames(tbl1_cv)
    tbl1 <-   full_join(tbl1x, tbl1_cv, by = "a_idx")          
    
    # tbl2 contains one row per a_idx by (lambda) step combination  
     
     grpd <- tibble(a_idx = i, mytidy(modi)) %>% group_by(step)
     tbl2  <- grpd %>% nest(step_info = c(nzero, estimate, std.error, conf.low, conf.high))
     #print(colnames(tbl2))
    
    # tbl3 contains one row per a_idx x (lambda) step combination with nested list beta
    
     tt3   <- mytidy(fiti, return_zeros = return_zeros, unnest = FALSE, ...) 
     step3 <- as.integer(tt3$step) 
     tt3   <- tt3 %>% select(-c(step, lambda))
     tbl3  <- tibble(a_idx = i, step = step3, tt3)
     # print(colnames(tbl3))
    ret1 <- left_join(tbl1, tbl2, by = "a_idx") 
    ret  <- left_join(ret1, tbl3, by = c("a_idx", "step"))
    ret
 }
 ret <- alphas %>% map_dfr(funi)
 if (unnest) ret <- unnest("beta", "step_info")
    
 #print("---- mytidy.cva.glmnet ends")
 return(ret)
}
