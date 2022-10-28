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
mytidy.cva.glmnet <- function(x, return_zeros = FALSE, unnest = TRUE , alpha_info = FALSE,...){
 #print("---- mytidy.cva.glmnet starts")
 xalpha <- x$alpha
 modlist <- x$modlist
 alphas <- as.list(1:length(xalpha))
 
   fun_alpha <- function(i){
      modi <- modlist[[i]]       # cv.glmnet
      fiti <- modi$glmnet.fit    # "glmnet"
  
      # tbl1 contains one row per alpha (indexed by a_idx)
      tbl1_x <- tibble(a_idx =i, alpha = xalpha[i], myglance(fiti)) 
      print("fun_alpha")
      print(colnames(tbl1_x))
      tbl1_x <- tbl1_x %>% select(-c(family, nobs, n_colx, nulldev)) # redundant columns (included in `myglance`)
              
      tbl1_cv <- tibble(a_idx =i, myglance(modi)) 
      print(colnames(tbl1_cv))
      tbl1_cv <- tbl1_cv %>% select(-c( n_lambda)) # columns included in myglance
       
      left_join(tbl1_x, tbl1_cv, by = "a_idx")
   } 
     
    # tbl_cv contains one row per a_idx by (lambda) step combination  
    fun_cv <- function(i){
         modi <- modlist[[i]]       # cv.glmnet
         grpd <- tibble(a_idx = i, mytidy(modi)) %>% group_by(step)
         tbl_cv  <- grpd %>% nest(step_info = c(nzero, estimate, std.error, conf.low, conf.high))
         #print("fun_cv")
         # colnames(tbl_cv)
         tbl_cv
    } 
    
    # tbl3 contains one row per a_idx x (lambda) step combination with nested list beta
    fun_x <- function(i){
     modi <- modlist[[i]]       # cv.glmnet
     fiti <- modi$glmnet.fit    #  "glmnet" 
     tt3   <- mytidy(fiti, return_zeros = return_zeros, unnest = FALSE, ...) 
     step3 <- as.integer(tt3$step) 
     tt3   <- tt3 %>% select(-c(step, lambda))
     tbl3  <- tibble(a_idx = i, step = step3, tt3)
     # print(colnames(tbl3))
     tbl3
 }
 
    tbl_alpha <- alphas %>% map_dfr(fun_alpha)
    tbl_cv    <- alphas %>% map_dfr(fun_cv)
    if (unnest) tbl_cv <- tbl_cv %>% unnest(step)
    tbl_beta  <- alphas %>% map_dfr(fun_x)
    if (unnest) tbl_beta <- tbl_beta %>% unnest(beta)
    list(alpha_info = tbl_alpha, glmnet.cv = tbl_cv, glmnet = tbl_beta)
 #print("---- mytidy.cva.glmnet ends")
}
