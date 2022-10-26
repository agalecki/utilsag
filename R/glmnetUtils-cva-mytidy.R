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
    family  = xcall$family,
    n_colx  = modfit1$dim[1],
    nobs    = modfit1$nobs
    )
return(ret)
}

#' 
#' @method mytidy cva.glmnet
#' @export
mytidy.cva.glmnet <- function(x){
 print("---- mytidy.cva.glmnet starts")
 modlist <- x$modlist
 print("---- mytidy.cva.glmnet 5")
 alphav <- tibble::tibble(alpha = x$alpha)
 print("---- mytidy.cva.glmnet 7")
 ret1 <-  modlist %>%  map_dfr(myglance) # `myglance` applied to  `cv.glmnet` class
 print("---- mytidy.cva.glmnet 11")
 glmnetfit <- lapply(modlist, FUN = function(mod) mod$glmnet.fit)
 print("---- mytidy.cva.glmnet 15")

 gfit <- glmnetfit %>%  map_dfr(mytidy)
 print("---- mytidy.cva.glmnet ends")
 return(gfit)
}
