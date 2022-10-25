#' 
#' @method myglance cva.glmnet
#' @export
myglance.cva.glmnet <- function(x){
 xcall <- x$call
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
 modlist <- x$modlist
 alpha <- tibble::tibble(alpha = x$alpha)
 ret1 <-  modlist %>%  map_dfc(glance)
 
 return(ret1)
}
