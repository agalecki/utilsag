#' 
#' @method myglance cva.glmnet
#' @export
myglance.cva.glmnet <- function(x){
 callx <- call(x)
 mod1 <- x$modlist[[1]]
 modfit1 <- mod1$glmnet.fit
 ret <- tibble::tibble(
    n_alpha = length(x$alpha),
    n_folds = x$nfolds,
    family  = callx$family,
    n_colx  = modfit1$dim[1]
return(ret)
}

