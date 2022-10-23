#' 
#' @method myglance cva.glmnet
#' @export
myglance.cva.glmnet <- function(x){
 ret <- tibble::tibble(
    n_alpha = length(x$alpha),
    n_folds = x$nfolds,
    na_types ="cii")
return(ret)
}
