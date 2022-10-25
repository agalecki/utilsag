#' @method myglance penAFT.cv
#' @export
myglance.penAFT.cv <- function(x){
 ret <- with(x, 
        tibble::tibble(
         alpha    = x$full.fit$alpha,
         n_lambda = length(x$full.fit$lambda),
         lambda_min = x$lambda.min,
         n_colx    =length(x$full.fit$X.mean)
       ))
  return(ret)   
}

#' @method mytidy penAFT.cv
#' @export
#' @family glmnet tidiers
#' @seealso [tidy()], [glmnet::cv.glmnet()]

mytidy.penAFT.cv <- function(x, ...) {
  ret  <- tibble(
      ## alpha  = alpha,
      step   = 1:length(x$lambda),
      lambda = x$lambda,
      estimate = x$cv.err.linPred
    )
  return(ret)
}

