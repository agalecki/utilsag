#' @method myglance penAFT.cv
#' @export
myglance.penAFT.cv <- function(x){
 ffit <- x$full.fit
 ret <- with(x, 
        tibble::tibble(
         alpha    = ffit$alpha,
         n_lambda = length(ffit$lambda),
         lambda_min = x$lambda.min,
         n_colx     = length(ffit$X.mean)
       ))
  return(ret)   
}

#' @method mytidy penAFT.cv
#' @export
#' @family glmnet tidiers
#' @seealso [tidy()], [glmnet::cv.glmnet()]

mytidy.penAFT.cv <- function(x, ...) {
  ffit <- x$full.fit
  ret  <- tibble(
      ## alpha  = alpha,
      step   = 1:len),
      lambda = ffit$lambda,
      estimate = x$cv.err.linPred
    )
  return(ret)
}

