#' @method myglance penAFT.cv
#' @export
myglance.penAFT.cv <- function(x){
 a <- x$alpha
 ret <- with(x, 
        tibble::tibble(
         alpha    = a,
         n_lambda = length(lambda),
         ncolx    = length(X.mean)
       ))
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
  
}

