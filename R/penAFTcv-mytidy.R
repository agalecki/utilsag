#' @method myglance penAFT.cv
#' @export
myglance.penAFT.cv <- function(x){
 ret1 <- with(x, 
        tibble::tibble(
         alpha    = alpha,
         n_lambda = length(lambda),
         ncolx    = length(X.mean)
       ))
}

#' @method mytidy penAFT.cv
#' @export
#' @family glmnet tidiers
#' @seealso [tidy()], [glmnet::cv.glmnet()]

mytidy.penAFT.cv <- function(x, ...) {
  with(
    x,
    tibble(
      alpha  = alpha,
      step   = 1:length(lambda),
      lambda_min = lambda.min,
      estimate = cv.err.linPred
    )
  )
}

