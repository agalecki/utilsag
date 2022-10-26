#' -@ templateVar class cv.glmnet
#' -@ template title_desc_tidy
#'- 
#' @param x A `cv.glmnet` object returned from [glmnet::cv.glmnet()].
#' - @ template param_unused_dots
#'
#' @evalRd broom:::return_tidy(
#'   "lambda",
#'   "std.error",
#'   "nzero",
#'   conf.low = "Lower bound on confidence interval for cross-validation
#'     estimated loss.",
#'   conf.high = "Upper bound on confidence interval for cross-validation
#'     estimated loss.",
#'   estimate = "Median loss across all cross-validation folds for a given
#'     lamdba"
#' )
#'
#' @examplesIf rlang::is_installed(c("glmnet", "ggplot2"))
#' 
#' # load libraries for models and data
#' library(glmnet)
#' 
#' set.seed(27)
#'
#' nobs <- 100
#' nvar <- 50
#' real <- 5
#'
#' x <- matrix(rnorm(nobs * nvar), nobs, nvar)
#' beta <- c(rnorm(real, 0, 1), rep(0, nvar - real))
#' y <- c(t(beta) %*% t(x)) + rnorm(nvar, sd = 3)
#'
#' cvfit1 <- cv.glmnet(x, y)
#'
#' mytidy(cvfit1)
#' glance(cvfit1)
#'
#' library(ggplot2)
#' 
#' tidied_cv <- tidy(cvfit1)
#' glance_cv <- glance(cvfit1)
#'
#' # plot of MSE as a function of lambda
#' g <- ggplot(tidied_cv, aes(lambda, estimate)) +
#'   geom_line() +
#'   scale_x_log10()
#' g
#'
#' # plot of MSE as a function of lambda with confidence ribbon
#' g <- g + geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .25)
#' g
#'
#' # plot of MSE as a function of lambda with confidence ribbon and choices
#' # of minimum lambda marked
#' g <- g +
#'   geom_vline(xintercept = glance_cv$lambda.min) +
#'   geom_vline(xintercept = glance_cv$lambda.1se, lty = 2)
#' g
#'
#' # plot of number of zeros for each choice of lambda
#' ggplot(tidied_cv, aes(lambda, nzero)) +
#'   geom_line() +
#'   scale_x_log10()
#'
#' # coefficient plot with min lambda shown
#' tidied <- tidy(cvfit1$glmnet.fit)
#'
#' 
#' @method mytidy cv.glmnet
#' @export
# -' @family glmnet tidiers
#' @seealso [tidy()], [glmnet::cv.glmnet()]
mytidy.cv.glmnet <- function(x, ...) {
  with(
    x,
    tibble::tibble(
      ## alpha  = call_alpha(x),
      step   = 1:length(lambda),
      lambda = lambda,
      estimate = cvm,
      std.error = cvsd,
      conf.low = cvlo,
      conf.high = cvup,
      nzero = nzero
    )
  )
}

#' - @ templateVar class cv.glmnet
#' - @ template title_desc_glance
#'
#' - @inherit tidy.cv.glmnet params examples
#'
# -' @evalRd broom::return_glance("alpha", "lambda.min", "lambda.1se", "nobs", "family")
#'
#' @method myglance cv.glmnet
#' @export
#' @seealso [glance()], [glmnet::cv.glmnet()]
#' @family glmnet tidiers
myglance.cv.glmnet <- function(x, ...) {
  print("---> myglance.cv.glmnet starts")
  a <- call_alpha(x)
  ncolx <- x$glmnet.fit$dim[1]
  ret0 <- broom::glance(x, ...)
  ret <- ret0 %>% mutate(alpha=a, family = family(x),
     index_min = x$index[1], index_1se = x$index[2],
     n_lambda = length(x$lambda),
     n_colx  = ncolx
     ) %>% relocate(alpha)
     print("---> myglance.cv.glmnet ends")
  
  return(ret)
 }
