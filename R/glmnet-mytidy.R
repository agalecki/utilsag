#' 
#' @method myglance glmnet
#' @export
myglance.glmnet <- function(x){
 xcall <- as.list(x$call)
 alpha <- xcall$alpha
 if (is.null(alpha)) alpha=1
 oclass <- paste0(class(x), collapse = ",")
 fam   <-  xcall$family
 if (is.null(fam)) fam <- "gaussian"
 ret0 <- glance(x)
 ret <- ret0 %>% 
    mutate(family = fam, n_lambda = length(x$lambda), alpha = alpha, oclass = oclass)
return(ret)
}

#' -@templateVar class glmnet
#'
#' -@template title_desc_tidy
#'
#' @param x A `glmnet` object returned from [glmnet::glmnet()].
#' @param `return_zeros` Logical indicating whether coefficients with value zero
#'   zero should be included in the results. Defaults to `FALSE`.
#' - @template param_unused_dots
#'
#' @evalRd broom:::return_tidy(
#'   "step",
#'   step.label = "Label used by `glmnet`. It refers to column name in a matrix of coefficients. See example.",
#'   "term",
#'   "estimate",
#'   "lambda",
#'   "dev.ratio",
#'   df.step  = "The number of nonzero coefficients for each value of lambda. For multnet, the number
#'      of variables with a nonzero coefficient for any class."
#' )
#'
#' @details Note that while this representation of GLMs is much easier
#'   to plot and combine than the default structure, it is also much
#'   more memory-intensive. Do not use for large, sparse matrices.
#'
#'   No `augment` method is yet provided even though the model produces
#'   predictions, because the input data is not tidy (it is a matrix that
#'   may be very wide) and therefore combining predictions with it is not
#'   logical. Furthermore, predictions make sense only with a specific
#'   choice of lambda.
#' 
#' @method mytidy glmnet
#' @export
mytidy.glmnet <- function(x, return_zeros = FALSE, ...) {
 ret <- tidy(x, return_zeros = return_zeros) %>% 
         mutate(sx = as.character(step-1), step.lambda = paste0("s", sx)) %>% 
         select(-sx) %>%
         group_by(step)
 return(ret)
}
# mytidy(fit)