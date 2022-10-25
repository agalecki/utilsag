# Not exported funcs

call_alpha <- function (x){
 xcall <- as.list(x$call)
 alpha <- xcall$alpha
 if (is.null(alpha)) alpha=1
return(alpha)
}


#' 
#' @method myglance glmnet
#' @export
myglance.glmnet <- function(x){
 ret0 <- broom::glance(x)
 ret1 <- with(x, 
          tibble::tibble(
            family   = family(x), 
            n_lambda = length(lambda),
            alpha    = call_alpha(x),
            n_colx   = dim[1]
           ))
return(dplyr::bind_cols(ret0,ret1))
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
#' @importFrom tibble tibble
#' @importFrom magrittr `%>%`
#' @importFrom broom glance tidy
#' @importFrom dplyr select group_by arrange
#' @importFrom tidyr nest
#' @method mytidy glmnet
#' @export

mytidy.glmnet <- function(x, return_zeros = FALSE, ...) {
 step <- 1:length(x$lambda)
 step_df <- tibble::tibble(step = step)
 dev <- tibble::tibble( alpha = call_alpha(x), 
                step = step,
                lambda = x$lambda, 
                dev.ratio = x$dev.ratio,
                df = x$df
              )
   betas  <- broom::tidy(x, return_zeros = return_zeros, ...) %>%
       select(-dev.ratio, -lambda)
       grpd   <- dplyr::left_join(step_df, betas, by = "step") %>%  group_by(step) 
       ret    <-  grpd %>% nest(beta = c(term, estimate))
   if (inherits(x, "multnet")){
       ret <- grpd %>%  arrange(class) %>% 
                 nest(beta = c(term, estimate))
   }
   retx <- dplyr::left_join(dev, ret, by = "step") 
 return(retx)
}

