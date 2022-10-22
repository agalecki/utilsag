#' 
#' @method myglance penAFT
#' @export
myglance.penAFT <- function(x){
   with(x, 
        tibble::tibble(
         alpha    = alpha,
         n_lambda = length(lambda)
       ))
}
        

#' -@templateVar class penAFT
#'
#' -@template title_desc_tidy
#'
#' @param x A `penAFT` object returned from [penAFT::penAFT()].
#' @param `return_zeros` Logical indicating whether coefficients with value zero
#'   zero should be included in the results. Defaults to `FALSE`.
#' - @template param_unused_dots
#'
#' @evalRd broom:::return_tidy(
#'   "step",
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
#' @method mytidy penAFT
#' @export

mytidy.penAFT <- function(x, return_zeros = FALSE, ...) {
 step <- as.integer(1:length(x$lambda))
 step_df <- tibble(step = step)
 dev <- tibble( alpha = x$alpha, 
                step = step,
                lambda = x$lambda, 
               )
   beta1 <- penAFT.coef(x, lambda= x$lambda)
   betax  <- beta1$beta
   colnames(betax) <- step
   beta_df <- as_tibble(betax)
   beta_df2 <- bind_cols(term = paste0("X", 1:length(x$X.mean)), beta_df)
   betas  <- pivot_longer(beta_df2, cols = c(dplyr::everything(), 
               -term), names_to = "step", values_to = "estimate") %>%
                mutate(step = as.integer(step))
   if (!return_zeros)  betas <- filter(betas, estimate != 0)
   grpd   <- left_join(step_df, betas, by = "step") %>%  group_by(step) 
   retx <-  grpd %>% nest(beta = c(term, estimate))
   ret <- left_join(dev, retx, by = "step") 
 return(ret)
}

#' @method myglance penAFT.cv
#' @export
myglance.penAFT <- function(x){
 ret1 <- with(x, 
        tibble::tibble(
         alpha    = alpha,
         n_lambda = length(lambda),
         ncolx    = length(X.mean)
       ))
}


