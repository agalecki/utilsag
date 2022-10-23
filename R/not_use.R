#-' 
#- ' @method myglance cva.glmnet
#--'  @export
myglance_cva_glmnet <- function(x){
 ret <- broom:::as_glance_tibble(
    oclass  = paste0(class(x), collapse =" "),
    n_alpha = length(x$alpha),
    n_folds = x$nfolds,
    na_types ="cii")
return(ret)
}

#- ' -@templateVar class glmnet
#- '
#- ' -@template title_desc_tidy
#- '
#- ' @param x A `cva.glmnet` object returned from [glmnetUtils::cva.glmnet()].
#- ' @param `return_zeros` Logical indicating whether coefficients with value zero
#- '   zero should be included in the results. Defaults to `FALSE`.
#- ' - @template param_unused_dots
#- '
#- ' @evalRd broom:::return_tidy(
#- '   "step",
#- '   step.label = "Label used by `glmnet`. It refers to column name in a matrix of coefficients. See example.",
#- '   "term",
#- '   "estimate",
#- '   "lambda",
#- '   "dev.ratio",
#- '   df.step  = "The number of nonzero coefficients for each value of lambda. For multnet, the number
#- '      of variables with a nonzero coefficient for any class."
#- ' )
#- '
#- ' @details Note that while this representation of GLMs is much easier
#- '   to plot and combine than the default structure, it is also much
#- '   more memory-intensive. Do not use for large, sparse matrices.
#- '
#- '   No `augment` method is yet provided even though the model produces
#- '   predictions, because the input data is not tidy (it is a matrix that
#- '   may be very wide) and therefore combining predictions with it is not
#- '   logical. Furthermore, predictions make sense only with a specific
#- '   choice of lambda.
#- ' 
#- ' @method mytidy cva.glmnet
#- ' @export

mytidy_cva_glmnet <- function(x, extract = c("cva", "mod", "cva.summ", "mod.summ"), return_zeros = FALSE, ...){
  # estimate := cvm
  # 
  extract <- match.arg(extract)
  alpha   <- x$alpha   # vector
  n <- length(alpha)  
  modlist <- x$modlist
  dtlist <- vector("list", length = n)
   for (i in 1:n) {
       modi <- modlist[[i]]      # cv.glmnet object
       dti <- switch (extract,
                 cva  = tidy(modi),        # tidy cv.glmnet object
                 mod = tidy(modi$glmnet.fit, return_zeros = return_zeros),  ## tidy glmnet object
                 cva.summ = glance(modi),
                 mod.summ = glance(modi$glmnet.fit)
                 )
       dti$alpha  <- alpha[i] 
       if (extract == "cva") dti <- dti %>% mutate(step = 1:nrow(dti)) 
       dtlist[[i]] <- dti
     }
     ret <- dplyr::bind_rows(dtlist) %>% relocate (alpha) %>% 
         arrange(alpha)                    
 return(as_tibble(ret))
}
