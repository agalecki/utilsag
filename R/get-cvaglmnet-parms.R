#' optimal cva (hyper)parameters
#'
#' Extract the optimal (hyper)parameters from `cva.glmnet` object
#'
#' @export

get_cvaglmnet_params <- function(fit) {
  alpha <- fit$alpha
  lambdaMin <- sapply(fit$modlist, `[[`, "lambda.min")
  lambdaSE <- sapply(fit$modlist, `[[`, "lambda.1se")
  error    <- sapply(fit$modlist, function(mod) {min(mod$cvm)})
  best   <- which.min(error)
  data.frame(alpha_idx = best, alpha= alpha[best], lambdaMin = lambdaMin[best],
             lambdaSE = lambdaSE[best], eror = error[best])
}
