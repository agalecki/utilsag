#' Predicted values for censored:::_coxnet model
#'
#' Invokes `censored:::predict._coxnet()` to calculate different types of predicted values for #' _coxnet model
#'
#' @export
pred_censored_coxnet <- function(model_fit, new_data, 
                       time_var = time, status_var = status,
                       time_points = NULL){
  lin_pred <- predict(model_fit, new_data, type = "linear_pred", increasing = FALSE)
  time_pred <- predict(model_fit, new_data, type = "time") 
  tmp <- time_points
  condt <- !is.null(tmp) && is.numeric(tmp) && length(tmp) > 0
  survprob <- NULL
  if (condt) survprob <- predict(model_fit, new_data, type = "survival", time = tmp)
  tvar <- as.character(substitute(time_var))
  svar <- as.character(substitute(status_var))
  cts  <- c(tvar, svar)
  res <- new_data %>% select(all_of(cts)) %>% cbind(lin_pred, time_pred)
  if (condt) res <- cbind(res, survprob)
  as_tibble(res)
}
