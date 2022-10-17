#' mytidyline1
#'
#' mytidyline2
#'
#' @name mytidy 
#' @rdname mytidy 
#' @export
mytidy <- function(x, ...) {
UseMethod("mytidy")
}

#' Elastic net cross-validation of alpha and lambda for penAFT model 
#' 
#' Do elastic net cross-validation of alpha and lambda simultaneously for penAFT models by calling penAFT::penAFT.cv() function
#'
#' @name penAFT.cva
#' @rdname penAFT.cva
#' @export
penAFT.cva <- function(x, ...)
UseMethod("penAFT.cva")

