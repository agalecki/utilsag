% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cva.penAFTFormula.R
\name{penAFT.cva.default}
\alias{penAFT.cva.default}
\title{Elastic net cross-validation of alpha and lambda for penAFT model}
\usage{
\method{penAFT.cva}{default}(
  x,
  logY,
  delta,
  alpha = seq(0, 1, len = 11)^3,
  nfolds = 10,
  seed = NULL,
  ...,
  outerParallel = NULL,
  checkInnerParallel = TRUE
)
}
\arguments{
\item{x}{A matrix of predictor variables; or for the plotting methods, an object returned by \code{penAFT.cva}.}

\item{logY}{A response vector ( with n-elements) of log-survival or log-censoring times.}

\item{delta}{A binary vector indicating whether the jth element of logY is an observed log-survival time (d_j = 1) or a log-censoring time (d_j = 0) for j=1,..., n.}

\item{alpha}{A vector of alpha values for which to do cross-validation. The default is a sequence of 11 values more closely spaced around alpha = 0. For the \code{predict} and \code{coef} methods, the specific value of alpha for which to return predictions/regression coefficients.}

\item{nfolds}{The number of cross-validation folds to use. Defaults to 10.}

\item{seed}{Seed value. It is recommended to  provide seed value. As a result the same folds are used for every value of vector \code{alpha}. Defualts to NULL.}

\item{outerParallel}{Method of parallelising the outer loop over alpha. See 'Details' below. If \code{NULL}, the loop is run sequentially.}

\item{checkInnerParallel}{If the outer loop is run in parallel, check that the inner loop over lambda will not be in contention for cores.}
}
\description{
Do elastic net cross-validation of alpha and lambda simultaneously for penAFT models by calling penAFT::penAFT.cv() function
}
\details{
The \code{penAFT.cva} function does simultaneous cross-validation for both the alpha and lambda parameters in an elastic net model. The procedure is as outlined in the documentation for \link[glmnet:cv.glmnet]{glmnet::cv.glmnet}: it creates a vector \code{foldid} allocating the observations into folds, and then calls \code{penAFT.cv} in a loop over different values of alpha, but the same values of \code{foldid} each time.

Optionally this loop over alpha can be parallelised; currently, \code{penAFT.cva} knows about two methods of doing so:
\itemize{
\item Via \link{parLapply} in the parallel package. To use this, set \code{outerParallel} to a valid cluster object created by \link{makeCluster}.
\item Via \code{rxExec} as supplied by Microsoft R Server's RevoScaleR package. To use this, set \code{outerParallel} to a valid compute context created by \code{RxComputeContext}, or a character string specifying such a context.
}

If the outer loop is run in parallel, \code{penAFT.cva} can check if the inner loop (over lambda) is also set to run in parallel, and disable this if it would lead to contention for cores. This is done if it is likely that the parallelisation is local on a multicore machine, ie if \code{outerParallel} is a \code{SOCKcluster} object running on \code{"localhost"}, or if the RevoScaleR compute context is local parallel.
}
\examples{
\dontrun{
dtc <- proteins21
X  <- as.matrix(dtc[, 3:26])
logY  <- log(dtc[, "time"]) 
delta <- dtc[, "status"]   # status=0 indicates censored observation 
fit.en.cva <- penAFT.cva(x=X, logY = logY, delta = delta,
              alpha = seq(0, 1, len = 11)^3,
              nlambda = 50, lambda.ratio.min = 0.1, lambda = NULL,
              penalty = "EN", nfolds = 5, seed = 1234)
}
}
