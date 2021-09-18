#' Internal functions
#' 
#' Get lag
#' @param x time series
#' @param k number of lags
#' Functions used internally in the seasonality package
#' @author Daniel Ollech
#' @export

.Lag <- function(x,k) {
  y<- c(rep(NA,k), x)[1:length(x)] 
  class(y) <- "ts"
  y <- stats::ts(y, start=stats::start(x), frequency=stats::frequency(x))
  return(y)
}

#' Internal functions
#' 
#' Get differenced series
#' @param x time series
#' @param lag which lag
#' @param ... further parameters given to diff()
#' Functions used internally in the seasonality package
#' @author Daniel Ollech
#' @export

.Diff <- function(x,lag=1, ...) {
  y <- c(rep(NA, lag) , diff(x,lag=lag, ...))
  class(y) <- "ts"
  y <- stats::ts(y, start=stats::start(x), frequency=stats::frequency(x))
  return(y)
}