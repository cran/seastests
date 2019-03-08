#' Internatl functions
#' 
#' Functions used internally in the seasonality package
#' @author Daniel Ollech
#' @export

.Lag <- function(x,k) {
  y<- c(rep(NA,k), x)[1:length(x)] 
  class(y) <- "ts"
  y <- stats::ts(y, start=stats::start(x), frequency=stats::frequency(x))
  return(y)
}

.Diff <- function(x,lag=1, ...) {
  y <- c(rep(NA, lag) , diff(x,lag=lag, ...))
  class(y) <- "ts"
  y <- stats::ts(y, start=stats::start(x), frequency=stats::frequency(x))
  return(y)
}