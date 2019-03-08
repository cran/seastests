#' Generic function for class seastests 
#' 
#' @param x result from seasonality test
#' @param ... additional arguments 
#' @author Daniel Ollech
#' @examples a <- qs(ts(rnorm(120, 10,10), frequency=12))
#' print(a)
#' summary(a)
#' @export


print.seastests <- print.seasinttests <- function(x, ...) {
  cat("Test used: ", x$test, "\n", "\n")
  cat("Test statistic: ", round(x$stat,2), "\n")
  cat("P-value: ", x$Pval)
  cat("\n")
}


