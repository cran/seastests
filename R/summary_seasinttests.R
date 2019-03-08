#' Generic functions for class seasinttests
#' 
#' @param object result from seasonal integration test
#' @param ... additional arguments 
#' @author Daniel Ollech
#' @examples a <- qs(ts(rnorm(120, 10,10), frequency=12))
#' print(a)
#' summary(a)
#' @export


summary.seasinttests <- function(object, ...) {
  cat("Test used: ", object$test, "\n", "\n")
  cat("Test statistic: ", round(object$stat,2), "\n")
  cat("P-value: ", object$Pval, "\n", "\n")
  if (object$Pval <= 0.05) {cat("The", "test rejects the null hypothesis of a seasonal unit root on a 5-percent-level")}
  if (object$Pval > 0.05) {cat("The", "test does not reject the null hypothesis of a seasonal unit root on a 5-percent-level")}
  cat("\n")
}


