#' Generic function for class seastests 
#' 
#' @param object result from seasonality test 
#' @param ... additional arguments 
#' @author Daniel Ollech
#' @examples a <- qs(ts(rnorm(120, 10,10), frequency=12))
#' print(a)
#' summary(a)
#' @export

summary.seastests <- function(object, ...) {
  cat("Test used: ", object$test, "\n", "\n")
  cat("Test statistic: ", round(object$stat,2), "\n")
  cat("P-value: ", object$Pval, "\n", "\n")
  if (object$test=="WO") {
    if (object$stat == 1) {cat("The", object$test, "- test identifies seasonality")}
    if (object$stat == 0) {cat("The", object$test, "- test does not identify  seasonality")
  } 
    if (object$test!="WO") {
  if (object$Pval <= 0.01) {cat("The", object$test, "- test finds significant seasonality on a 1-percent-level")}
  if (object$Pval > 0.01) {cat("The", object$test, "- test does not find significant seasonality on  a 1-percent-level")}}
  cat("\n")}
}
