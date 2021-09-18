#' Ollech and Webel's combined seasonality test
#' 
#' Ollech-Webel overall seasonality test that combines results from different seasonality tests.
#' @param y time series
#' @param freq Frequency of the time series
#' @references Ollech, D. and Webel, K. (forthcoming). An overall seasonality test. Deutsche Bundesbank's Discussion Paper series.
#' @references Ollech, D. and Webel, K. (2020). A random forest-based approach to identifying the most informative seasonality tests. Deutsche Bundesbank's Discussion Paper series 55/2020.
#' @details By default, the WO-test combines the results of the QS-test and the kw-test, both calculated on the residuals of an automatic non-seasonal ARIMA model. If the p-value of the QS-test is below 0.01 or the p-value of the kw-test is below 0.002, the WO-test will classify the corresponding time series as seasonal.
#' @details If residuals=FALSE the autoarima settings are ignored.
#' @details If residuals=TRUE, a non-seasonal ARIMA model is estimated for the time series. And the residuals of the fitted model are used as input to the test statistic. If an automatic order selection is used, the Hyndman-Khandakar algorithm is employed with max(p)=max(q) <= 3.
#' @author Daniel Ollech
#' @examples combined_test(ts(rnorm(120, 10,10), frequency=12))
#' combined_test(ts(rnorm(120, 10,10), frequency=7))
#' @export


combined_test <- function(y, freq=NA) {
  # Frequency of time series
  if(is.na(freq)) {
    if (any(class(y)=="ts")) {freq <- stats::frequency(y)} else {
      if (any(class(y)=="xts")) {freq <- freq_xts(y)} else {
        stop("Do not know the frequency of the time series.")
      }}}
  if(freq<2){stop(paste("The number of observations per cycle (usually years) is", freq, "and thus too small."))}
  
   # Single Tests  
  qs_res <- qs(y, freq=freq, diff=T, residuals=F)
  qs2_res <- qs(y, freq=freq, diff=F, residuals=T)
  kw2_res <-kw(y, freq=freq, diff=F, residuals=T)
  
  
  # Test Statistic
  stat = FALSE
  if (qs2_res$Pval < 0.01 | kw2_res$Pval < 0.001) stat = TRUE
  if (qs_res$Pval < 0.01 & kw2_res$Pval < 0.01) stat = TRUE
  
  Pval = c(qs_res$Pval, qs2_res$Pval, kw2_res$Pval); names(Pval) <- c("QS p-value", "QS-R p-value","KW-R p-value")
  
  # Return
  out <- list(stat=stat, Pval=Pval, test="WO", model=NULL)
  class(out) <- "seastests"
  
  return(out)
}

wo <- function(...) {
  message("Function is deprecated and will be removed in future versions of the package")
  combined_test(...)
}