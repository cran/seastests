#' Testing the seasonality of series
#' 
#' Using a user-chosen seasonality test, the seasonality of a time series is assessed and a boolean value is returned.
#' @param x time series
#' @param test Test to be used
#' @param freq Frequency of the time series
#' @details By default, the combined-test is used to assess the seasonality of a time series and returns a boolean. Alternatively, the QS test (test='qs'), Friedman test (test='fried'), Kruskall-Wallis (test='kw'), F-test on seasonal dummies (test='seasdum') or the Welch test (test='welch') can be used.
#' @references Webel, K. and Ollech, D. (2019). An overall seasonality test. Deutsche Bundesbank's Discussion Paper series.
#' @author Daniel Ollech
#' @examples isSeasonal(ts(rnorm(120, 10,10), frequency=12))
#' isSeasonal(ts(rnorm(1200, 10,10), frequency=7))
#' @export


isSeasonal <- function(x, test="combined", freq=NA) {
  if (tolower(test)=="combined" | tolower(test)=="combined_test" | tolower(test)=="wo" | tolower(test)=="ow") {statistic <- combined_test(x, freq=freq); seasonal=ifelse(statistic$stat==TRUE, TRUE, FALSE)}
  if (tolower(test)=="qs") {statistic <- qs(x, freq=freq, residuals=T, diff=F); seasonal=ifelse(statistic$Pval < 0.01, TRUE, FALSE)}
  if (tolower(test)=="fried") {statistic <- fried(x, freq=freq, residuals=T, diff=F); seasonal=ifelse(statistic$Pval < 0.01, TRUE, FALSE)}
  if (tolower(test)=="kw") {statistic <- kw(x, freq=freq, residuals=T, diff=F); seasonal=ifelse(statistic$Pval < 0.01, TRUE, FALSE)}
  if (tolower(test)=="seasdum") {statistic <- seasdum(x, freq=freq); seasonal=ifelse(statistic$Pval < 0.01, TRUE, FALSE)}
  if (tolower(test)=="welch") {statistic <- welch(x, freq=freq, residuals=T, diff=F); seasonal=ifelse(statistic$Pval < 0.01, TRUE, FALSE)}
  
  return(seasonal)
}

